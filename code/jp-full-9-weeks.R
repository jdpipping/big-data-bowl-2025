#############
### SETUP ###
#############

# .rs.restartR()
showConnections()
closeAllConnections()
gc(reset = TRUE)
Sys.setenv('R_MAX_VSIZE'=64000000000)

library(vctrs)
library(readxl)
library(tidyselect)
library(naniar)
library(tidyr)
# library(plyr) ... only use if needed, can get in the way of dplyr
library(nnet)
library(mlogit)
library(reactable)
library(plotly)
library(ggplot2)
library(survey)
library(rafalib)
library(modelr)
library(na.tools)
library(ggimage)
library(ggrepel)
library(ggforce)
library(remotes)
library(reshape2)
library(glue)
library(readr)
library(car)
library(caret)
library(rsample)      
library(ggthemes)
library(scales)
library(Metrics)
library(here)
library(gbm)
library(gt)
library(webshot)
library(webshot2)
library(ICEbox)
library(devtools)
# library(cfbfastR)
# library(cfbplotR)
library(nflfastR)
library(teamcolors)
library(cvTools)
library(splines)
# library(keras) ... this relates to Python
# library(tensorflow) ... this relates to Python
library(htmltools)
library(slider)
library(Rfast)
library(gganimate)
library(viridis)
library(deldir)
# library(ggvoronoi)
library(gifski)
library(randomForest)
library(ranger)
library(xgboost)
library(tidymodels)
library(usemodels)
library(textrecipes)
library(vip)
library(pROC)
library(patchwork)
library(gridExtra)
library(tictoc)
library(data.table)
library(nflreadr)
library(nflplotR)
library(nflverse)
library(tidyverse)
library(dplyr)
library(rsvg)
# library(conflicted)
library(progress)

options(digits = 4)
options(scipen = 999) 

######################
### IMPORT / CLEAN ###
######################

# progress bar for loop
pb = progress_bar$new(total = 9)
# for loop to run each tracking week one at a time
for (iter in 1:9) {
  # read in data
  games <- fread("raw-data/games.csv")
  players <- fread("raw-data/players.csv")
  plays <- fread("raw-data/plays.csv")
  player_play <- fread("raw-data/player_play.csv")
  # read in tracking
  tracking <- fread(paste0('raw-data/tracking_week_', iter, '.csv'))

# Get rid of extra columns right away
games <- games %>% select(-c("gameDate", "gameTimeEastern"))
players <- players %>% select(-c("birthDate", "collegeName", "height", "weight"))
plays <- plays %>% select(-c("yardlineSide", "yardlineNumber", "gameClock", "absoluteYardlineNumber", 
                             "playClockAtSnap", "passLength", "targetX", "targetY", "playAction", "dropbackDistance", "passLocationType",
                             "timeToThrow", "timeInTackleBox", "timeToSack", "passTippedAtLine", "unblockedPressure",
                             "qbSneak", "rushLocationType", "pff_runConceptPrimary", "pff_runConceptSecondary", "pff_runPassOption"))
player_play <- player_play %>% select(c("gameId", "playId", "nflId", "teamAbbr", "pff_defensiveCoverageAssignment"))

tracking_combined <- tracking
# table(tracking_combined$event) - note there's no event for missed tackle
# Tons of notable stuff here, like huddle_break_offense, huddle_Start_offense, man_in_motion, etc.

tracking_combined <- tracking_combined %>% select(-"time")

# # get rid of individual weekly files to save space
# rm(tracking_week_1, tracking_week_2, tracking_week_3,
#    tracking_week_4, tracking_week_5, tracking_week_6,
#    tracking_week_7, tracking_week_8, tracking_week_9)

# Before anything else, fix Robbie Chosen/Robby Anderson name error
# View(players %>% filter(nflId == 43808))
# View(tracking_combined %>% filter(nflId == 43808))
players <- players %>% mutate(displayName = 
                                ifelse(nflId == 43808, "Robby Anderson", displayName))
tracking_combined <- tracking_combined %>% mutate(displayName = 
                                                    ifelse(nflId == 43808, "Robby Anderson", displayName))

# Also manual fix of a random "huddle_start_offense" during a play in a Week 8 game
# View(tracking_combined %>% filter(gameId %in% 2022103003 & playId %in% 2394))
tracking_combined <- tracking_combined %>% 
  mutate(event = ifelse((gameId %in% 2022103003 & playId %in% 2394 & frameId %in% 120), NA, event))

# General data cleansing/checking for errors
sort(table(players$nflId)) # no duplicates
sort(table(players$displayName)) # no duplicates

# Convert down into an ordinal factor variable, rather than numeric
# I.e., 2nd down + 2nd down doesn't equal 4th down, the way 2 TD + 2 TD = 4 TD
class(plays$down)
plays <- plays %>% mutate(down = factor(down, ordered = TRUE, levels = c("1", "2", "3", "4")))
# Note that L, Q, C means linear, quadratic, cubic, etc.

table(player_play$hadDropback)
# This only refers to the individual player dropping back, NOT whether the play itself was a dropback
# E.G., for gameId 2022090800 and playId 56, only one player has a “1” value there, which is nflId 46076, aka Josh Allen
# But the plays data frame has an "isDropback" column

# These plays (among others) have multiple "first_contact" events, but we want only first instance
# View(tracking_combined %>% filter(gameId == 2022100209, playId == 1581, event == "first_contact"))
# View(tracking_combined %>% filter(gameId == 2022103100, playId == 1689, event == "first_contact"))
# View(tracking_combined %>% filter(gameId == 2022103004, playId == 2106, event == "first_contact"))
FirstContact_Events <- tracking_combined %>% filter(event == "first_contact") %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(FirstContact_rank = rank(frameId, ties.method = "first")) %>%
  ungroup()
FirstContact_Events <- FirstContact_Events %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId", "FirstContact_rank"))
MultiFirstContact_Plays <- FirstContact_Events %>% 
  group_by(gameId, playId) %>% 
  summarize(n = n(), Frames = n_distinct(frameId)) %>% arrange(desc(Frames))
tracking_combined <- tracking_combined %>%
  left_join(FirstContact_Events, by = c("gameId", "playId", "nflId", "displayName", "frameId"))

table(tracking_combined$event)
# For any "FirstContact_rank" bigger than 1, change the event name to NA
tracking_combined <- tracking_combined %>% mutate(event =
                                                    ifelse(is.na(FirstContact_rank), event,
                                                           ifelse(FirstContact_rank > 1 & event == "first_contact", NA, event)))

rm(FirstContact_Events, MultiFirstContact_Plays)
tracking_combined <- tracking_combined %>% select(-"FirstContact_rank")

# Do the same thing with ball_snap, except this time we only want the last instance
# But first, do a quick confirmation that there are no plays with zero events for ball being snapped
tracking_combined <- tracking_combined %>% mutate(BallSnap_OnFrame = 
                                                    ifelse(!is.na(event) & event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap"), 1, 
                                                           ifelse(!is.na(event) & !event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap"), 0, NA)))
tracking_combined <- tracking_combined %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(BallSnap_OnFullPlay = sum(BallSnap_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(tracking_combined %>% filter(is.na(BallSnap_OnFullPlay))) - it's empty
table(tracking_combined$BallSnap_OnFullPlay)
# View(tracking_combined %>% filter(BallSnap_OnFullPlay != 1)) - it's empty
tracking_combined <- tracking_combined %>% select(-"BallSnap_OnFullPlay")

BallSnap_Events <- tracking_combined %>% filter(event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(BallSnap_rank = rank(-frameId, ties.method = "first")) %>%
  ungroup()
BallSnap_Events <- BallSnap_Events %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId", "BallSnap_rank"))
MultiBallSnap_Plays <- BallSnap_Events %>% 
  group_by(gameId, playId) %>% 
  summarize(n = n(), Frames = n_distinct(frameId)) %>% arrange(desc(Frames))
tracking_combined <- tracking_combined %>%
  left_join(BallSnap_Events, by = c("gameId", "playId", "nflId", "displayName", "frameId"))

table(tracking_combined$event)
# For any "BallSnap_rank" bigger than 1, change the event name to NA
tracking_combined <- tracking_combined %>% mutate(event =
                                                    ifelse(is.na(BallSnap_rank), event,
                                                           ifelse(BallSnap_rank > 1 & event == "first_contact", NA, event)))

# Let's also make sure no plays have more than 23 "players" during ball snap (i.e. one snap event per player, and the ball)
BallSnap_Multiples <- BallSnap_Events %>%
  group_by(gameId, playId) %>%
  summarize(n = n()) %>% arrange(desc(n))
# If any have less than 23, get rid of those (10 men on the field leads to iffy data)
BallSnap_All22_OnField <- BallSnap_Multiples %>% filter(n == 23)
tracking_combined <- merge(x = tracking_combined, y = BallSnap_All22_OnField,
                           by = c("gameId", "playId"))
tracking_combined <- tracking_combined %>% select(-"n")

rm(BallSnap_Events, MultiBallSnap_Plays, BallSnap_Multiples, BallSnap_All22_OnField)
tracking_combined <- tracking_combined %>% select(-"BallSnap_rank")

# Note that o means orientation, and dir means direction, both scaled 0-360
# In both cases, 0 is facing the visitor sideline (i.e. where Y = 53.3)
# Thus, both 0 and 180 are parallel to the LOS when discussing o and dir
# Similarly, X = 120 at back of visitor end zone (X = 0 at back of home EZ)
# See this Kaggle thread: https://www.kaggle.com/competitions/nfl-big-data-bowl-2024/discussion/454380
# "Home EZ" is simply always the one to the left of the home team's sideline, regardless of which EZ that team is trying to score on in that moment

# And playDirection refers to which end zone the offense is facing, NOT the actual play direction
# E.G. a rush outside the left tackle doesn't necessarily have playDirection == left

# Code that will eventually help w/ gap classification and pre-snap alignment
# To start, standardize so that the highest "y" is always to offense's left
# I.e., make it so that it doesn't matter which end zone the offense is aiming at
tracking_combined <- tracking_combined %>%
  mutate(y = ifelse(playDirection == "right", y, (53.3 - y)))

# Likewise, adjust "o" and "dir" in same manner, make 0 always be offense's left
# So 90 is always toward the EZ offense is aiming at, 180 is to offense's right, etc.
tracking_combined <- tracking_combined %>%
  mutate(o = ifelse(playDirection == "right", o, 
                    ifelse((playDirection == "left" & o < 180), o + 180, o - 180)))
tracking_combined <- tracking_combined %>%
  mutate(dir = ifelse(playDirection == "right", dir, 
                      ifelse((playDirection == "left" & dir < 180), dir + 180, dir - 180)))

# Same adjustment for "x" - make high "x" always be where offense is aiming at
tracking_combined <- tracking_combined %>%
  mutate(x = ifelse(playDirection == "right", x, (120 - x)))

# Note that the first frame is typically, but not always, huddle_break_offense: View(tracking_combined[1:5000, ])
FirstFrame_NotHuddleBreak <- tracking_combined %>% filter(frameId == 1 & !event %in% "huddle_break_offense")
# Just glance at a few samples ... They often start with NA, then line_set comes a few frames later
# View(tracking_combined %>% filter(gameId == 2022091200 & playId == 741 | gameId == 2022091102 & playId == 322 | gameId == 2022091101 & playId == 1785 |  gameId == 2022090800 & playId == 3190 | gameId == 2022091811 & playId == 2348 | gameId == 2022091805 & playId == 79))
# But sometimes first frame is line_set, or huddle_start_offense, or ball_snap, or man_in_motion
table(FirstFrame_NotHuddleBreak$event)

# View(tracking_combined %>% filter(is.na(frameId))) - this is empty, as it should be 

# First, find out if the minimum frameId on any given play is always 1, or if there are data entry errors where a play starts w/ higher frameId
OpeningFrame_EachPlay <- tracking_combined %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(frameId, ties.method = "first")) %>%
  ungroup() 
OpeningFrame_EachPlay <- OpeningFrame_EachPlay %>% filter(Frame_Rank == 1)
OpeningFrame_EachPlay <- OpeningFrame_EachPlay %>% select(-"Frame_Rank")
table(OpeningFrame_EachPlay$frameId) # it's always 1, so there are no "late frameId" errors

# Now that we know this, use group_by() to mutate tracking_combined so all frames are labelled with the event from that play's first frame
# In other words, did this play's tracking data begin w/ huddle_break_offense, or ball_snap, or something else?
OpeningFrame_Event <- OpeningFrame_EachPlay %>% select("playId", "gameId", "nflId", "displayName", "event")
OpeningFrame_Event <- OpeningFrame_Event %>% rename(Frame1_Event = `event`)

tracking_combined <- merge(x = tracking_combined, y = OpeningFrame_Event, 
                           by = c("playId", "gameId", "nflId", "displayName"))
tracking_combined <- tracking_combined %>% arrange(gameId, playId, nflId, frameId)
table(tracking_combined$Frame1_Event)
rm(FirstFrame_NotHuddleBreak, OpeningFrame_EachPlay, OpeningFrame_Event)

# Let's diagnose whether a play included the event for huddle_break_offense
# This will also allow us to see if any play erroneously includes multiple "huddle_break_offense" events
tracking_combined <- tracking_combined %>% mutate(HuddleBreak_OnFrame = 
                                                    ifelse(!is.na(event) & event %in% "huddle_break_offense", 1, 
                                                           ifelse(!is.na(event) & !event %in% "huddle_break_offense", 0, NA)))

tracking_combined <- tracking_combined %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(HuddleBreak_OnFullPlay = sum(HuddleBreak_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(tracking_combined %>% filter(is.na(HuddleBreak_OnFullPlay))) - it's empty
table(tracking_combined$HuddleBreak_OnFullPlay)
# View(tracking_combined %>% filter(HuddleBreak_OnFullPlay > 1))
# Some examples: gameId 2022091107, playId 959 ... gameId 2022092500, playId 906

# For any play w/ multiple "huddle_break_offense" events, get rid of any frames before the most recent such event
HuddleBreak_DF <- tracking_combined %>%
  filter(event %in% "huddle_break_offense") %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_HuddleBreak = frameId)

# Account for plays that could have multiple of these events ... we want to keep only the most recent
HuddleBreak_DF <- HuddleBreak_DF %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(-FrameNumber_HuddleBreak, ties.method = "first")) %>%
  ungroup() 
HuddleBreak_DF <- HuddleBreak_DF %>% filter(Frame_Rank == 1)
HuddleBreak_DF <- HuddleBreak_DF %>% select(-"Frame_Rank")

# Do a quick confirmation that there are no plays left with more than one
# None of these should have more than 23 (i.e. one huddle break event per player, and the ball)
HuddleBreak_Multiples <- HuddleBreak_DF %>%
  group_by(gameId, playId) %>%
  summarize(n = n()) %>% arrange(desc(n))
rm(HuddleBreak_Multiples)

tracking_combined <- merge(x = tracking_combined, y = HuddleBreak_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_combined <- tracking_combined %>% arrange(gameId, playId, nflId, frameId)

tracking_combined <- tracking_combined %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_HuddleBreak) & frameId < FrameNumber_HuddleBreak & HuddleBreak_OnFullPlay > 1, TRUE, FALSE)) %>% 
  ungroup()

tracking_combined <- tracking_combined %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(HuddleBreak_DF)
tracking_combined <- tracking_combined %>% select(-c("Unnecessary_Early", "FrameNumber_HuddleBreak"))

# Repeat that process for huddle_start_offense
tracking_combined <- tracking_combined %>% mutate(HuddleStart_OnFrame = 
                                                    ifelse(!is.na(event) & event %in% "huddle_start_offense", 1, 
                                                           ifelse(!is.na(event) & !event %in% "huddle_start_offense", 0, NA)))

tracking_combined <- tracking_combined %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(HuddleStart_OnFullPlay = sum(HuddleStart_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(tracking_combined %>% filter(is.na(HuddleStart_OnFullPlay))) - it's empty
table(tracking_combined$HuddleStart_OnFullPlay)
# View(tracking_combined %>% filter(HuddleStart_OnFullPlay > 1))

# For any play w/ multiple "huddle_start_offense" events, get rid of any frames before the most recent such event
HuddleStart_DF <- tracking_combined %>%
  filter(event %in% "huddle_start_offense") %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_HuddleStart = frameId)

# Account for plays that could have multiple of these events ... we want to keep only the most recent
HuddleStart_DF <- HuddleStart_DF %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(-FrameNumber_HuddleStart, ties.method = "first")) %>%
  ungroup() 
HuddleStart_DF <- HuddleStart_DF %>% filter(Frame_Rank == 1)
HuddleStart_DF <- HuddleStart_DF %>% select(-"Frame_Rank")

# Do a quick confirmation that there are no plays left with more than one
# None of these should have more than 23 (i.e. one huddle start event per player, and the ball)
HuddleStart_Multiples <- HuddleStart_DF %>%
  group_by(gameId, playId) %>%
  summarize(n = n()) %>% arrange(desc(n))
rm(HuddleStart_Multiples)

tracking_combined <- merge(x = tracking_combined, y = HuddleStart_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_combined <- tracking_combined %>% arrange(gameId, playId, nflId, frameId)

tracking_combined <- tracking_combined %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_HuddleStart) & frameId < FrameNumber_HuddleStart & HuddleStart_OnFullPlay > 1, TRUE, FALSE)) %>% 
  ungroup()

tracking_combined <- tracking_combined %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(HuddleStart_DF)
tracking_combined <- tracking_combined %>% select(-c("Unnecessary_Early", "FrameNumber_HuddleStart"))

# Now see if there are any plays that have huddle_start_offense but not huddle_break_offense (should be impossible)
# View(tracking_combined %>% filter(HuddleStart_OnFullPlay > 0 & HuddleBreak_OnFullPlay == 0))
# Turns out this is reasonably common ... best route is probably to get rid of any frames before huddle_start_offense, see below
# View(tracking_combined %>% filter(HuddleStart_OnFullPlay > 0 & HuddleBreak_OnFullPlay > 0))
# IN CONTRAST, THIS ONE IS EMPTY ... no plays have an event for huddle starting AND huddle breaking

# Here's how we would get rid of unnecessary frames coming BEFORE offense broke the huddle
# In other words, doing it for all plays with a huddle_break_offense event, NOT just the plays that had multiple
# Isolate the plays that include an event for huddle_break_offense
Tracking_PlaysWithHuddleBreak <- tracking_combined %>% filter(HuddleBreak_OnFullPlay == 1)

# Get rid of any frames that came before the huddle_break_offense event, IF THE PLAY HAD ONE
HuddleBreak_DF <- tracking_combined %>%
  filter(event %in% c("huddle_break_offense")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_HuddleBreak = frameId)

tracking_combined <- merge(x = tracking_combined, y = HuddleBreak_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_combined <- tracking_combined %>% arrange(gameId, playId, nflId, frameId)

tracking_combined <- tracking_combined %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_HuddleBreak) & frameId < FrameNumber_HuddleBreak, TRUE, FALSE)) %>% 
  ungroup()

tracking_combined <- tracking_combined %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(HuddleBreak_DF)
tracking_combined <- tracking_combined %>% select(-c("Unnecessary_Early", "FrameNumber_HuddleBreak"))

# Glance to see if there's ever a huddle_start_offense event that isn't on the first frame ... there is
# View(tracking_combined %>% filter(event %in% "huddle_start_offense" & frameId != 1))
# View(tracking_combined %>% filter(gameId == 2022092505, playId == 2919))
# Therefore, can repeat the same process that we used with huddle_break_offense
Tracking_PlaysWithHuddleStart <- tracking_combined %>% filter(HuddleStart_OnFullPlay == 1)

# Get rid of any frames that came before the huddle_start_offense event, IF THE PLAY HAD ONE
HuddleStart_DF <- tracking_combined %>%
  filter(event %in% c("huddle_start_offense")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_HuddleStart = frameId)

tracking_combined <- merge(x = tracking_combined, y = HuddleStart_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_combined <- tracking_combined %>% arrange(gameId, playId, nflId, frameId)

tracking_combined <- tracking_combined %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_HuddleStart) & frameId < FrameNumber_HuddleStart, TRUE, FALSE)) %>% 
  ungroup()

tracking_combined <- tracking_combined %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(HuddleStart_DF)
tracking_combined <- tracking_combined %>% select(-c("Unnecessary_Early", "FrameNumber_HuddleStart"))

Tracking_PlaysWithHuddle_StartOrBreak <- tracking_combined %>% filter(HuddleStart_OnFullPlay == 1 | HuddleBreak_OnFullPlay == 1)

# Also mutate a variable for whether a play had a "line_set" event
tracking_combined <- tracking_combined %>% mutate(LineSet_OnFrame = 
                                                    ifelse(!is.na(event) & event %in% "line_set", 1, 
                                                           ifelse(!is.na(event) & !event %in% "line_set", 0, NA)))

tracking_combined <- tracking_combined %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(LineSet_OnFullPlay = sum(LineSet_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(tracking_combined %>% filter(is.na(LineSet_OnFullPlay))) - it's empty
table(tracking_combined$LineSet_OnFullPlay)
# OpeningFrames <- tracking_combined %>% filter(frameId == 1)
# table(OpeningFrames$LineSet_OnFullPlay) ... over the full 9 weeks, 367494 / 370852 plays have a line_set, or 99.1%

# For any play w/ multiple "line_set" events, get rid of any frames before the most recent such event
LineSet_DF <- tracking_combined %>%
  filter(event %in% "line_set") %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_LineSet = frameId)

# Account for plays that could have multiple of these events ... we want to keep only the most recent
LineSet_DF <- LineSet_DF %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(-FrameNumber_LineSet, ties.method = "first")) %>%
  ungroup() 
LineSet_DF <- LineSet_DF %>% filter(Frame_Rank == 1)
LineSet_DF <- LineSet_DF %>% select(-"Frame_Rank")

# Do a quick confirmation that there are no plays left with more than one
# None of these should have more than 23 (i.e. one line set event per player, and the ball)
LineSet_Multiples <- LineSet_DF %>%
  group_by(gameId, playId) %>%
  summarize(n = n()) %>% arrange(desc(n))
rm(LineSet_Multiples)

tracking_combined <- merge(x = tracking_combined, y = LineSet_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_combined <- tracking_combined %>% arrange(gameId, playId, nflId, frameId)

tracking_combined <- tracking_combined %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_LineSet) & frameId < FrameNumber_LineSet & LineSet_OnFullPlay > 1, TRUE, FALSE)) %>% 
  ungroup()

tracking_combined <- tracking_combined %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(LineSet_DF)
tracking_combined <- tracking_combined %>% select(-c("Unnecessary_Early", "FrameNumber_LineSet"))

# And now do the same thing for all plays, i.e. NOT just the ones with multiple line_set events
# Get rid of any frames that came before the line_set event, IF THE PLAY HAD ONE
LineSet_DF <- tracking_combined %>%
  filter(event %in% c("line_set")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_LineSet = frameId)

tracking_combined <- merge(x = tracking_combined, y = LineSet_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_combined <- tracking_combined %>% arrange(gameId, playId, nflId, frameId)

tracking_combined <- tracking_combined %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_LineSet) & frameId < FrameNumber_LineSet, TRUE, FALSE)) %>% 
  ungroup()

tracking_combined <- tracking_combined %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(LineSet_DF)
tracking_combined <- tracking_combined %>% select(-c("Unnecessary_Early", "FrameNumber_LineSet"))

# Here's what the plays look like with no line_set: View(tracking_combined %>% filter(LineSet_OnFullPlay == 0))
# Typically it's just huddle start or huddle break, then ball snap, maybe w/ motion in between

# Now get rid of all plays with no line_set
tracking_combined <- tracking_combined %>% filter(LineSet_OnFullPlay == 1)
tracking_combined <- tracking_combined %>% select(-"LineSet_OnFullPlay")

# Remove these unless there's a reason we need them
rm(Tracking_PlaysWithHuddleBreak, Tracking_PlaysWithHuddleStart, Tracking_PlaysWithHuddle_StartOrBreak)

# Then use rank() to retroactively fix frameId for all plays (i.e. make them start at 1)
# This might not even really be necessary, but here's how to do it
# Probably helpful for incorporating video/dots, i.e. having any play start at Frame 1
tracking_combined <- tracking_combined %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(frameId = rank(frameId, ties.method = "first")) %>%
  ungroup() 
table(tracking_combined$frameId)

# Now create line of scrimmage for each play using ball data
# Obviously, where the ball is during the ball_snap event is the LOS
Snap_Ball_Location <- tracking_combined %>%
  filter(club == "football", event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(Ball_X_Snap = x, Ball_Y_Snap = y, frameId_Snap = frameId)

tracking_combined <- tracking_combined %>%
  left_join(Snap_Ball_Location, by = c("playId", "gameId"))

tracking_combined <- tracking_combined %>%
  mutate(X_dist_FromBall_OrigLocation = x - Ball_X_Snap, Y_distFromMOF = y - 26.65,
         Y_dist_FromBall_OrigLocation = y - Ball_Y_Snap)

# Code for where the ball is at any given point, and each player's distance from it
ball_df <- tracking_combined %>% 
  filter(club == "football") %>% 
  select(gameId, playId, frameId, x, y) %>% 
  rename(ball_x = x,
         ball_y = y)

tracking_combined <- tracking_combined %>% 
  left_join(ball_df, by = c("playId", "gameId", "frameId"))

tracking_combined <- tracking_combined %>% 
  mutate(TotDistFromBall = sqrt((x - ball_x)^2 + (y - ball_y)^2),
         TotDistFromBall_OrigLocation = sqrt((x - Ball_X_Snap)^2 + (y - Ball_Y_Snap)^2),
         Y_DistFromBall = (y - ball_y), X_DistFromBall = (x - ball_x),
         Y_AbsDistFromBall = abs(y - ball_y), X_AbsDistFromBall = abs(x - ball_x))
rm(ball_df, Snap_Ball_Location)

# Likewise, add the ball's distance from goal line and sideline
tracking_combined <- tracking_combined %>%
  mutate(Ball_DistFromGoalLine = 110 - ball_x,
         Ball_DistFromSideline = ifelse(ball_y >= 26.65, 53.3 - ball_y, ball_y))

# And mutate a binary variable for being near goal line or sideline
tracking_combined <- tracking_combined %>% 
  mutate(BallNearGoalLine = ifelse(Ball_DistFromGoalLine <= 3, 1, 0),
         BallNearSideline = ifelse(Ball_DistFromSideline <= 3, 1, 0))

nflverse_pbp <- nflfastR::load_pbp(2022)
nflverse_pbp <- nflverse_pbp %>% filter(week %in% 1:9)
# This gives descriptions of NFLVerse columns: View(field_descriptions)

# View(nflverse_pbp %>% filter(run_location == "middle" & !is.na(run_gap)))
# This is empty, meaning all "middle" runs have NA for run_gap - adjust this
nflverse_pbp <- nflverse_pbp %>% mutate(
  run_gap = ifelse(run_location == "middle", "center", run_gap))

table(plays$passResult)
# C means complete, R means scramble, S means sack, IN means interception, I means incomplete
# All others are designed runs
plays <- plays %>% mutate(passResult = 
                            ifelse(passResult %in% c("C", "R", "IN", "I", "S"), passResult, NA))

# Arrange plays so they are sorted chronologically
plays <- plays %>% arrange(gameId, playId)

# Fix one column that is spelled wrong
plays <- plays %>% rename(visitorTeamWinProbabilityAdded = `visitorTeamWinProbilityAdded`)

# View(plays %>% filter(is.na(homeTeamWinProbabilityAdded))) - it's empty
# View(plays %>% filter(is.na(visitorTeamWinProbabilityAdded))) - it's empty

# View(plays %>% filter(is.na(expectedPointsAdded)))
# One play to fix here, a David Montgomery run on 10/09/22
# Since it's only one play, just find the right answer from nflverse
# View(nflverse_pbp %>% filter(old_game_id == 2022100904, qtr == 2))
plays <- plays %>% mutate(expectedPointsAdded =
                            ifelse(is.na(expectedPointsAdded), -0.29447450, expectedPointsAdded))  

# Get rid of all QB kneels and spikes
plays <- plays %>% filter(qbSpike == FALSE | is.na(qbSpike))
plays <- plays %>% filter(qbKneel == 0 | is.na(qbKneel))
plays <- plays %>% select(-c("qbSpike", "qbKneel"))

# And fix the Cover-6 Right vs. Cover 6-Left error
table(plays$pff_passCoverage)
plays <- plays %>% mutate(pff_passCoverage = 
                            ifelse(!is.na(pff_passCoverage) & pff_passCoverage %in% "Cover 6-Left", "Cover-6 Left", pff_passCoverage))
# View(plays %>% filter(is.na(pff_passCoverage))) ... none of these are dropbacks, and they're mostly QB sneaks

# Mutate whether post-snap safeties were middle-of-field-open (MOFO) or closed (MOFC)
plays <- plays %>% mutate(PostSnap_MOF =
                            ifelse(!is.na(pff_passCoverage) & pff_passCoverage %in% c("Cover-1", "Cover-1 Double", "Cover-3", "Cover-3 Cloud Left", "Cover-3 Cloud Right", "Cover-3 Double Cloud", "Cover-3 Seam"), "MOF Closed",
                                   ifelse(!is.na(pff_passCoverage) & pff_passCoverage %in% c("Cover-0", "Cover-2", "Quarters", "Cover-4", "2-Man", "Cover-6", "Cover-6 Left", "Cover-6 Right"), "MOF Open", 
                                          ifelse(!is.na(pff_passCoverage) & pff_passCoverage %in% c("Bracket", "Goal Line", "Miscellaneous", "Prevent", "Red Zone"), "Ambiguous", NA))))
table(plays$PostSnap_MOF)
# View(plays %>% filter(is.na(PostSnap_MOF)))

# Turn receiver alignment into numeric variable(s), rather than character
table(plays$receiverAlignment) # all have exactly three characters
# Glance at an example of 3x0 so we can diagnose if the numbers mean wide receivers, or eligible receivers
# View(plays %>% filter(receiverAlignment %in% "3x0")); clearly it means eligible receivers, i.e. TEs are included
# Use text analysis to extract from the formations; substr() syntax is substr(x, start, stop)
plays <- plays %>% 
  mutate(aligned_left_receivers = substr(receiverAlignment, 1, 1))
plays <- plays %>% 
  mutate(aligned_right_receivers = substr(receiverAlignment, 3, 3))
class(plays$aligned_left_receivers) <- "numeric"
class(plays$aligned_right_receivers) <- "numeric"
plays <- plays %>% mutate(aligned_total_receivers = aligned_left_receivers + aligned_right_receivers)
plays <- plays %>% relocate(aligned_left_receivers, aligned_right_receivers, aligned_total_receivers, .after = offenseFormation)

# For the sake of simplicity, let's get rid of weird plays with more than 5
# View(plays %>% filter(receiverAlignment %in% "4x2")) ... direct snap to RB can make weird numbers show up
plays <- plays %>% filter(aligned_total_receivers <= 5)

# Then, next obligation is to properly merge everything
# Must change the class of nflverse_pbp to make it compatible with the rest
class(nflverse_pbp) <- c("data.table", "data.frame")

# Also must coerce old_game_id in nflverse into a numeric variable
nflverse_pbp$old_game_id <- as.numeric(nflverse_pbp$old_game_id)

# And coerce down in nflverse to an ordered factor
nflverse_pbp <- nflverse_pbp %>% mutate(down = factor(down, ordered = TRUE, levels = c("1", "2", "3", "4")))

PlaysAndGames <- merge(x = games, y = plays, by = "gameId") 
PlaysAndGames_NFLVerse <- merge(x = PlaysAndGames, y = nflverse_pbp, 
                                by.x = c("gameId", "playId", "season", "week", "down"), 
                                by.y = c("old_game_id", "play_id",  "season", "week", "down")) 
rm(games, plays, nflverse_pbp, PlaysAndGames)

# Use all.y = TRUE, to make sure all tracking data is included
TrackingWithStats <- merge(x = player_play, y = tracking_combined,
                           by = c("gameId", "playId", "nflId"), all.y = TRUE)

TrackingWithStats_PlayerNames <- merge(x = TrackingWithStats, y = players,
                                       by = c("nflId"))
# View(TrackingWithStats_PlayerNames %>% filter(is.na(club)))
# If we wanted to keep club == "football", we could use all.x = TRUE here
rm(players, player_play, tracking_combined, TrackingWithStats)

# Check for name discrepancies besides Chosen/Robbie Anderson within this DF
NameDiscrepancies <- TrackingWithStats_PlayerNames %>% filter(displayName.x != displayName.y)
table(NameDiscrepancies$displayName.x)
table(NameDiscrepancies$displayName.y)
# Go with the Y one (i.e. players.csv originally)
TrackingWithStats_PlayerNames <- TrackingWithStats_PlayerNames %>% 
  mutate(displayName = ifelse(displayName.x != displayName.y, displayName.y, displayName.y))
# Get rid of extra columns once that's done
TrackingWithStats_PlayerNames <- TrackingWithStats_PlayerNames %>%
  select(-"displayName.x", -"displayName.y")
rm(NameDiscrepancies)
TrackingWithStats_PlayerNames <- TrackingWithStats_PlayerNames %>% relocate("displayName")

colnames(TrackingWithStats_PlayerNames)
colnames(PlaysAndGames_NFLVerse)
# Make a new version of PlaysAndGames_NFLVerse with fewer columns, to save memory space 
# Note that home_score and away_score in NFLVerse refer to final scores
# Whereas total_home_score and total_away_score are entering that specific play
# Also passLocationType refers to QB's location, pass_location refers to where ball was thrown
# Note that "time" includes game date for the tracking version, but just game clock for NFLVerse version
NFLVerse_Reduced <- PlaysAndGames_NFLVerse %>% 
  select(-c(3, 8:9, 15:19, 31:32, 39:41, 45, 47, 51:68, 70:83, 85:88, 97:105,
            107:125, 127:129, 131:160, 162, 164:185, 187, 192:197, 199:303, 307:309,
            313:318, 325:326, 330, 333:355, 360, 363, 374:379, 383, 385:402))
TrackingWithStats_PlayerNames <- TrackingWithStats_PlayerNames %>% select(-"Frame1_Event")

rm(PlaysAndGames_NFLVerse)
MergedData <- merge(x = NFLVerse_Reduced, y = TrackingWithStats_PlayerNames,
                    by = c("gameId", "playId"))
rm(TrackingWithStats_PlayerNames, NFLVerse_Reduced)

# Before anything, take out all plays besides dropbacks
MergedData <- MergedData %>% filter(isDropback == 1)

# Arrange it by game ID, play ID, player ID, and frame ID
# But turn it into a data table so that it takes up less memory space
# MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

# One last check for name discrepancies that slipped through
# E.G. Kenneth Walker III and Kenneth Walker duplicate for one ID (similar to Robbie Chosen/Anderson)
NameDiscrepancies <- MergedData %>% 
  group_by(nflId, displayName) %>%
  summarize(n = n())
NameRankings <- sort(table(NameDiscrepancies$displayName), decreasing = TRUE)
NameDiscrepancy_Table <- as.data.frame(NameRankings)
NameDiscrepancy_Table <- NameDiscrepancy_Table %>% filter(Freq > 1) 
# The three instances are Jonah Williams, Michael Carter, and David Long, all of whom are legit doubles
# Therefore, no need to make any adjustments
rm(NameDiscrepancies, NameDiscrepancy_Table, NameRankings)

# Add a WP success column
MergedData <- MergedData %>% 
  mutate(WPSuccess = ifelse(wpa > 0, 1, 0))
# Check for NAs: View(MergedData %>% filter(is.na(WPSuccess)))

# And do the same for defensive WPA (just negative offensive WPA)
MergedData <- MergedData %>% mutate(DefWPA = (-1) * wpa)

# Find other ways to filter the data, e.g. get rid of garbage time
# MergedData <- MergedData %>% filter(winProbability >= 0.05 & winProbability <= 0.95)
# Here's how to filter in a data table
MergedData <- MergedData[wp >= 0.05 & wp <= 0.95]

# Other possible modifications for this specific project:
MergedData <- MergedData[xpass <= 0.95]
MergedData <- MergedData[half_seconds_remaining >= 30]
# MergedData <- MergedData[half_seconds_remaining >= 45 | yardline_100 <= 50]

# Make sure we have both offensive and defensive players
table(MergedData$position)

# Make broader "PosGroup" label from there, based on official NFL/GSIS positions
MergedData <- MergedData %>% mutate(PosGroup = ifelse(position %in% c("C", "G", "T"), "OL",
                                                      ifelse(position %in% c("CB", "DB", "FS", "SS"), "DB",
                                                             ifelse(position %in% c("DE", "DT", "NT"), "DL",
                                                                    ifelse(position %in% c("FB", "RB"), "RB",
                                                                           ifelse(position %in% c("ILB", "LB", "MLB", "OLB"), "LB",
                                                                                  ifelse(position == "QB", "QB",
                                                                                         ifelse(position == "TE", "TE", "WR"))))))))
table(MergedData$PosGroup)

MergedData <- MergedData %>%
  mutate(PlayerSideOfBall = ifelse(((club == homeTeamAbbr) &
                                      (posteam == homeTeamAbbr)) |
                                     ((club == visitorTeamAbbr) &
                                        (posteam == visitorTeamAbbr)),
                                   "offense",
                                   "defense"))
# View(MergedData %>% filter(is.na(PlayerSideOfBall))) -- it's empty

# Get rid of some more columns not needed for this specific project (can add back later if desired)
# View(MergedData %>% filter(playId != order_sequence)) ... order_sequence is almost always the same as playId
colnames(MergedData)
MergedData <- MergedData %>% 
  select(-c("week", "teamAbbr", "qb_scramble", "Ball_DistFromGoalLine", "Ball_DistFromSideline", "BallNearGoalLine", "BallNearSideline"))
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)

# Add distance ranks for each side of ball
# E.G. who is closest to ball-carrier at any given point of the play
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId, PlayerSideOfBall) %>%
  mutate(Y_AbsDistFromBall_Rank_BySide = rank(Y_AbsDistFromBall, ties.method = "first"),
         Y_NetDistFromBall_Rank_BySide = rank(Y_DistFromBall, ties.method = "first")) %>%
  ungroup()

# And same idea for "overall" ranks (i.e., doesn't matter if player is on offense or defense)
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(Y_AbsDistFromBall_Rank_OVR = rank(Y_AbsDistFromBall, ties.method = "first"),
         Y_NetDistFromBall_Rank_OVR = rank(Y_DistFromBall, ties.method = "first")) %>%
  ungroup()

# Now we want to exclude "extra" frames, i.e. ones after the play ended
table(MergedData$event)
# QB slide example: View(MergedData %>% filter(playId == 4789))
# Include fumbles, b/c anything after a defensive player recovers a fumble isn't relative to this project's analysis
# Similarly, we can include "dropped_pass", b/c even if a dropped pass got intercepted later, that's not relevant for this project
Frames_EndOfPlay <- MergedData %>%
  filter(event %in% c("play_submit", "out_of_bounds", "safety", "qb_sack", "qb_slide", "pass_outcome_incomplete", "dropped_pass", "pass_outcome_touchdown", "tackle", "touchdown", "fumble", "fumble_defense_recovered")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_EndOfPlay = frameId)

# Do a quick check to see if there are any plays with no "play-ending" events
MergedData <- MergedData %>% mutate(PlayEnd_OnFrame = 
                                      ifelse(!is.na(event) & event %in% c("play_submit", "out_of_bounds", "safety", "qb_sack", "qb_slide", "pass_outcome_incomplete", "dropped_pass", "pass_outcome_touchdown", "tackle", "touchdown", "fumble", "fumble_defense_recovered"), 1, 
                                             ifelse(!is.na(event) & !event %in% c("play_submit", "out_of_bounds", "safety", "qb_sack", "qb_slide", "pass_outcome_incomplete", "dropped_pass", "pass_outcome_touchdown", "tackle", "touchdown", "fumble", "fumble_defense_recovered"), 0, NA)))
MergedData <- MergedData %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(PlayEnd_OnFullPlay = sum(PlayEnd_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(MergedData %>% filter(is.na(PlayEnd_OnFullPlay))) - it's empty
table(MergedData$PlayEnd_OnFullPlay)
# View(MergedData %>% filter(PlayEnd_OnFullPlay < 1)) ... should be empty
MergedData <- MergedData %>% select(-"PlayEnd_OnFullPlay")

# Some plays have multiple of these "play-ending" events
# Therefore, make it so that only the first relevant "play-ending" frame shows up
# But if project theme was different, e.g. based on tackling, we might want the last "play-ending" frame instead
Frames_EndOfPlay <- Frames_EndOfPlay %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(FrameNumber_EndOfPlay, ties.method = "first")) %>%
  ungroup() 
Frames_EndOfPlay <- Frames_EndOfPlay %>% filter(Frame_Rank == 1)
Frames_EndOfPlay <- Frames_EndOfPlay %>% select(-"Frame_Rank")

MergedData <- merge(x = MergedData, y = Frames_EndOfPlay, 
                    by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
# MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

MergedData <- MergedData %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Late = ifelse(!is.na(FrameNumber_EndOfPlay) & frameId > FrameNumber_EndOfPlay, TRUE, 
                                   ifelse(!is.na(FrameNumber_EndOfPlay) & frameId <= FrameNumber_EndOfPlay, FALSE, NA))) %>% 
  ungroup()

MergedData <- MergedData %>% filter(Unnecessary_Late == FALSE | is.na(Unnecessary_Late))
rm(Frames_EndOfPlay)
MergedData <- MergedData %>% select(-"Unnecessary_Late", "PlayEnd_OnFrame")

# Write a function for calculating distance
calc_distance <- function(x, y, x_baseline = 0, y_baseline = 0) {
  sqrt((x-x_baseline)^2 + (y - y_baseline)^2)
}

########################
### DATA ENGINEERING ###
########################

# Let's start with adding each player's "x" and "y" pre-snap rank for each side of ball (e.g. who is closest to opposing end zone)
# Same concept as previously established Y_NetDistFromBall_Rank_BySide, but this refers to only at the time of the snap
MergedData_AtSnap <- MergedData %>%
  group_by(gameId, playId, PlayerSideOfBall) %>%
  filter(event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) %>% 
  mutate(X_PreSnap_Rank_BySide = rank(-x, ties.method = "first"),
         Y_PreSnap_Rank_BySide = rank(-y, ties.method = "first")) %>%
  ungroup() %>%
  select(gameId, playId, nflId, displayName, PlayerSideOfBall, X_PreSnap_Rank_BySide, Y_PreSnap_Rank_BySide, 
         PreSnap_x = x, PreSnap_y = y)
# table(FirstFrameOfPlay_DesignedRuns$X_PreSnap_Rank_BySide); rank is never higher than 11

# Note that the highest "x" rank pre-snap is NOT always the center
# View(MergedData_AtSnap %>% filter(X_PreSnap_Rank_BySide == 1, PlayerSideOfBall == "offense"))

MergedData <- MergedData %>%
  merge(MergedData_AtSnap, by = c("gameId", "playId", "nflId", "displayName", "PlayerSideOfBall"))
MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
rm(MergedData_AtSnap)

# Now, create a variable for whether a player had safety responsibilities on the play
# Even if that person wasn't aligned as a safety before the snap
MergedData <- MergedData %>% mutate(is_post_snap_safety =
                                      ifelse(!is.na(pff_defensiveCoverageAssignment) & pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), TRUE,
                                             ifelse(!is.na(pff_defensiveCoverageAssignment) & !pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), FALSE, 
                                                    ifelse(is.na(pff_defensiveCoverageAssignment) & PlayerSideOfBall %in% "defense", FALSE, NA))))

# Also mutate variables for the #1 receivers for the offense (i.e. the furthest outside) at the time of snap ... or line set?
# Recall that Y_NetDistFromBall_Rank_BySide exists already, so whoever ranks 1 and 11 there are the closest to sideline
# And we established that going to offense's left is always positive Y, but the rank goes by SMALLEST distance
# In other words, the person with rank 1 (on offense) is the RIGHT WR, b/c that'll have the smallest/most negative distance
# Whereas if we were using Y_AbsDistFromBall_Rank_BySide, then the person with rank 1 would be closest to the ball, i.e. the center
# Likewise, on defense, the person with rank 1 would be the defense's left CB (i.e. offense's right)
# The reason we don't use X_PreSnap_Rank_BySide here is that we want to use the "event" so we get each person's actual X and Y values at the snap
LeftMost_Receivers <- MergedData %>% 
  filter(PlayerSideOfBall == "offense", Y_NetDistFromBall_Rank_BySide == 11, event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) 
LeftMost_Receivers <- LeftMost_Receivers %>% select("gameId", "playId", "nflId", "displayName", "x", "y")
LeftMost_Receivers <- LeftMost_Receivers %>% rename(LeftMost_Receiver_ID = `nflId`,
                                                    LeftMost_Receiver_Name = `displayName`,
                                                    LeftMost_Receiver_X_AtSnap = `x`, LeftMost_Receiver_Y_AtSnap = 'y')
MergedData <- MergedData %>% left_join(LeftMost_Receivers, by = c("gameId", "playId"))

RightMost_Receivers <- MergedData %>% 
  filter(PlayerSideOfBall == "offense", Y_NetDistFromBall_Rank_BySide == 1, event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) 
RightMost_Receivers <- RightMost_Receivers %>% select("gameId", "playId", "nflId", "displayName", "x", "y")
RightMost_Receivers <- RightMost_Receivers %>% rename(RightMost_Receiver_ID = `nflId`,
                                                      RightMost_Receiver_Name = `displayName`,
                                                      RightMost_Receiver_X_AtSnap = `x`, RightMost_Receiver_Y_AtSnap = 'y')
MergedData <- MergedData %>% left_join(RightMost_Receivers, by = c("gameId", "playId"))

LeftMost_Defenders <- MergedData %>% 
  filter(PlayerSideOfBall == "defense", Y_NetDistFromBall_Rank_BySide == 1, event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) 
LeftMost_Defenders <- LeftMost_Defenders %>% select("gameId", "playId", "nflId", "displayName", "x", "y")
LeftMost_Defenders <- LeftMost_Defenders %>% rename(LeftMost_Defender_ID = `nflId`,
                                                    LeftMost_Defender_Name = `displayName`,
                                                    LeftMost_Defender_X_AtSnap = `x`, LeftMost_Defender_Y_AtSnap = 'y')
MergedData <- MergedData %>% left_join(LeftMost_Defenders, by = c("gameId", "playId"))

RightMost_Defenders <- MergedData %>% 
  filter(PlayerSideOfBall == "defense", Y_NetDistFromBall_Rank_BySide == 11, event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) 
RightMost_Defenders <- RightMost_Defenders %>% select("gameId", "playId", "nflId", "displayName", "x", "y")
RightMost_Defenders <- RightMost_Defenders %>% rename(RightMost_Defender_ID = `nflId`,
                                                      RightMost_Defender_Name = `displayName`,
                                                      RightMost_Defender_X_AtSnap = `x`, RightMost_Defender_Y_AtSnap = 'y')
MergedData <- MergedData %>% left_join(RightMost_Defenders, by = c("gameId", "playId"))

setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")
rm(LeftMost_Receivers, RightMost_Receivers, LeftMost_Defenders, RightMost_Defenders)

# Here is code for determining a pre-snap safety, made relevant for MergedData
# Recall that we've already eliminated all frames before the "line_set" event of each play
pre_snap_safety <- MergedData %>% filter(frameType != 'AFTER_SNAP') %>%
  # indicator for whether the player was a pre-snap safety, based on location
  mutate(safety_location = if_else(x >= Ball_X_Snap + 8.5 & PlayerSideOfBall == "defense" & displayName != RightMost_Defender_Name & displayName != LeftMost_Defender_Name, TRUE, FALSE)) %>% 
  # Alernate idea, scrap this one
  # mutate(safety_location = if_else(x >= Ball_X_Snap + 8.5 & PlayerSideOfBall == "defense" & !Y_NetDistFromBall_Rank_BySide %in% c(1, 11), TRUE, FALSE)) %>%   
  # group by game, play, player
  group_by(gameId, playId, nflId) %>% 
  # define pre-snap safeties
  summarize(is_pre_snap_safety = any(safety_location, na.rm = TRUE)) %>% 
  # ungroup players
  ungroup(nflId) %>% 
  # count number of safeties
  mutate(num_safeties_pre_snap = sum(is_pre_snap_safety)) %>% 
  # ungroup all columns
  ungroup()

# Get the ids of pre-snap safeties on each play
safety_ids_pre_snap = pre_snap_safety %>% 
  # filter out non-safeties
  filter(is_pre_snap_safety == TRUE) %>% 
  # drop is_pre_snap_safety column (will be accounted for later)
  select(-is_pre_snap_safety) %>%
  # group by game and play
  group_by(gameId, playId) %>% 
  # sort by nfl id
  arrange(nflId) %>% 
  # define safety id's
  mutate(pre_snap_safety_id = row_number()) %>%
  # pivot to wide
  pivot_wider(names_from = pre_snap_safety_id, values_from = nflId, names_prefix = 'pre_snap_safety_')

# Now join each of those two DFs into MergedData, to help us ID which individual players were pre-snap safeties
pre_snap_safety <- pre_snap_safety %>% select(-"num_safeties_pre_snap")
MergedData <- MergedData %>% left_join(pre_snap_safety, by = c("gameId", "playId", "nflId"))
# MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

MergedData <- MergedData %>% left_join(safety_ids_pre_snap, by = c("gameId", "playId"))
# MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

rm(pre_snap_safety, safety_ids_pre_snap)

# To briefly check data, see the maximum number of pre-snap safeties using this method
# View(MergedData %>% filter(frameId == 1) %>% arrange(desc(num_safeties_pre_snap)))

# View(MergedData %>% filter(frameId == 1, num_safeties_pre_snap > 2))
# Example of play that has a lot of pre-snap safeties show up: View(MergedData %>% filter(gameId == 2022091105, playId == 4308))
# 3rd-and-22, makes sense 
# Another: View(MergedData %>% filter(gameId == 2022091100, playId == 1928)) ... this one has 0:15 left in first half
# But there also instances where tracking data is just faulty, e.g. View(MergedData %>% filter(gameId == 2022091101, playId == 3125, is_pre_snap_safety == TRUE, frameType != "AFTER_SNAP"))
# On that one, it says ball_x at the snap was roughly 94.4, but film shows ball was at the +12, should've been around 98 in tracking data
# Even some offensive players have higher "x" than ball at the snap: View(tracking_week_1 %>% filter(gameId == 2022091101, playId == 3125, frameType == "SNAP"))
# Similar situation here, some of offense has lower "x" value than ball even though they shouldn't: View(MergedData %>% filter(gameId == 2022091101, playId == 1744, is_pre_snap_safety == TRUE, frameType != "AFTER_SNAP"))
# View(tracking_week_1 %>% filter(gameId == 2022091101, playId == 1744, frameType == "SNAP")) ... ball_x is around 65, should be around 62

# More thorough safety quality check
df_safety_quality_check <- MergedData %>%
  filter(position %in% c("SS","FS") | is_pre_snap_safety) %>%
  distinct(gameId, playId, nflId, displayName, position, is_pre_snap_safety)
# View(df_safety_quality_check)
# examine the players we assigned to pre-snap-safety who aren't officially safeties
table(
  (df_safety_quality_check %>% filter(is_pre_snap_safety))$position
)
# examine the official safeties who we did not assign to pre-snap-safety
table(
  (df_safety_quality_check %>% filter(position %in% c("SS","FS")))$is_pre_snap_safety
)
rm(df_safety_quality_check)

# Repeat the process with post-snap safeties
post_snap_safety <- MergedData %>% filter(frameType == 'AFTER_SNAP') %>%
  # group by game, play, player
  group_by(gameId, playId, nflId) %>% 
  # use the previously established definition of post_snap_safety (i.e. based on coverage responsibility)
  summarize(safety = any(is_post_snap_safety, na.rm = TRUE)) %>% 
  # ungroup players
  ungroup(nflId) %>% 
  # count number of safeties
  mutate(num_safeties_post_snap = sum(safety)) %>% 
  # ungroup all columns
  ungroup()

# ids of post-snap safeties on each play
safety_ids_post_snap = post_snap_safety %>% 
  # filter out non-safeties
  filter(safety == TRUE) %>% 
  # drop safety column, b/c bigger data set already has is_post_snap_safety_column
  select(-"safety") %>%
  # group by game and play
  group_by(gameId, playId) %>% 
  # sort by nfl id
  arrange(nflId) %>% 
  # define safety id's
  mutate(post_snap_safety_id = row_number()) %>%
  # pivot to wide
  pivot_wider(names_from = post_snap_safety_id, values_from = nflId, names_prefix = 'post_snap_safety_')

# Now add that info to MergedData
MergedData <- MergedData %>% left_join(safety_ids_post_snap, by = c("gameId", "playId"))
# MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")
rm(safety_ids_post_snap, post_snap_safety)

# Check to make sure we have FALSE values ... any defensive players who aren't safeties should have FALSE
# But any offensive players should have NA
table(MergedData$is_pre_snap_safety)
table(MergedData$is_post_snap_safety)
MergedData <- MergedData %>% mutate(is_pre_snap_safety = ifelse(PlayerSideOfBall == "offense", NA, is_pre_snap_safety))
MergedData <- MergedData %>% mutate(is_post_snap_safety = ifelse(PlayerSideOfBall == "offense", NA, is_post_snap_safety))

# Mutate a "time since snap" variable, which can be negative if the frame comes before the snap
# The 0.1 is so that the outcome variable is in seconds, rather than in frames
MergedData <- MergedData %>% mutate(time_since_snap = 0.1*(frameId - frameId_Snap))

# Here, limit to plays with <= 2 pre-snap safeties ... get rid of "3rd-and-forever" situations
MergedData <- MergedData %>% filter(num_safeties_pre_snap <= 2)

# Now, after avoiding any plays with more than 2 safeties, get each safety's X/Y position at the time of the snap
# This will help us detect whether there was an "X stagger," i.e. one safety way deeper than the other
Safety_1_AtSnap <- MergedData %>% 
  filter(PlayerSideOfBall == "defense", nflId == pre_snap_safety_1, event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) 
Safety_1_AtSnap <- Safety_1_AtSnap %>% select("gameId", "playId", "displayName", "x", "y")
Safety_1_AtSnap <- Safety_1_AtSnap %>% rename(pre_snap_safety_1_name = `displayName`,
                                              pre_snap_safety_1_X_AtSnap = `x`, pre_snap_safety_1_Y_AtSnap = `y`)
MergedData <- MergedData %>% left_join(Safety_1_AtSnap, by = c("gameId", "playId"))

Safety_2_AtSnap <- MergedData %>% 
  filter(PlayerSideOfBall == "defense", nflId == pre_snap_safety_2, event %in% c("ball_snap", "snap_direct", "autoevent_ballsnap")) 
Safety_2_AtSnap <- Safety_2_AtSnap %>% select("gameId", "playId", "displayName", "x", "y")
Safety_2_AtSnap <- Safety_2_AtSnap %>% rename(pre_snap_safety_2_name = `displayName`,
                                              pre_snap_safety_2_X_AtSnap = `x`, pre_snap_safety_2_Y_AtSnap = `y`)
MergedData <- MergedData %>% left_join(Safety_2_AtSnap, by = c("gameId", "playId"))
rm(Safety_1_AtSnap, Safety_2_AtSnap)

MergedData <- MergedData %>% mutate(X_Diff_BetweenSafeties_AtSnap = abs(pre_snap_safety_1_X_AtSnap - pre_snap_safety_2_X_AtSnap))
MergedData <- MergedData %>% mutate(Y_Diff_BetweenSafeties_AtSnap = abs(pre_snap_safety_1_Y_AtSnap - pre_snap_safety_2_Y_AtSnap))
MergedData <- MergedData %>% mutate(TotDist_BetweenSafeties_AtSnap =
                                      (sqrt((pre_snap_safety_1_X_AtSnap - pre_snap_safety_2_X_AtSnap)^2 + (pre_snap_safety_1_Y_AtSnap - pre_snap_safety_2_Y_AtSnap)^2)))

# Also get post_snap_safety_1_name and post_snap_safety_2_name using merge()
players <- fread("raw-data/players.csv")
NamesAndIDs <- players %>% select("nflId", "displayName")
NamesAndIDs_Safety1 <- NamesAndIDs %>% rename(post_snap_safety_1_name = `displayName`)
NamesAndIDs_Safety2 <- NamesAndIDs %>% rename(post_snap_safety_2_name = `displayName`)
MergedData <- merge(x = MergedData, y = NamesAndIDs_Safety1, 
                    by.x = "post_snap_safety_1", by.y = "nflId", all.x = TRUE)
MergedData <- merge(x = MergedData, y = NamesAndIDs_Safety2, 
                    by.x = "post_snap_safety_2", by.y = "nflId", all.x = TRUE)
rm(players, NamesAndIDs, NamesAndIDs_Safety1, NamesAndIDs_Safety2)

# And also, for eventual modeling purposes, turn PostSnap_MOF into a numeric variable (MOFO can be 1)
MergedData <- MergedData %>% mutate(PostSnap_MOF_Num = ifelse(PostSnap_MOF %in% "MOF Open", 1,
                                                              ifelse(PostSnap_MOF %in% "MOF Closed", 0, NA)))

# Get rid of the "Ambiguous" coverages
MergedData <- MergedData %>% filter(PostSnap_MOF %in% c("MOF Closed", "MOF Open"))

# For what it's worth, here are plays with exactly 2 pre-snap safeties
# TwoPreSnapSafety_Snaps <- MergedData %>% filter(num_safeties_pre_snap == 2)
# TwoPreSnapSafety_Snaps <- TwoPreSnapSafety_Snaps %>% select(-"num_safeties_pre_snap")
# Should be no NAs here: View(TwoPreSnapSafety_Snaps %>% filter(is.na(X_Diff_BetweenSafeties_AtSnap)))

###################################
### VECTORIZATION AND DISTANCES ###
###################################

# Frames for how long we want to project forward, if needed (e.g. where is safety projected to be in X.X seconds?)
frame_length <- 0.5

MergedData <- MergedData %>%
  mutate(X_proj_1 = x + (s*.1*cos((90-dir)*pi/180)),
         X_proj_2 = x + (s*.2*cos((90-dir)*pi/180)),
         X_proj_3 = x + (s*.3*cos((90-dir)*pi/180)),
         X_proj_4 = x + (s*.4*cos((90-dir)*pi/180)),
         X_proj_5 = x + (s*.5*cos((90-dir)*pi/180)),
         Y_proj_1 = y + (s*.1*sin((90-dir)*pi/180)),
         Y_proj_2 = y + (s*.2*sin((90-dir)*pi/180)),
         Y_proj_3 = y + (s*.3*sin((90-dir)*pi/180)),
         Y_proj_4 = y + (s*.4*sin((90-dir)*pi/180)),
         Y_proj_5 = y + (s*.5*sin((90-dir)*pi/180)))

# MergedData <- MergedData %>% 
#   left_join(BallCarrier_ProjDist, by = c("playId", "gameId", "frameId"))
# MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

# For the vectorized code, arrange like this so all 11 players on one team show up before other team
# MergedData <- MergedData %>% arrange(gameId, playId, frameId, club, nflId)
setDT(MergedData)
setkey(MergedData, gameId, playId, frameId, club, nflId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

# Now, here's code that gives each player's directional speed and acceleration
MergedData <- MergedData %>%
  mutate(x_vel_component = (s*cos((90-dir)*pi/180)),
         y_vel_component = (s*sin((90-dir)*pi/180)),
         x_acc_component = (a*cos((90-dir)*pi/180)),
         y_acc_component = (a*sin((90-dir)*pi/180)))

# Also create Y_SpeedTowardMOF and Y_AccTowardMOF
# Recall, going to offense's left/defense's right is positive Y, and offense's right/defense's left is negative Y
# In other words, Y = 0 at sideline to offense's right, Y = 53.3 at sideline to offense's left, Y = 26.65 at MOF
# Thus, if current Y location is >= 26.65 (so person is to offense's left), a positive Y velocity means you're moving further away from MOF
# Likewise, if current Y location is < 26.65 (person is to offense's right), a positive Y velocity means you're moving toward MOF
MergedData <- MergedData %>% 
  mutate(Y_SpeedTowardMOF = case_when(
    y >= 26.65 ~ -1 * y_vel_component,
    y < 26.65 ~ y_vel_component)) 

MergedData <- MergedData %>% 
  mutate(Y_AccTowardMOF = case_when(
    y >= 26.65 ~ -1 * y_acc_component,
    y < 26.65 ~ y_acc_component)) 

# Create "safety creep distance" using code similar to "aggregating frames" from 2023-24
# NOTE: b/c this DF will only include defensive players and only pre-snap frames, X_AbsDistFromBall is equivalent to X_DistFromBall
# In other words, it's not possible for a defensive player to have a negative X_DistFromBall value before the snap
MergedData_PreSnapFrames <- MergedData %>% filter(frameType != 'AFTER_SNAP')

# NOTE: if we want "last element," just replace the [1] with [length(ball_x)], or whatever the variable was
Safety1_PreSnapMovement_ByPlay <- MergedData_PreSnapFrames %>% filter(nflId == pre_snap_safety_1) %>%
  group_by(gameId, playId, displayName) %>%
  summarize(PreSnap_Frames = n(), down = max(down), distance = max(yardsToGo), Team = max(club),
            PosTeam = max(posteam), # DefTeam = max(defteam), posteam_type = max(posteam_type), 
            yardline_100 = max(yardline_100), Dropback = max(isDropback),
            Safety1_Initial_X = x[1], Safety1_Initial_Y = y[1], Safety1_Max_PreSnapSpeed = max(s), Safety1_Avg_PreSnapSpeed = mean(s),
            Safety1_Max_PreSnapAcceleration = max(a), Safety1_Avg_PreSnapAcceleration = mean(a),
            Safety1_Max_PreSnap_x_vel_component = max(x_vel_component), Safety1_Max_PreSnap_y_vel_component = max(y_vel_component), 
            Safety1_Max_PreSnap_x_acc_component = max(x_acc_component), Safety1_Max_PreSnap_y_acc_component = max(y_acc_component),
            Safety1_Min_PreSnap_x_vel_component = min(x_vel_component), Safety1_Min_PreSnap_y_vel_component = min(y_vel_component), 
            Safety1_Min_PreSnap_x_acc_component = min(x_acc_component), Safety1_Min_PreSnap_y_acc_component = min(y_acc_component),
            Safety1_Mean_PreSnap_x_vel_component = mean(x_vel_component), Safety1_Mean_PreSnap_y_vel_component = mean(y_vel_component), 
            Safety1_Mean_PreSnap_x_acc_component = mean(x_acc_component), Safety1_Mean_PreSnap_y_acc_component = mean(y_acc_component), 
            Safety1_Tot_PreSnapDistance = sum(dis), Safety1_InitialOrientation = o[1],
            Safety1_InitialDirection = dir[1], Ball_X_Snap = max(Ball_X_Snap),
            Ball_Y_Snap = max(Ball_Y_Snap), Safety1_Initial_X_DistFromBall = X_DistFromBall[1],
            Safety1_Initial_Y_DistFromBall = Y_DistFromBall[1], Safety1_Initial_Tot_DistFromBall = TotDistFromBall[1],
            Safety1_Initial_Y_DistFromMOF = Y_distFromMOF[1], Safety1_Initial_Y_AbsDistFromMOF = abs(Y_distFromMOF[1]),
            Safety1_Max_PreSnap_X_DistFromBall = max(X_DistFromBall), Safety1_Max_PreSnap_Y_DistFromBall = max(Y_DistFromBall),
            Safety1_Max_PreSnap_Y_AbsDistFromBall = max(Y_AbsDistFromBall), Safety1_Min_PreSnap_Y_DistFromBall = min(Y_DistFromBall),
            Safety1_Max_PreSnap_Tot_DistFromBall = max(TotDistFromBall),
            Safety1_X_DistFromBall_AtSnap = X_DistFromBall[length(X_DistFromBall)],
            Safety1_Y_DistFromBall_AtSnap = Y_DistFromBall[length(Y_DistFromBall)],
            Safety1_Y_AbsDistFromBall_AtSnap = Y_AbsDistFromBall[length(Y_AbsDistFromBall)],
            Safety1_Y_DistFromMOF_AtSnap = Y_distFromMOF[length(Y_distFromMOF)],
            Safety1_Y_AbsDistFromMOF_AtSnap = abs(Y_distFromMOF[length(Y_distFromMOF)]),
            Safety1_Tot_DistFromBall_AtSnap = TotDistFromBall[length(TotDistFromBall)],
            Safety1_Speed_AtSnap = s[length(s)], Safety1_Acceleration_AtSnap = a[length(a)],
            Safety1_x_vel_component_AtSnap = x_vel_component[length(x_vel_component)],
            Safety1_y_vel_component_AtSnap = y_vel_component[length(y_vel_component)],
            Safety1_x_acc_component_AtSnap = x_acc_component[length(x_acc_component)],
            Safety1_y_acc_component_AtSnap = y_acc_component[length(y_acc_component)],
            Safety1_Y_SpeedTowardMOF_AtSnap = Y_SpeedTowardMOF[length(Y_SpeedTowardMOF)],
            Safety1_Y_AccTowardMOF_AtSnap = Y_AccTowardMOF[length(Y_SpeedTowardMOF)],
            Safety1_Orientation_AtSnap = o[length(o)], Safety1_Direction_AtSnap = dir[length(dir)],
            Safety1_PosGroup = max(PosGroup), Safety1_position = max(position))

Safety2_PreSnapMovement_ByPlay <- MergedData_PreSnapFrames %>% filter(nflId == pre_snap_safety_2) %>%
  group_by(gameId, playId, displayName) %>%
  summarize(PreSnap_Frames = n(), down = max(down), distance = max(yardsToGo), Team = max(club),
            PosTeam = max(posteam), # DefTeam = max(defteam), posteam_type = max(posteam_type), 
            yardline_100 = max(yardline_100), Dropback = max(isDropback),
            Safety2_Initial_X = x[1], Safety2_Initial_Y = y[1], Safety2_Max_PreSnapSpeed = max(s), Safety2_Avg_PreSnapSpeed = mean(s),
            Safety2_Max_PreSnapAcceleration = max(a), Safety2_Avg_PreSnapAcceleration = mean(a),
            Safety2_Max_PreSnap_x_vel_component = max(x_vel_component), Safety2_Max_PreSnap_y_vel_component = max(y_vel_component), 
            Safety2_Max_PreSnap_x_acc_component = max(x_acc_component), Safety2_Max_PreSnap_y_acc_component = max(y_acc_component),
            Safety2_Min_PreSnap_x_vel_component = min(x_vel_component), Safety2_Min_PreSnap_y_vel_component = min(y_vel_component), 
            Safety2_Min_PreSnap_x_acc_component = min(x_acc_component), Safety2_Min_PreSnap_y_acc_component = min(y_acc_component),
            Safety2_Mean_PreSnap_x_vel_component = mean(x_vel_component), Safety2_Mean_PreSnap_y_vel_component = mean(y_vel_component), 
            Safety2_Mean_PreSnap_x_acc_component = mean(x_acc_component), Safety2_Mean_PreSnap_y_acc_component = mean(y_acc_component), 
            Safety2_Tot_PreSnapDistance = sum(dis), Safety2_InitialOrientation = o[1],
            Safety2_InitialDirection = dir[1], Ball_X_Snap = max(Ball_X_Snap),
            Ball_Y_Snap = max(Ball_Y_Snap), Safety2_Initial_X_DistFromBall = X_DistFromBall[1],
            Safety2_Initial_Y_DistFromBall = Y_DistFromBall[1], Safety2_Initial_Tot_DistFromBall = TotDistFromBall[1],
            Safety2_Initial_Y_DistFromMOF = Y_distFromMOF[1], Safety2_Initial_Y_AbsDistFromMOF = abs(Y_distFromMOF[1]),
            Safety2_Max_PreSnap_X_DistFromBall = max(X_DistFromBall), Safety2_Max_PreSnap_Y_DistFromBall = max(Y_DistFromBall),
            Safety2_Max_PreSnap_Y_AbsDistFromBall = max(Y_AbsDistFromBall), Safety2_Min_PreSnap_Y_DistFromBall = min(Y_DistFromBall),
            Safety2_Max_PreSnap_Tot_DistFromBall = max(TotDistFromBall),
            Safety2_X_DistFromBall_AtSnap = X_DistFromBall[length(X_DistFromBall)],
            Safety2_Y_DistFromBall_AtSnap = Y_DistFromBall[length(Y_DistFromBall)],
            Safety2_Y_AbsDistFromBall_AtSnap = Y_AbsDistFromBall[length(Y_AbsDistFromBall)],
            Safety2_Y_DistFromMOF_AtSnap = Y_distFromMOF[length(Y_distFromMOF)],
            Safety2_Y_AbsDistFromMOF_AtSnap = abs(Y_distFromMOF[length(Y_distFromMOF)]),
            Safety2_Tot_DistFromBall_AtSnap = TotDistFromBall[length(TotDistFromBall)],
            Safety2_Speed_AtSnap = s[length(s)], Safety2_Acceleration_AtSnap = a[length(a)],
            Safety2_x_vel_component_AtSnap = x_vel_component[length(x_vel_component)],
            Safety2_y_vel_component_AtSnap = y_vel_component[length(y_vel_component)],
            Safety2_x_acc_component_AtSnap = x_acc_component[length(x_acc_component)],
            Safety2_y_acc_component_AtSnap = y_acc_component[length(y_acc_component)],
            Safety2_Y_SpeedTowardMOF_AtSnap = Y_SpeedTowardMOF[length(Y_SpeedTowardMOF)],
            Safety2_Y_AccTowardMOF_AtSnap = Y_AccTowardMOF[length(Y_SpeedTowardMOF)],
            Safety2_Orientation_AtSnap = o[length(o)], Safety2_Direction_AtSnap = dir[length(dir)],
            Safety2_PosGroup = max(PosGroup), Safety2_position = max(position))
rm(MergedData_PreSnapFrames)

# Let's define "creep distance" as distance from ball at the time of line_set - distance from ball at the snap
# Another option is maximum distance from ball - distance from ball at the snap
# And another is maximum distance from ball - minimum distance from ball
# But we should probably account for players back-pedaling further away from the ball as well  ... i.e. we want negative values to be possible
Safety1_PreSnapMovement_ByPlay <- Safety1_PreSnapMovement_ByPlay %>% 
  mutate(Safety1_VertCreptDistance = Safety1_Initial_X_DistFromBall - Safety1_X_DistFromBall_AtSnap)
Safety1_PreSnapMovement_ByPlay <- Safety1_PreSnapMovement_ByPlay %>% 
  mutate(Safety1_TotalCreptDistance_TowardBall = Safety1_Initial_Tot_DistFromBall - Safety1_Tot_DistFromBall_AtSnap)

Safety2_PreSnapMovement_ByPlay <- Safety2_PreSnapMovement_ByPlay %>% 
  mutate(Safety2_VertCreptDistance = Safety2_Initial_X_DistFromBall - Safety2_X_DistFromBall_AtSnap)
Safety2_PreSnapMovement_ByPlay <- Safety2_PreSnapMovement_ByPlay %>% 
  mutate(Safety2_TotalCreptDistance_TowardBall = Safety2_Initial_Tot_DistFromBall - Safety2_Tot_DistFromBall_AtSnap)
# Keep in mind TotDistance already exists, for total distance covered altogether (even if it wasn't all directly toward the ball)

# Now before we run any merge()/join() functions, get rid of any unnecessary columns
# For example, don't need displayName, because pre_snap_safety_1_name already exists within MergedData
# And don't need down/distance, because those also already exist within MergedData
Safety1_PreSnapMovement_ByPlay <- Safety1_PreSnapMovement_ByPlay %>% 
  select(-c("displayName", "PreSnap_Frames", "down", "distance", "Team", "PosTeam", "yardline_100", "Dropback", "Ball_X_Snap", "Ball_Y_Snap"))
Safety2_PreSnapMovement_ByPlay <- Safety2_PreSnapMovement_ByPlay %>% 
  select(-c("displayName", "PreSnap_Frames", "down", "distance", "Team", "PosTeam", "yardline_100", "Dropback", "Ball_X_Snap", "Ball_Y_Snap"))

MergedData <- MergedData %>%
  left_join(Safety1_PreSnapMovement_ByPlay, by = c("gameId", "playId"))
rm(Safety1_PreSnapMovement_ByPlay)
MergedData <- MergedData %>%
  left_join(Safety2_PreSnapMovement_ByPlay, by = c("gameId", "playId"))
rm(Safety2_PreSnapMovement_ByPlay)

# MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

# Recall that X_Diff_BetweenSafeties_AtSnap, Y_Diff_BetweenSafeties_AtSnap, TotDist_BetweenSafeties_AtSnap already exist

# Also add difference in X_velocity, Y_velocity, total velocity at the snap
MergedData <- MergedData %>% mutate(X_Vel_Diff_BetweenSafeties_AtSnap = abs(Safety1_x_vel_component_AtSnap - Safety2_x_vel_component_AtSnap))
MergedData <- MergedData %>% mutate(Y_Vel_Diff_BetweenSafeties_AtSnap = abs(Safety1_y_vel_component_AtSnap - Safety2_y_vel_component_AtSnap))
MergedData <- MergedData %>% mutate(TotSpeed_Diff_BetweenSafeties_AtSnap = abs(Safety1_Speed_AtSnap - Safety2_Speed_AtSnap))

# Do the same for accelerations
MergedData <- MergedData %>% mutate(X_Acc_Diff_BetweenSafeties_AtSnap = abs(Safety1_x_acc_component_AtSnap - Safety2_x_acc_component_AtSnap))
MergedData <- MergedData %>% mutate(Y_Acc_Diff_BetweenSafeties_AtSnap = abs(Safety1_y_acc_component_AtSnap - Safety2_y_acc_component_AtSnap))
MergedData <- MergedData %>% mutate(TotAcc_Diff_BetweenSafeties_AtSnap = abs(Safety1_Acceleration_AtSnap - Safety2_Acceleration_AtSnap))

# Add Max_VertCreptDistance_AnySafety and Max_TotalCreptDistance_TowardBall_AnySafety
MergedData <- MergedData %>% mutate(Max_VertCreptDistance_AnySafety = pmax(Safety1_VertCreptDistance, Safety2_VertCreptDistance, na.rm = TRUE))
MergedData <- MergedData %>% mutate(Max_TotalCreptDistance_ToBall_AnySafety = pmax(Safety1_TotalCreptDistance_TowardBall, Safety2_TotalCreptDistance_TowardBall, na.rm = TRUE))

# Add Min_PreSnap_X_vel_component_AnySafety and Min_PreSnap_X_acc_component_AnySafety
# In other words, was either safety moving quickly toward the ball (i.e., smaller X) at some point before snap?
# Don't bother doing this for Y, since a negative "Y" value doesn't tell the same story
MergedData <- MergedData %>% mutate(Min_PreSnap_X_vel_component_AnySafety = pmin(Safety1_Min_PreSnap_x_vel_component, Safety2_Min_PreSnap_x_vel_component, na.rm = TRUE))
MergedData <- MergedData %>% mutate(Min_PreSnap_X_acc_component_AnySafety = pmin(Safety1_Min_PreSnap_x_acc_component, Safety2_Min_PreSnap_x_acc_component, na.rm = TRUE))

# With 1-high plays in mind, add Max_Y_AbsSpeed_AtSnap_AnySafety, Max_Y_AbsAcc_AtSnap_AnySafety, and Max_Y_AbsDistFromMOF_AtSnap_AnySafety
MergedData <- MergedData %>% mutate(Max_Y_AbsSpeed_AtSnap_AnySafety = pmax(abs(Safety1_y_vel_component_AtSnap), abs(Safety2_y_vel_component_AtSnap), na.rm = TRUE))
MergedData <- MergedData %>% mutate(Max_Y_AbsAcc_AtSnap_AnySafety = pmax(abs(Safety1_y_acc_component_AtSnap), abs(Safety2_y_acc_component_AtSnap), na.rm = TRUE))
MergedData <- MergedData %>% mutate(Max_Y_AbsDistFromMOF_AtSnap_AnySafety = pmax(Safety1_Y_AbsDistFromMOF_AtSnap, Safety2_Y_AbsDistFromMOF_AtSnap, na.rm = TRUE))

# Also create Max_Y_SpeedTowardMOF_AtSnap_AnySafety and Max_Y_AccTowardMOF_AtSnap_AnySafety, and minimums of same metrics
MergedData <- MergedData %>% mutate(Max_Y_SpeedTowardMOF_AtSnap_AnySafety = pmax(Safety1_Y_SpeedTowardMOF_AtSnap, Safety2_Y_SpeedTowardMOF_AtSnap, na.rm = TRUE))
MergedData <- MergedData %>% mutate(Min_Y_SpeedTowardMOF_AtSnap_AnySafety = pmin(Safety1_Y_SpeedTowardMOF_AtSnap, Safety2_Y_SpeedTowardMOF_AtSnap, na.rm = TRUE))
MergedData <- MergedData %>% mutate(Max_Y_AccTowardMOF_AtSnap_AnySafety = pmax(Safety1_Y_AccTowardMOF_AtSnap, Safety2_Y_AccTowardMOF_AtSnap, na.rm = TRUE))
MergedData <- MergedData %>% mutate(Min_Y_AccTowardMOF_AtSnap_AnySafety = pmin(Safety1_Y_AccTowardMOF_AtSnap, Safety2_Y_AccTowardMOF_AtSnap, na.rm = TRUE))

# And also add Y_SpeedTowardMOF_Diff_BetweenSafeties_AtSnap and Y_AccTowardMOF_Diff_BetweenSafeties_AtSnap 
MergedData <- MergedData %>% mutate(Y_SpeedTowardMOF_Diff_BetweenSafeties_AtSnap = abs(Safety1_Y_SpeedTowardMOF_AtSnap - Safety2_Y_SpeedTowardMOF_AtSnap))
MergedData <- MergedData %>% mutate(Y_AccTowardMOF_Diff_BetweenSafeties_AtSnap = abs(Safety1_Y_AccTowardMOF_AtSnap - Safety2_Y_AccTowardMOF_AtSnap))

# AND don't forget difference in the absolute distances from MOF between the safeties at the snap
# In other words, if one safety is 2 yards to his left of MOF, and other is 12 yards to his right, that's 10, not 14
# So a very big number indicates that one safety is very near MOF, the other is not
# In contrast, a small number means they're roughly equally far from MOF, in their respective directions
MergedData <- MergedData %>% mutate(Y_AbsDistFromMOF_Diff_BetweenSafeties_AtSnap = abs(Safety1_Y_AbsDistFromMOF_AtSnap - Safety2_Y_AbsDistFromMOF_AtSnap))

# Keep minSafetyDistToMOF in mind too (part of df_safety_movement_1 and 2 DFs)

# And create a data table for just dropbacks as well
Dropbacks_Merged <- MergedData 
rm(MergedData)
Dropbacks_Merged <- Dropbacks_Merged %>% select(-"isDropback")
setDT(Dropbacks_Merged)
setkey(Dropbacks_Merged, gameId, playId, nflId, frameId)
Dropbacks_Merged <- Dropbacks_Merged %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

# NOTE: Part 4 is data visualizations, but I'm putting that at the bottom b/c it doesn't need to use MergedData or Dropbacks_Merged

################
### MODELING ###
################

# But here's how to read in the data that resulted from the NN model's code
NN_model_results_DF <- fread("drive-data/results_df_preds.csv")
# Recall that "p" means probability of MOFO coverage

# Get the column names here to align with those of Dropbacks_Merged
colnames(NN_model_results_DF)
colnames(Dropbacks_Merged)
NN_model_results_DF <- NN_model_results_DF %>% rename(num_safeties_pre_snap = `num_safeties`)
NN_model_results_DF <- NN_model_results_DF %>% rename(defteam = `defensiveTeam`)
NN_model_results_DF <- NN_model_results_DF %>% rename(posteam = `possessionTeam`)
# NN_model_results_DF <- NN_model_results_DF %>% rename(wp = `winProbability`)
NN_model_results_DF <- NN_model_results_DF %>% rename(PostSnap_MOF_Num = `mofo_postsnap`)
NN_model_results_DF <- NN_model_results_DF %>% rename(pre_snap_safety_1 = `nflId_p1`)
NN_model_results_DF <- NN_model_results_DF %>% rename(pre_snap_safety_2 = `nflId_p2`)
NN_model_results_DF <- NN_model_results_DF %>% rename(pre_snap_safety_1_name = `displayName_p1`)
NN_model_results_DF <- NN_model_results_DF %>% rename(pre_snap_safety_2_name = `displayName_p2`)

# Now merge() this to Dropbacks_Merged ... to keep it simple, can just limit the NN_model_results_DF to gameId, playId, and "p"
NN_DF_abridged <- NN_model_results_DF %>% select(c("gameId", "playId", "p"))
# Also keep in mind that the model limited to first downs and second downs with 5+ yards to go
Dropbacks_Merged <- merge(x = Dropbacks_Merged, y = NN_DF_abridged, 
                          by = c("gameId", "playId"))
# FIXING DUPLICATION
Dropbacks_Merged <- Dropbacks_Merged %>% rename(MOFO_probability_FDA = `p`)
rm(NN_DF_abridged)

setDT(Dropbacks_Merged)
setkey(Dropbacks_Merged, gameId, playId, nflId, frameId)
Dropbacks_Merged <- Dropbacks_Merged %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

# The goal is to explore why the model says what it says, and to do so, clustering will be important
# Practically, to do so, we will need to join the modeling CSV with df_safety_movement_1 and df_safety_movement_2 CSVs
# We can cluster on the same features used in the model
# See the function “get_movement_X” in the 2_modeling GH doc for the features
df_safety_movement_1 <- read_csv("drive-data/df_safety_movement_1.csv")
df_safety_movement_2 <- read_csv("drive-data/df_safety_movement_2.csv")

# Get the column names here to align with those of Dropbacks_Merged
# Note: for 2-high plays, i.e. df_safety_movement_2, minSafetyDistToMOF refers to the minimum across both safeties, BEFORE SNAP
colnames(df_safety_movement_1)
colnames(df_safety_movement_2)
colnames(Dropbacks_Merged)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(num_safeties_pre_snap = `num_safeties`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(num_safeties_pre_snap = `num_safeties`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(defteam = `defensiveTeam`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(defteam = `defensiveTeam`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(posteam = `possessionTeam`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(posteam = `possessionTeam`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(PostSnap_MOF_Num = `mofo_postsnap`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(PostSnap_MOF_Num = `mofo_postsnap`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(Ball_X_Snap = `los`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Ball_X_Snap = `los`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(pre_snap_safety_1 = `nflId`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_1 = `nflId_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_2 = `nflId_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(pre_snap_safety_1_name = `displayName`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_1_name = `displayName_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_2_name = `displayName_p2`)
# Recall that x_first, y_first refer to 3 seconds before the snap in the NN model, NOT the locations during line_set
df_safety_movement_1 <- df_safety_movement_1 %>% rename(Safety1_X_3SecBeforeSnap = `x_first`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety1_X_3SecBeforeSnap = `x_first_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety2_X_3SecBeforeSnap = `x_first_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(Safety1_Y_3SecBeforeSnap = `y_first`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety1_Y_3SecBeforeSnap = `y_first_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety2_Y_3SecBeforeSnap = `y_first_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(pre_snap_safety_1_X_AtSnap = `x_last`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_1_X_AtSnap = `x_last_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_2_X_AtSnap = `x_last_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(pre_snap_safety_1_Y_AtSnap = `y_last`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_1_Y_AtSnap = `y_last_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_2_Y_AtSnap = `y_last_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(Safety1_x_vel_component_AtSnap = `v_x_last`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety1_x_vel_component_AtSnap = `v_x_last_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety2_x_vel_component_AtSnap = `v_x_last_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(Safety1_y_vel_component_AtSnap = `v_y_last`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety1_y_vel_component_AtSnap = `v_y_last_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety2_y_vel_component_AtSnap = `v_y_last_p2`)

# Now merge() these to Dropbacks_Merged, but probably have to separate to 1-high and 2-high plays
# To keep the merge simple, take out some unnecessary columns from df_safety_movement DFs
df_safety_movement_1 <- df_safety_movement_1 %>% 
  select(-c("pre_snap_safety_1", "pre_snap_safety_1_name", "num_safeties_pre_snap", "PostSnap_MOF_Num",
            "defteam", "posteam", "Ball_X_Snap", "pre_snap_safety_1_X_AtSnap", "pre_snap_safety_1_Y_AtSnap", "Safety1_x_vel_component_AtSnap", "Safety1_y_vel_component_AtSnap"))
final_dropbacks_1High <- merge(x = Dropbacks_Merged, y = df_safety_movement_1,
                               by = c("gameId", "playId"))

df_safety_movement_2 <- df_safety_movement_2 %>% 
  select(-c("pre_snap_safety_1", "pre_snap_safety_1_name", "pre_snap_safety_2", "pre_snap_safety_2_name", 
            "num_safeties_pre_snap", "PostSnap_MOF_Num",
            "defteam", "posteam", "Ball_X_Snap", "pre_snap_safety_1_X_AtSnap", "pre_snap_safety_1_Y_AtSnap",
            "pre_snap_safety_2_X_AtSnap", "pre_snap_safety_2_Y_AtSnap", , "Safety1_x_vel_component_AtSnap", "Safety1_y_vel_component_AtSnap",
            "Safety2_x_vel_component_AtSnap", "Safety2_y_vel_component_AtSnap"))
final_dropbacks_2High <- merge(x = Dropbacks_Merged, y = df_safety_movement_2,
                               by = c("gameId", "playId"))

rm(df_safety_movement_1, df_safety_movement_2)

# Adjust 1-high column names to match 2-high, then rbind()
final_dropbacks_1High <- final_dropbacks_1High %>% rename(x_spline_basis1_p1 = `x_spline_basis1`,
                                                          x_spline_basis2_p1 = `x_spline_basis2`,
                                                          x_spline_basis3_p1 = `x_spline_basis3`,
                                                          x_spline_basis4_p1 = `x_spline_basis4`,
                                                          x_spline_basis5_p1 = `x_spline_basis5`,
                                                          #  x_spline_basis6_p1 = `x_spline_basis6`,
                                                          #  x_spline_basis7_p1 = `x_spline_basis7`,
                                                          #  x_spline_basis8_p1 = `x_spline_basis8`,
                                                          #  x_spline_basis9_p1 = `x_spline_basis9`,
                                                          #  x_spline_basis10_p1 = `x_spline_basis10`,
                                                          y_spline_basis1_p1 = `y_spline_basis1`,
                                                          y_spline_basis2_p1 = `y_spline_basis2`,
                                                          y_spline_basis3_p1 = `y_spline_basis3`,
                                                          y_spline_basis4_p1 = `y_spline_basis4`,
                                                          y_spline_basis5_p1 = `y_spline_basis5`)
#  y_spline_basis6_p1 = `y_spline_basis6`,
#  y_spline_basis7_p1 = `y_spline_basis7`,
#  y_spline_basis8_p1 = `y_spline_basis8`,
#  y_spline_basis9_p1 = `y_spline_basis9`,
#  y_spline_basis10_p1 = `y_spline_basis10`)

# We can't rbind() while they have a different number of columns, so let's add "NA" columns to final_dropbacks_1High
# Identify the columns that are in final_dropbacks_2High but not in final_dropbacks_1High
missing_cols <- setdiff(names(final_dropbacks_2High), names(final_dropbacks_1High))

# Add these columns to final_dropbacks_1High with NA values
for (col in missing_cols) {
  final_dropbacks_1High[[col]] <- NA
}

# Now rbind() will work since both data frames have the same columns
final_dropbacks_merged <- rbind(final_dropbacks_1High, final_dropbacks_2High)
rm(Dropbacks_Merged, missing_cols, final_dropbacks_1High, final_dropbacks_2High)

# And change column name of minSafetyDistToMOF to make it less ambiguous
final_dropbacks_merged <- final_dropbacks_merged %>% rename(min_SafetyAbsHorizDistToMOF_PreSnap = `minSafetyDistToMOF`)

###################################
### AGGREGATE FRAMES INTO PLAYS ###
###################################

# Start with a version where each row represents a player on a full play (i.e. not a frame) 
# NOTE: if we want "last element," just replace the [1] with [length(ball_x)], or whatever the variable was
colnames(final_dropbacks_merged) 
# Now create a version where each row represents an entire play (not a player or a frame)
Stats_ByFullPlay_All9Weeks <- final_dropbacks_merged %>%
  group_by(gameId, playId) %>%
  summarize(Frames = n(), down = max(down), distance = max(yardsToGo), PosTeam = max(posteam), 
            DefTeam = max(defteam), posteam_type = max(posteam_type), # HomeTeam = max(homeTeamAbbr), AwayTeam = max(visitorTeamAbbr),
            yardline_100 = max(yardline_100), yardsToGo = max(yardsToGo), # Dropback = max(isDropback),
            # BallCarrierID = max(ballCarrierId), BallCarrierName = max(ballCarrierDisplayName),
            Description = max(playDescription), PenaltyYards = max(penaltyYards), 
            NetYardage = max(yardsGained), PrePenaltyYardage = max(prePenaltyYardsGained), 
            # YdsBeforeContact = max(YdsBeforeContact), YdsAfterContact = max(YdsAfterContact),
            # TeamDefendersInBox = max(defendersInTheBox),
            xpass = max(xpass), # pass_oe = max(pass_oe),
            EP = max(expectedPoints), EPA = max(expectedPointsAdded), EPSuccess = max(success),
            posteam_type = max(posteam_type), yardline_100 = max(yardline_100), # Shotgun = max(shotgun),
            aligned_left_receivers = max(aligned_left_receivers), 
            aligned_right_receivers = max(aligned_right_receivers), aligned_total_receivers = max(aligned_total_receivers),
            # sp = max(sp), goal_to_go = max(goal_to_go),
            DropbackType = max(passResult),
            # TargetReceiver_X_AtArrival = max(targetX), TargetReceiver_Y_AtArrival = max(targetY),
            # RPO = max(pff_runPassOption), 
            Coverage_Scheme = max(pff_passCoverage),
            ManVsZone = max(pff_manZone), PostSnap_MOF = max(PostSnap_MOF), PostSnap_MOF_Num = max(PostSnap_MOF_Num),
            # PlayClock_AtSnap = max(playClockAtSnap), no_huddle = max(no_huddle),
            # pass_horiz_location = max(pass_location), air_yards = max(air_yards), team_yards_after_catch = max(yards_after_catch), 
            Team_PassTD = max(pass_touchdown), Touchdown = max(touchdown),
            Team_RushTD = max(rush_touchdown), Team_ReturnTD = max(return_touchdown), 
            td_team = max(td_team), WP = max(wp), WPA = max(wpa), DefWPA = max(DefWPA), WPSuccess = max(WPSuccess),
            # OffTeam_Fumble = max(fumble), OffTeam_LostFumble = max(fumble_lost), 
            penalty_team = max(penalty_team), penalty_type = max(penalty_type), 
            return_team = max(return_team), # return_yards = max(return_yards),
            Completion_Prob = max(cp), CPOE = max(cpoe),
            Team_Interception = max(interception), # Off_AllowedSafety = max(safety), 
            Team_Completion = max(complete_pass),
            Team_PassAttempt = max(pass_attempt), # TeamTFL = max(tackled_for_loss), 
            # Temperature = max(Temperature), roof = max(roof), surface = max(surface), 
            first_down = max(first_down), # out_of_bounds = max(out_of_bounds), 
            PlayDirection = max(playDirection), 
            num_safeties_pre_snap = max(num_safeties_pre_snap), num_safeties_post_snap = max(num_safeties_post_snap),
            pre_snap_safety_1_ID = max(pre_snap_safety_1), pre_snap_safety_1_name = max(pre_snap_safety_1_name),
            pre_snap_safety_2_ID = max(pre_snap_safety_2), pre_snap_safety_2_name = max(pre_snap_safety_2_name),
            post_snap_safety_1_ID = max(post_snap_safety_1), post_snap_safety_1_name = max(post_snap_safety_1_name),
            post_snap_safety_2_ID = max(post_snap_safety_2), post_snap_safety_2_name = max(post_snap_safety_2_name),
            pre_snap_safety_1_X_AtSnap = max(pre_snap_safety_1_X_AtSnap), pre_snap_safety_1_Y_AtSnap = max(pre_snap_safety_1_Y_AtSnap), 
            pre_snap_safety_2_X_AtSnap = max(pre_snap_safety_2_X_AtSnap), pre_snap_safety_2_Y_AtSnap = max(pre_snap_safety_2_Y_AtSnap), 
            X_Diff_BetweenSafeties_AtSnap = max(X_Diff_BetweenSafeties_AtSnap),
            Y_Diff_BetweenSafeties_AtSnap = max(Y_Diff_BetweenSafeties_AtSnap),
            TotDist_BetweenSafeties_AtSnap = max(TotDist_BetweenSafeties_AtSnap),
            MOFO_probability_FDA = max(MOFO_probability_FDA), min_SafetyAbsHorizDistToMOF_PreSnap = max(min_SafetyAbsHorizDistToMOF_PreSnap),
            X_Vel_Diff_BetweenSafeties_AtSnap = max(X_Vel_Diff_BetweenSafeties_AtSnap),           
            Y_Vel_Diff_BetweenSafeties_AtSnap = max(Y_Vel_Diff_BetweenSafeties_AtSnap),
            TotSpeed_Diff_BetweenSafeties_AtSnap = max(TotSpeed_Diff_BetweenSafeties_AtSnap),        
            X_Acc_Diff_BetweenSafeties_AtSnap = max(X_Acc_Diff_BetweenSafeties_AtSnap),
            Y_Acc_Diff_BetweenSafeties_AtSnap = max(Y_Acc_Diff_BetweenSafeties_AtSnap),          
            TotAcc_Diff_BetweenSafeties_AtSnap = max(TotAcc_Diff_BetweenSafeties_AtSnap),
            Max_VertCreptDistance_AnySafety = max(Max_VertCreptDistance_AnySafety),           
            Max_TotalCreptDistance_ToBall_AnySafety = max(Max_TotalCreptDistance_ToBall_AnySafety),
            Min_PreSnap_X_vel_component_AnySafety = min(Min_PreSnap_X_vel_component_AnySafety),    
            Min_PreSnap_X_acc_component_AnySafety = min(Min_PreSnap_X_acc_component_AnySafety),
            Y_AbsDistFromMOF_Diff_BetweenSafeties_AtSnap = max(Y_AbsDistFromMOF_Diff_BetweenSafeties_AtSnap),
            Y_SpeedTowardMOF_Diff_BetweenSafeties_AtSnap = max(Y_SpeedTowardMOF_Diff_BetweenSafeties_AtSnap),
            Y_AccTowardMOF_Diff_BetweenSafeties_AtSnap = max(Y_AccTowardMOF_Diff_BetweenSafeties_AtSnap),
            Max_Y_AbsSpeed_AtSnap_AnySafety = max(Max_Y_AbsSpeed_AtSnap_AnySafety),   
            Max_Y_AbsAcc_AtSnap_AnySafety = max(Max_Y_AbsAcc_AtSnap_AnySafety),              
            Max_Y_AbsDistFromMOF_AtSnap_AnySafety = max(Max_Y_AbsDistFromMOF_AtSnap_AnySafety),
            Max_Y_SpeedTowardMOF_AtSnap_AnySafety = max(Max_Y_SpeedTowardMOF_AtSnap_AnySafety),
            Min_Y_SpeedTowardMOF_AtSnap_AnySafety = min(Max_Y_SpeedTowardMOF_AtSnap_AnySafety),
            Max_Y_AccTowardMOF_AtSnap_AnySafety = max(Max_Y_AccTowardMOF_AtSnap_AnySafety),
            Min_Y_AccTowardMOF_AtSnap_AnySafety = min(Max_Y_AccTowardMOF_AtSnap_AnySafety),
            Safety1_Initial_X = Safety1_Initial_X[1], Safety1_Initial_Y = Safety1_Initial_Y[1], 
            Safety1_Max_PreSnap_PreSnapSpeed = max(Safety1_Max_PreSnapSpeed), Safety1_Avg_PreSnapSpeed = mean(Safety1_Avg_PreSnapSpeed),
            Safety1_Max_PreSnapAcceleration = max(Safety1_Max_PreSnapAcceleration), 
            Safety1_Avg_PreSnapAcceleration = mean(Safety1_Avg_PreSnapAcceleration),
            Safety1_Max_PreSnap_x_vel_component = max(Safety1_Max_PreSnap_x_vel_component), 
            Safety1_Max_PreSnap_y_vel_component = max(Safety1_Max_PreSnap_y_vel_component), 
            Safety1_Max_PreSnap_x_acc_component = max(Safety1_Max_PreSnap_x_acc_component), 
            Safety1_Max_PreSnap_y_acc_component = max(Safety1_Max_PreSnap_y_acc_component),
            Safety1_Min_PreSnap_x_vel_component = min(Safety1_Min_PreSnap_x_vel_component), 
            Safety1_Min_PreSnap_y_vel_component = min(Safety1_Min_PreSnap_y_vel_component), 
            Safety1_Min_PreSnap_x_acc_component = min(Safety1_Min_PreSnap_x_acc_component), 
            Safety1_Min_PreSnap_y_acc_component = min(Safety1_Min_PreSnap_y_acc_component),
            Safety1_Mean_PreSnap_x_vel_component = mean(Safety1_Mean_PreSnap_x_vel_component), 
            Safety1_Mean_PreSnap_y_vel_component = mean(Safety1_Mean_PreSnap_y_vel_component), 
            Safety1_Mean_PreSnap_x_acc_component = mean(Safety1_Mean_PreSnap_x_acc_component), 
            Safety1_Mean_PreSnap_y_acc_component = mean(Safety1_Mean_PreSnap_y_acc_component), 
            Safety1_Tot_PreSnapDistance = sum(Safety1_Tot_PreSnapDistance), Safety1_InitialOrientation = Safety1_InitialOrientation[1],
            Safety1_InitialDirection = Safety1_InitialDirection[1], Safety1_Initial_X_DistFromBall = Safety1_Initial_X_DistFromBall[1],
            Safety1_Initial_Y_DistFromBall = Safety1_Initial_Y_DistFromBall[1], Safety1_Initial_Tot_DistFromBall = Safety1_Initial_Tot_DistFromBall[1],
            Safety1_Initial_Y_DistFromMOF = Safety1_Initial_Y_DistFromMOF[1], Safety1_Initial_Y_AbsDistFromMOF = Safety1_Initial_Y_AbsDistFromMOF[1],
            Safety1_Max_PreSnap_X_DistFromBall = max(Safety1_Max_PreSnap_X_DistFromBall),
            Safety1_Max_PreSnap_Y_DistFromBall = max(Safety1_Max_PreSnap_Y_DistFromBall), 
            Safety1_Min_PreSnap_Y_DistFromBall = min(Safety1_Min_PreSnap_Y_DistFromBall), 
            Safety1_Max_PreSnap_Y_AbsDistFromBall = max(Safety1_Max_PreSnap_Y_AbsDistFromBall), 
            Safety1_Max_PreSnap_Tot_DistFromBall = max(Safety1_Max_PreSnap_Tot_DistFromBall),
            Safety1_X_DistFromBall_AtSnap = max(Safety1_X_DistFromBall_AtSnap),
            Safety1_Y_DistFromBall_AtSnap = max(Safety1_Y_DistFromBall_AtSnap),
            Safety1_Y_AbsDistFromBall_AtSnap = max(Safety1_Y_AbsDistFromBall_AtSnap),
            Safety1_Y_DistFromMOF_AtSnap = max(Safety1_Y_DistFromMOF_AtSnap),
            Safety1_Y_AbsDistFromMOF_AtSnap = max(Safety1_Y_AbsDistFromMOF_AtSnap),
            Safety1_Tot_DistFromBall_AtSnap = max(Safety1_Tot_DistFromBall_AtSnap),
            Safety1_Speed_AtSnap = max(Safety1_Speed_AtSnap), Safety1_Acceleration_AtSnap = max(Safety1_Acceleration_AtSnap),
            Safety1_x_vel_component_AtSnap = max(Safety1_x_vel_component_AtSnap),
            Safety1_y_vel_component_AtSnap = max(Safety1_y_vel_component_AtSnap),
            Safety1_x_acc_component_AtSnap = max(Safety1_x_acc_component_AtSnap),
            Safety1_y_acc_component_AtSnap = max(Safety1_y_acc_component_AtSnap),
            Safety1_Orientation_AtSnap = max(Safety1_Orientation_AtSnap), Safety1_Direction_AtSnap = max(Safety1_Direction_AtSnap),
            Safety1_X_3SecBeforeSnap = max(Safety1_X_3SecBeforeSnap), Safety1_Y_3SecBeforeSnap = max(Safety1_Y_3SecBeforeSnap), 
            Safety1_VertCreptDistance = max(Safety1_VertCreptDistance), Safety1_TotalCreptDistance_TowardBall = max(Safety1_TotalCreptDistance_TowardBall),
            Safety1_PosGroup = max(Safety1_PosGroup), Safety1_position = max(Safety1_position),
            x_spline_basis1_p1 = max(x_spline_basis1_p1), x_spline_basis2_p1 = max(x_spline_basis2_p1), 
            x_spline_basis3_p1 = max(x_spline_basis3_p1), x_spline_basis4_p1 = max(x_spline_basis4_p1),
            x_spline_basis5_p1 = max(x_spline_basis5_p1), # x_spline_basis6_p1 = max(x_spline_basis6_p1),                       
            #  x_spline_basis7_p1 = max(x_spline_basis7_p1), x_spline_basis8_p1 = max(x_spline_basis8_p1),                     
            #  x_spline_basis9_p1 = max(x_spline_basis9_p1), x_spline_basis10_p1 = max(x_spline_basis10_p1),                   
            y_spline_basis1_p1 = max(y_spline_basis1_p1), y_spline_basis2_p1 = max(y_spline_basis2_p1),          
            y_spline_basis3_p1 = max(y_spline_basis3_p1), y_spline_basis4_p1 = max(y_spline_basis4_p1),                  
            y_spline_basis5_p1 = max(y_spline_basis5_p1), # y_spline_basis6_p1 = max(y_spline_basis6_p1),                 
            #  y_spline_basis7_p1 = max(y_spline_basis7_p1), y_spline_basis8_p1 = max(y_spline_basis8_p1),                  
            #  y_spline_basis9_p1 = max(y_spline_basis9_p1), y_spline_basis10_p1 = max(y_spline_basis10_p1),                  
            Safety2_Initial_X = Safety2_Initial_X[1], Safety2_Initial_Y = Safety2_Initial_Y[1], 
            Safety2_Max_PreSnap_PreSnapSpeed = max(Safety2_Max_PreSnapSpeed), Safety2_Avg_PreSnapSpeed = mean(Safety2_Avg_PreSnapSpeed),
            Safety2_Max_PreSnapAcceleration = max(Safety2_Max_PreSnapAcceleration), 
            Safety2_Avg_PreSnapAcceleration = mean(Safety2_Avg_PreSnapAcceleration),
            Safety2_Max_PreSnap_x_vel_component = max(Safety2_Max_PreSnap_x_vel_component), 
            Safety2_Max_PreSnap_y_vel_component = max(Safety2_Max_PreSnap_y_vel_component), 
            Safety2_Max_PreSnap_x_acc_component = max(Safety2_Max_PreSnap_x_acc_component), 
            Safety2_Max_PreSnap_y_acc_component = max(Safety2_Max_PreSnap_y_acc_component),
            Safety2_Min_PreSnap_x_vel_component = min(Safety2_Min_PreSnap_x_vel_component), 
            Safety2_Min_PreSnap_y_vel_component = min(Safety2_Min_PreSnap_y_vel_component), 
            Safety2_Min_PreSnap_x_acc_component = min(Safety2_Min_PreSnap_x_acc_component), 
            Safety2_Min_PreSnap_y_acc_component = min(Safety2_Min_PreSnap_y_acc_component),
            Safety2_Mean_PreSnap_x_vel_component = mean(Safety2_Mean_PreSnap_x_vel_component), 
            Safety2_Mean_PreSnap_y_vel_component = mean(Safety2_Mean_PreSnap_y_vel_component), 
            Safety2_Mean_PreSnap_x_acc_component = mean(Safety2_Mean_PreSnap_x_acc_component), 
            Safety2_Mean_PreSnap_y_acc_component = mean(Safety2_Mean_PreSnap_y_acc_component), 
            Safety2_Tot_PreSnapDistance = sum(Safety2_Tot_PreSnapDistance), Safety2_InitialOrientation = Safety2_InitialOrientation[1],
            Safety2_InitialDirection = Safety2_InitialDirection[1], Safety2_Initial_X_DistFromBall = Safety2_Initial_X_DistFromBall[1],
            Safety2_Initial_Y_DistFromBall = Safety2_Initial_Y_DistFromBall[1], Safety2_Initial_Tot_DistFromBall = Safety2_Initial_Tot_DistFromBall[1],
            Safety2_Initial_Y_DistFromMOF = Safety2_Initial_Y_DistFromMOF[1], Safety2_Initial_Y_AbsDistFromMOF = Safety2_Initial_Y_AbsDistFromMOF[1],
            Safety2_Max_PreSnap_X_DistFromBall = max(Safety2_Max_PreSnap_X_DistFromBall),
            Safety2_Max_PreSnap_Y_DistFromBall = max(Safety2_Max_PreSnap_Y_DistFromBall), 
            Safety2_Min_PreSnap_Y_DistFromBall = min(Safety2_Min_PreSnap_Y_DistFromBall), 
            Safety2_Max_PreSnap_Y_AbsDistFromBall = max(Safety2_Max_PreSnap_Y_AbsDistFromBall), 
            Safety2_Max_PreSnap_Tot_DistFromBall = max(Safety2_Max_PreSnap_Tot_DistFromBall),
            Safety2_X_DistFromBall_AtSnap = max(Safety2_X_DistFromBall_AtSnap),
            Safety2_Y_DistFromBall_AtSnap = max(Safety2_Y_DistFromBall_AtSnap),
            Safety2_Y_AbsDistFromBall_AtSnap = max(Safety2_Y_AbsDistFromBall_AtSnap),
            Safety2_Y_DistFromMOF_AtSnap = max(Safety2_Y_DistFromMOF_AtSnap),
            Safety2_Y_AbsDistFromMOF_AtSnap = max(Safety2_Y_AbsDistFromMOF_AtSnap),
            Safety2_Tot_DistFromBall_AtSnap = max(Safety2_Tot_DistFromBall_AtSnap),
            Safety2_Speed_AtSnap = max(Safety2_Speed_AtSnap), Safety2_Acceleration_AtSnap = max(Safety2_Acceleration_AtSnap),
            Safety2_x_vel_component_AtSnap = max(Safety2_x_vel_component_AtSnap),
            Safety2_y_vel_component_AtSnap = max(Safety2_y_vel_component_AtSnap),
            Safety2_x_acc_component_AtSnap = max(Safety2_x_acc_component_AtSnap),
            Safety2_y_acc_component_AtSnap = max(Safety2_y_acc_component_AtSnap),
            Safety2_Orientation_AtSnap = max(Safety2_Orientation_AtSnap), Safety2_Direction_AtSnap = max(Safety2_Direction_AtSnap),
            Safety2_X_3SecBeforeSnap = max(Safety2_X_3SecBeforeSnap), Safety2_Y_3SecBeforeSnap = max(Safety2_Y_3SecBeforeSnap), 
            Safety2_VertCreptDistance = max(Safety2_VertCreptDistance), Safety2_TotalCreptDistance_TowardBall = max(Safety2_TotalCreptDistance_TowardBall),
            Safety2_PosGroup = max(Safety2_PosGroup), Safety2_position = max(Safety2_position),
            x_spline_basis1_p2 = max(x_spline_basis1_p2), x_spline_basis2_p2 = max(x_spline_basis2_p2), 
            x_spline_basis3_p2 = max(x_spline_basis3_p2), x_spline_basis4_p2 = max(x_spline_basis4_p2),
            x_spline_basis5_p2 = max(x_spline_basis5_p2), # x_spline_basis6_p2 = max(x_spline_basis6_p2),                       
            #  x_spline_basis7_p2 = max(x_spline_basis7_p2), x_spline_basis8_p2 = max(x_spline_basis8_p2),                     
            #  x_spline_basis9_p2 = max(x_spline_basis9_p2), x_spline_basis10_p2 = max(x_spline_basis10_p2),                   
            y_spline_basis1_p2 = max(y_spline_basis1_p2), y_spline_basis2_p2 = max(y_spline_basis2_p2),          
            y_spline_basis3_p2 = max(y_spline_basis3_p2), y_spline_basis4_p2 = max(y_spline_basis4_p2),                  
            y_spline_basis5_p2 = max(y_spline_basis5_p2)) # y_spline_basis6_p2 = max(y_spline_basis6_p2),                 
#  y_spline_basis7_p2 = max(y_spline_basis7_p2), y_spline_basis8_p2 = max(y_spline_basis8_p2),                  
#  y_spline_basis9_p2 = max(y_spline_basis9_p2), y_spline_basis10_p2 = max(y_spline_basis10_p2))

Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>%
  mutate(Off_TD = ifelse(!is.na(td_team) & td_team == PosTeam, 1,
                         ifelse(is.na(td_team) | td_team != PosTeam, 0, NA)))

# And add the single-snap entropy value 
Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>% 
  mutate(Snap_Entropy = -MOFO_probability_FDA*log(MOFO_probability_FDA, 2) - (1 - MOFO_probability_FDA)*log((1-MOFO_probability_FDA), 2))

Median_Entropy <- median(Stats_ByFullPlay_All9Weeks$Snap_Entropy)

Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>% 
  mutate(HighEntropy = ifelse(Snap_Entropy > Median_Entropy, "High Entropy", "Low Entropy"))

Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>% 
  mutate(HighEntropy_Num = ifelse(Snap_Entropy > Median_Entropy, 1, 0))

# Interesting play-level stats: does offense perform notably worse against disguised coverages?
# For the sake of simplicity, define "disguised" as different number of safeties before and after the snap
Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>% mutate(Disguise = ifelse(num_safeties_pre_snap != num_safeties_post_snap, "Disguise", "No Disguise"))
Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>% mutate(Disguise_Num = ifelse(num_safeties_pre_snap != num_safeties_post_snap, 1, 0))

# write csv
write.csv(Stats_ByFullPlay_All9Weeks, paste0('drive-data/Stats_ByFullPlay_Week', iter, '.csv'))
# update progress bar
pb$tick()
}

###############
### MERGING ###
###############

# remove previous data
rm(list = ls())
# read in csv's
w1 = read_csv('drive-data/Stats_ByFullPlay_Week1.csv')
w2 = read_csv('drive-data/Stats_ByFullPlay_Week2.csv')
w3 = read_csv('drive-data/Stats_ByFullPlay_Week3.csv')
w4 = read_csv('drive-data/Stats_ByFullPlay_Week4.csv')
w5 = read_csv('drive-data/Stats_ByFullPlay_Week5.csv')
w6 = read_csv('drive-data/Stats_ByFullPlay_Week6.csv')
w7 = read_csv('drive-data/Stats_ByFullPlay_Week7.csv')
w8 = read_csv('drive-data/Stats_ByFullPlay_Week8.csv')
w9 = read_csv('drive-data/Stats_ByFullPlay_Week9.csv')
# mega merge
all = bind_rows(w1, w2, w3, w4, w5, w6, w7, w8, w9)
# write csv
write_csv(all, 'drive-data/Stats_ByFullPlay_ALL.csv')
