#################
### LIBRARIES ###
#################

# install.packages('gganimate')
library(gganimate)
# install.packages('gifski')
library(gifski)
# install.packages('tidyverse')
library(tidyverse)

#################
### FUNCTIONS ###
#################

# read tracking data
read_tracking = function(weeks) {
  # require tidyverse
  require(tidyverse, quietly = T)
  # initialize tracking
  tracking = tibble()
  # loop through each week
  for (week in weeks) {
    # construct the file path
    file_path = paste0('raw-data/tracking_week_', week, '.csv') 
    # read week data in
    tracking_week <- read_csv(file_path)
    # append week data to tracking
    tracking = bind_rows(tracking, tracking_week)
  }
  # return final tracking
  return(tracking)
}

# standardize tracking data
standardize_tracking = function(tracking) {
  # require tidyverse
  require(tidyverse, quietly = T)
  # field standardization
  tracking_std = tracking |> 
    mutate(
      # make all plays go from left to right
      x = ifelse(playDirection == 'left', 120 - x, x),
      y = ifelse(playDirection == 'left', 160 / 3 - y, y),
      # flip player direction, orientation
      dir = ifelse(playDirection == 'left', (dir + 180) %% 360, dir %% 360),
      o = ifelse(playDirection == 'left', (o + 180) %% 360, o %% 360),
      # change play direction to right
      playDirection = 'right'
    )
  # return standardized tracking
  return(tracking_std)
}

###########################
### CLEAN TRACKING DATA ###
###########################

# read in data
tracking_all = read_tracking(weeks = 1:9)
# write to file
write_csv(tracking_all, 'raw-data/tracking_all.csv')

# standardize tracking data
tracking_std = standardize_tracking(tracking_all)
# save to file
write_csv(tracking_std, 'processed-data/tracking_std.csv')

# read cleaned data
tracking_std = read_csv('processed-data/tracking_std.csv')

# These plays (among others) have multiple "first_contact" events, but we want only first instance
# View(tracking_std %>% filter(gameId == 2022100209, playId == 1581, event == "first_contact"))
# View(tracking_std %>% filter(gameId == 2022103100, playId == 1689, event == "first_contact"))
# View(tracking_std %>% filter(gameId == 2022103004, playId == 2106, event == "first_contact"))
FirstContact_Events <- tracking_std %>% filter(event == "first_contact") %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(FirstContact_rank = rank(frameId, ties.method = "first")) %>%
  ungroup()
FirstContact_Events <- FirstContact_Events %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId", "FirstContact_rank"))
MultiFirstContact_Plays <- FirstContact_Events %>% 
  group_by(gameId, playId) %>% 
  summarize(n = n(), Frames = n_distinct(frameId)) %>% arrange(desc(Frames))
tracking_std <- tracking_std %>%
  left_join(FirstContact_Events, by = c("gameId", "playId", "nflId", "displayName", "frameId"))

table(tracking_std$event)
# For any "FirstContact_rank" bigger than 1, change the event name to NA
tracking_std <- tracking_std %>% mutate(event =
                                                    ifelse(is.na(FirstContact_rank), event,
                                                           ifelse(FirstContact_rank > 1 & event == "first_contact", NA, event)))

rm(FirstContact_Events, MultiFirstContact_Plays)
tracking_std <- tracking_std %>% select(-"FirstContact_rank")

# Do the same thing with ball_snap, except this time we only want the last instance
# But first, do a quick confirmation that there are no plays with zero events for ball being snapped
tracking_std <- tracking_std %>% mutate(BallSnap_OnFrame = 
                                                    ifelse(!is.na(event) & event %in% c("ball_snap", "snap_direct"), 1, 
                                                           ifelse(!is.na(event) & !event %in% c("ball_snap", "snap_direct"), 0, NA)))
tracking_std <- tracking_std %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(BallSnap_OnFullPlay = sum(BallSnap_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(tracking_std %>% filter(is.na(BallSnap_OnFullPlay))) - it's empty
table(tracking_std$BallSnap_OnFullPlay)
# View(tracking_std %>% filter(BallSnap_OnFullPlay != 1)) - it's empty
tracking_std <- tracking_std %>% select(-"BallSnap_OnFullPlay")

BallSnap_Events <- tracking_std %>% filter(event %in% c("ball_snap", "snap_direct")) %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(BallSnap_rank = rank(-frameId, ties.method = "first")) %>%
  ungroup()
BallSnap_Events <- BallSnap_Events %>% 
  select(c("gameId", "playId", "nflId", "displayName", "frameId", "BallSnap_rank"))
MultiBallSnap_Plays <- BallSnap_Events %>% 
  group_by(gameId, playId) %>% 
  summarize(n = n(), Frames = n_distinct(frameId)) %>% arrange(desc(Frames))
tracking_std <- tracking_std %>%
  left_join(BallSnap_Events, by = c("gameId", "playId", "nflId", "displayName", "frameId"))

table(tracking_std$event)
# For any "BallSnap_rank" bigger than 1, change the event name to NA
tracking_std <- tracking_std %>% mutate(event =
                                                    ifelse(is.na(BallSnap_rank), event,
                                                           ifelse(BallSnap_rank > 1 & event == "first_contact", NA, event)))

# Let's also make sure no plays have more than 23 "players" during ball snap (i.e. one snap event per player, and the ball)
BallSnap_Multiples <- BallSnap_Events %>%
  group_by(gameId, playId) %>%
  summarize(n = n()) %>% arrange(desc(n))
# If any have less than 23, get rid of those (10 men on the field leads to iffy data)
BallSnap_All22_OnField <- BallSnap_Multiples %>% filter(n == 23)
tracking_std <- merge(x = tracking_std, y = BallSnap_All22_OnField,
                           by = c("gameId", "playId"))
tracking_std <- tracking_std %>% select(-"n")

rm(BallSnap_Events, MultiBallSnap_Plays, BallSnap_Multiples, BallSnap_All22_OnField)
tracking_std <- tracking_std %>% select(-"BallSnap_rank")

# Note that the first frame is typically, but not always, huddle_break_offense
FirstFrame_NotHuddleBreak <- tracking_std %>% filter(frameId == 1 & !event %in% "huddle_break_offense")
# Just glance at a few samples ... They often start with NA, then line_set comes a few frames later
# View(tracking_std %>% filter(gameId == 2022091200 & playId == 741 | gameId == 2022091102 & playId == 322 | gameId == 2022091101 & playId == 1785 |  gameId == 2022090800 & playId == 3190 | gameId == 2022091811 & playId == 2348 | gameId == 2022091805 & playId == 79))
# But sometimes first frame is line_set, or huddle_start_offense, or ball_snap, or man_in_motion
table(FirstFrame_NotHuddleBreak$event)

# View(tracking_std %>% filter(is.na(frameId))) - this is empty, as it should be 

# First, find out if the minimum frameId on any given play is always 1, or if there are data entry errors where a play starts w/ higher frameId
OpeningFrame_EachPlay <- tracking_std %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(frameId, ties.method = "first")) %>%
  ungroup() 
OpeningFrame_EachPlay <- OpeningFrame_EachPlay %>% filter(Frame_Rank == 1)
OpeningFrame_EachPlay <- OpeningFrame_EachPlay %>% select(-"Frame_Rank")
table(OpeningFrame_EachPlay$frameId) # it's always 1, so there are no "late frameId" errors

# Now that we know this, use group_by() to mutate tracking_std so all frames are labelled with the event from that play's first frame
# In other words, did this play's tracking data begin w/ huddle_break_offense, or ball_snap, or something else?
OpeningFrame_Event <- OpeningFrame_EachPlay %>% select("playId", "gameId", "nflId", "displayName", "event")
OpeningFrame_Event <- OpeningFrame_Event %>% rename(Frame1_Event = `event`)
  
tracking_std <- merge(x = tracking_std, y = OpeningFrame_Event, 
                          by = c("playId", "gameId", "nflId", "displayName"))
tracking_std <- tracking_std %>% arrange(gameId, playId, nflId, frameId)
table(tracking_std$Frame1_Event)
rm(FirstFrame_NotHuddleBreak, OpeningFrame_EachPlay, OpeningFrame_Event)

# Let's diagnose whether a play included the event for huddle_break_offense
# This will also allow us to see if any play erroneously includes multiple "huddle_break_offense" events
tracking_std <- tracking_std %>% mutate(HuddleBreak_OnFrame = 
      ifelse(!is.na(event) & event %in% "huddle_break_offense", 1, 
             ifelse(!is.na(event) & !event %in% "huddle_break_offense", 0, NA)))

tracking_std <- tracking_std %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(HuddleBreak_OnFullPlay = sum(HuddleBreak_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(tracking_std %>% filter(is.na(HuddleBreak_OnFullPlay))) - it's empty
table(tracking_std$HuddleBreak_OnFullPlay)
# View(tracking_std %>% filter(HuddleBreak_OnFullPlay > 1))
# Some examples: gameId 2022091107, playId 959 ... gameId 2022092500, playId 906

# For any play w/ multiple "huddle_break_offense" events, get rid of any frames before the most recent such event
HuddleBreak_DF <- tracking_std %>%
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

tracking_std <- merge(x = tracking_std, y = HuddleBreak_DF, 
                          by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_std <- tracking_std %>% arrange(gameId, playId, nflId, frameId)

tracking_std <- tracking_std %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_HuddleBreak) & frameId < FrameNumber_HuddleBreak & HuddleBreak_OnFullPlay > 1, TRUE, FALSE)) %>% 
  ungroup()

tracking_std <- tracking_std %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(HuddleBreak_DF)
tracking_std <- tracking_std %>% select(-c("Unnecessary_Early", "FrameNumber_HuddleBreak"))

# Repeat that process for huddle_start_offense
tracking_std <- tracking_std %>% mutate(HuddleStart_OnFrame = 
                                                    ifelse(!is.na(event) & event %in% "huddle_start_offense", 1, 
                                                           ifelse(!is.na(event) & !event %in% "huddle_start_offense", 0, NA)))

tracking_std <- tracking_std %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(HuddleStart_OnFullPlay = sum(HuddleStart_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(tracking_std %>% filter(is.na(HuddleStart_OnFullPlay))) - it's empty
table(tracking_std$HuddleStart_OnFullPlay)
# View(tracking_std %>% filter(HuddleStart_OnFullPlay > 1))

# For any play w/ multiple "huddle_start_offense" events, get rid of any frames before the most recent such event
HuddleStart_DF <- tracking_std %>%
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

tracking_std <- merge(x = tracking_std, y = HuddleStart_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_std <- tracking_std %>% arrange(gameId, playId, nflId, frameId)

tracking_std <- tracking_std %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_HuddleStart) & frameId < FrameNumber_HuddleStart & HuddleStart_OnFullPlay > 1, TRUE, FALSE)) %>% 
  ungroup()

tracking_std <- tracking_std %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(HuddleStart_DF)
tracking_std <- tracking_std %>% select(-c("Unnecessary_Early", "FrameNumber_HuddleStart"))

# Now see if there are any plays that have huddle_start_offense but not huddle_break_offense (should be impossible)
# View(tracking_std %>% filter(HuddleStart_OnFullPlay > 0 & HuddleBreak_OnFullPlay == 0))
# Turns out this is reasonably common ... best route is probably to get rid of any frames before huddle_start_offense, see below
# View(tracking_std %>% filter(HuddleStart_OnFullPlay > 0 & HuddleBreak_OnFullPlay > 0))
# IN CONTRAST, THIS ONE IS EMPTY ... no plays have an event for huddle starting AND huddle breaking

# Here's how we would get rid of unnecessary frames coming BEFORE offense broke the huddle
# In other words, doing it for all plays with a huddle_break_offense event, not just the plays that had multiple
# Isolate the plays that include an event for huddle_break_offense
Tracking_PlaysWithHuddleBreak <- tracking_std %>% filter(HuddleBreak_OnFullPlay == 1)
  
# Get rid of any frames that came before the huddle_break_offense event, IF THE PLAY HAD ONE
HuddleBreak_DF <- tracking_std %>%
  filter(event %in% c("huddle_break_offense")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_HuddleBreak = frameId)

tracking_std <- merge(x = tracking_std, y = HuddleBreak_DF, 
                            by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_std <- tracking_std %>% arrange(gameId, playId, nflId, frameId)

tracking_std <- tracking_std %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_HuddleBreak) & frameId < FrameNumber_HuddleBreak, TRUE, FALSE)) %>% 
  ungroup()

tracking_std <- tracking_std %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(HuddleBreak_DF)
tracking_std <- tracking_std %>% select(-c("Unnecessary_Early", "FrameNumber_HuddleBreak"))

# Glance to see if there's ever a huddle_start_offense event that isn't on the first frame ... there is
# View(tracking_std %>% filter(event %in% "huddle_start_offense" & frameId != 1))
# View(tracking_std %>% filter(gameId == 2022092505, playId == 2919))
# Therefore, can repeat the same process that we used with huddle_break_offense
Tracking_PlaysWithHuddleStart <- tracking_std %>% filter(HuddleStart_OnFullPlay == 1)

# Get rid of any frames that came before the huddle_start_offense event, IF THE PLAY HAD ONE
HuddleStart_DF <- tracking_std %>%
  filter(event %in% c("huddle_start_offense")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_HuddleStart = frameId)

tracking_std <- merge(x = tracking_std, y = HuddleStart_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_std <- tracking_std %>% arrange(gameId, playId, nflId, frameId)

tracking_std <- tracking_std %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_HuddleStart) & frameId < FrameNumber_HuddleStart, TRUE, FALSE)) %>% 
  ungroup()

tracking_std <- tracking_std %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(HuddleStart_DF)
tracking_std <- tracking_std %>% select(-c("Unnecessary_Early", "FrameNumber_HuddleStart"))

Tracking_PlaysWithHuddle_StartOrBreak <- tracking_std %>% filter(HuddleStart_OnFullPlay == 1 | HuddleBreak_OnFullPlay == 1)
# Can use this in place of the other two
rm(Tracking_PlaysWithHuddleBreak, Tracking_PlaysWithHuddleStart)

# Also mutate a variable for whether a play had a "line_set" event
tracking_std <- tracking_std %>% mutate(LineSet_OnFrame = 
                                                    ifelse(!is.na(event) & event %in% "line_set", 1, 
                                                           ifelse(!is.na(event) & !event %in% "line_set", 0, NA)))

tracking_std <- tracking_std %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(LineSet_OnFullPlay = sum(LineSet_OnFrame, na.rm = TRUE)) %>%
  ungroup() 
# View(tracking_std %>% filter(is.na(LineSet_OnFullPlay))) - it's empty
table(tracking_std$LineSet_OnFullPlay)
# OpeningFrames <- tracking_std %>% filter(frameId == 1)
# table(OpeningFrames$LineSet_OnFullPlay) ... over the full 9 weeks, 367494 / 370852 plays have a line_set, or 99.1%

# For any play w/ multiple "line_set" events, get rid of any frames before the most recent such event
LineSet_DF <- tracking_std %>%
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

tracking_std <- merge(x = tracking_std, y = LineSet_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_std <- tracking_std %>% arrange(gameId, playId, nflId, frameId)

tracking_std <- tracking_std %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_LineSet) & frameId < FrameNumber_LineSet & LineSet_OnFullPlay > 1, TRUE, FALSE)) %>% 
  ungroup()

tracking_std <- tracking_std %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(LineSet_DF)
tracking_std <- tracking_std %>% select(-c("Unnecessary_Early", "FrameNumber_LineSet"))

# And now do the same thing for all plays, i.e. NOT just the ones with multiple line_set events
# Get rid of any frames that came before the line_set event, IF THE PLAY HAD ONE
LineSet_DF <- tracking_std %>%
  filter(event %in% c("line_set")) %>%
  select(gameId, playId, nflId, displayName, frameId) %>%
  rename(FrameNumber_LineSet = frameId)

tracking_std <- merge(x = tracking_std, y = LineSet_DF, 
                           by = c("playId", "gameId", "nflId", "displayName"), all.x = TRUE)
tracking_std <- tracking_std %>% arrange(gameId, playId, nflId, frameId)

tracking_std <- tracking_std %>% group_by(gameId, playId, nflId) %>% 
  mutate(Unnecessary_Early = ifelse(!is.na(FrameNumber_LineSet) & frameId < FrameNumber_LineSet, TRUE, FALSE)) %>% 
  ungroup()

tracking_std <- tracking_std %>% filter(Unnecessary_Early == FALSE | is.na(Unnecessary_Early))
rm(LineSet_DF)
tracking_std <- tracking_std %>% select(-c("Unnecessary_Early", "FrameNumber_LineSet"))

# Here's what the plays look like with no line_set: View(tracking_std %>% filter(LineSet_OnFullPlay == 0))
# Typically it's just huddle start or huddle break, then ball snap, maybe w/ motion in between

# Now get rid of all plays with no line_set
tracking_std <- tracking_std %>% filter(LineSet_OnFullPlay == 1)
tracking_std <- tracking_std %>% select(-"LineSet_OnFullPlay")

# Remove these unless there's a reason we need them
rm(Tracking_PlaysWithHuddleBreak, Tracking_PlaysWithHuddleStart, Tracking_PlaysWithHuddle_StartOrBreak)

# Then use rank() to retroactively fix frameId for all plays (i.e. make them start at 1)
# This might not even really be necessary, but here's how to do it
# Probably helpful for incorporating video/dots, i.e. having any play start at Frame 1
tracking_std <- tracking_std %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(frameId = rank(frameId, ties.method = "first")) %>%
  ungroup() 
table(tracking_std$frameId)

# Now create line of scrimmage for each play using ball data
# Obviously, where the ball is during the ball_snap event is the LOS
Snap_Ball_Location <- tracking_combined %>%
  filter(club == "football", event %in% c("ball_snap", "snap_direct")) %>%
  select(gameId, playId, x, y, frameId) %>%
  rename(Ball_X_Snap = x, Ball_Y_Snap = y, frameId_Snap = frameId)

tracking_std <- tracking_std %>%
  left_join(Snap_Ball_Location, by = c("playId", "gameId"))

tracking_std <- tracking_std %>%
  mutate(X_dist_FromBall_OrigLocation = x - Ball_X_Snap, Y_distFromMOF = y - 26.65,
         Y_dist_FromBall_OrigLocation = y - Ball_Y_Snap)

# Code for where the ball is at any given point, and each player's distance from it
ball_df <- tracking_std %>% 
  filter(club == "football") %>% 
  select(gameId, playId, frameId, x, y) %>% 
  rename(ball_x = x,
         ball_y = y)

tracking_std <- tracking_std %>% 
  left_join(ball_df, by = c("playId", "gameId", "frameId"))

tracking_std <- tracking_std %>% 
  mutate(TotDistFromBall = sqrt((x - ball_x)^2 + (y - ball_y)^2),
         TotDistFromBall_OrigLocation = sqrt((x - Ball_X_Snap)^2 + (y - Ball_Y_Snap)^2),
         Y_DistFromBall = (y - ball_y), X_DistFromBall = (x - ball_x),
         Y_AbsDistFromBall = abs(y - ball_y), X_AbsDistFromBall = abs(x - ball_x))
rm(ball_df, Snap_Ball_Location)

# Likewise, add the ball's distance from goal line and sideline
tracking_std <- tracking_std %>%
  mutate(Ball_DistFromGoalLine = 110 - ball_x,
         Ball_DistFromSideline = ifelse(ball_y >= 26.65, 53.3 - ball_y, ball_y))

# And mutate a binary variable for being near goal line or sideline
tracking_std <- tracking_std %>% 
  mutate(BallNearGoalLine = ifelse(Ball_DistFromGoalLine <= 3, 1, 0),
         BallNearSideline = ifelse(Ball_DistFromSideline <= 3, 1, 0))

nflverse_pbp <- nflfastR::load_pbp(2022)
nflverse_pbp <- nflverse_pbp %>% filter(week %in% 1:9)
# This gives descriptions of NFLVerse columns: View(field_descriptions)

# View(nflverse_pbp %>% filter(run_location == "middle" & !is.na(run_gap)))
# This is empty, meaning all "middle" runs have NA for run_gap - adjust this
nflverse_pbp <- nflverse_pbp %>% mutate(
  run_gap = ifelse(run_location == "middle", "center", run_gap))

# Make sure that team column has no tricode inconsistency (e.g. JAX vs. JAC)
GamesDF_HomeTeamTricodes <- unique(games$homeTeamAbbr)
GamesDF_AwayTeamTricodes <- unique(games$visitorTeamAbbr)
PlaysDF_PosTeamTricodes <- unique(plays$possessionTeam)
PlaysDF_DefTeamTricodes <- unique(plays$defensiveTeam)
PlaysDF_SideOfFieldTricodes <- unique(plays$yardlineSide)
NFLVerse_HomeTeamTricodes <- unique(nflverse_pbp$home_team)
NFLVerse_AwayTeamTricodes <- unique(nflverse_pbp$away_team)
NFLVerse_PosTeamTricodes <- unique(nflverse_pbp$posteam)
NFLVerse_DefTeamTricodes <- unique(nflverse_pbp$defteam)

GamesDF_HomeTeamTricodes <- sort(GamesDF_HomeTeamTricodes)
GamesDF_AwayTeamTricodes <- sort(GamesDF_AwayTeamTricodes)
PlaysDF_PosTeamTricodes <- sort(PlaysDF_PosTeamTricodes)
PlaysDF_DefTeamTricodes <- sort(PlaysDF_DefTeamTricodes)
PlaysDF_SideOfFieldTricodes <- sort(PlaysDF_SideOfFieldTricodes)
NFLVerse_HomeTeamTricodes <- sort(NFLVerse_HomeTeamTricodes)
NFLVerse_AwayTeamTricodes <- sort(NFLVerse_AwayTeamTricodes)
NFLVerse_PosTeamTricodes <- sort(NFLVerse_PosTeamTricodes)
NFLVerse_DefTeamTricodes <- sort(NFLVerse_DefTeamTricodes)

identical(GamesDF_HomeTeamTricodes, GamesDF_AwayTeamTricodes)
identical(PlaysDF_PosTeamTricodes, PlaysDF_DefTeamTricodes)
identical(PlaysDF_SideOfFieldTricodes, PlaysDF_DefTeamTricodes)
identical(PlaysDF_SideOfFieldTricodes, GamesDF_HomeTeamTricodes)
identical(NFLVerse_HomeTeamTricodes, NFLVerse_AwayTeamTricodes)
identical(NFLVerse_PosTeamTricodes, NFLVerse_DefTeamTricodes)
identical(NFLVerse_HomeTeamTricodes, NFLVerse_PosTeamTricodes)
identical(NFLVerse_HomeTeamTricodes, GamesDF_HomeTeamTricodes)
# They are all aligned
rm(GamesDF_HomeTeamTricodes, GamesDF_AwayTeamTricodes,
   PlaysDF_PosTeamTricodes, PlaysDF_DefTeamTricodes,
   PlaysDF_SideOfFieldTricodes, NFLVerse_HomeTeamTricodes,
   NFLVerse_AwayTeamTricodes, NFLVerse_PosTeamTricodes,
   NFLVerse_DefTeamTricodes)

table(plays$passResult)
# C means complete, R means scramble, S means sack, IN means interception, I means incomplete
# All others are designed runs
plays <- plays %>% mutate(passResult = 
                            ifelse(passResult %in% c("C", "R", "IN", "I", "S"), passResult, NA))

# Arrange plays so they are sorted chronologically
plays <- plays %>% arrange(gameId, playId)

# The absoluteYardlineNumber is not accurate, just use nflverse's yardline_100
# And yardlineNumber doesn't give full field picture, e.g. -35 and +35 both say 35
# View(plays %>% filter(possessionTeam == yardlineSide & yardlineNumber < 20))
# This shows us all plays when the offensive team is inside its own 20
# Sometimes absoluteYardlineNumber is under 30, sometimes it's more than 100
plays <- plays %>% select(-"absoluteYardlineNumber", -"yardlineNumber", -"yardlineSide")

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

# These are all empty
# View(plays %>% filter(is.na(playNullifiedByPenalty)))
# View(plays %>% filter(is.na(offenseFormation)))
# View(plays %>% filter(is.na(playAction)))
# View(plays %>% filter(is.na(yardsGained)))
# View(plays %>% filter(is.na(isDropback)))

# Fix spelling error
player_play <- player_play %>% rename(wasTargetedReceiver = `wasTargettedReceiver`)

# View(player_play %>% filter(is.na(wasRunningRoute) & !is.na(routeRan))); it's empty
# View(player_play %>% filter(!is.na(wasRunningRoute) & is.na(routeRan))); it's empty

# View(player_play %>% filter(tackleAssist >= 1 & assistedTackle >= 1))
# This is empty, so clearly they are never both true ... make a new column that combines them
# View(player_play %>% filter(is.na(tackleAssist) | is.na(assistedTackle))) ... this is empty too
player_play <- player_play %>% mutate(assistedTackle = ifelse(tackleAssist %in% 1 | assistedTackle %in% 1, 1, 0))
player_play <- player_play %>% select(-"tackleAssist")

# Check if any play has multiple "solo tackle" instances, should be impossible
SoloTackle_Multiples <- player_play %>% filter(soloTackle > 0) %>%
  group_by(gameId, playId) %>%
  summarize(Players = n(), SoloTackles = sum(soloTackle), Assists = sum(assistedTackle)) %>% arrange(desc(SoloTackles))
# View(SoloTackle_Multiples) ... there are a lot of instances with multiple, probably all turnovers
# An instance of 3: View(plays %>% filter(gameId == 2022102303, playId == 3627)) ... this one had two separate forced fumbles
rm(SoloTackle_Multiples)

max(player_play$soloTackle) # it's 1, as it should be
min(player_play$soloTackle) # it's 0, as it should be
max(player_play$assistedTackle) # it's 1, as it should be
min(player_play$assistedTackle) # it's 0, as it should be
max(player_play$tackleForALoss) # it's 1, as it should be
min(player_play$tackleForALoss) # it's 0, as it should be
max(player_play$hadRushAttempt) # it's 1, as it should be
min(player_play$hadRushAttempt) # it's 0, as it should be
max(player_play$hadDropback) # it's 1, as it should be
min(player_play$hadDropback) # it's 0, as it should be
max(player_play$hadPassReception) # it's 1, as it should be
min(player_play$hadPassReception) # it's 0, as it should be
max(player_play$wasTargetedReceiver) # it's 1, as it should be
min(player_play$wasTargetedReceiver) # it's 0, as it should be
max(player_play$fumbles) # actually can be more than 1 b/c of weird lateral plays
# View(plays %>% filter(gameId == 2022100208, playId == 3895))
min(player_play$fumbles) # it's 0, as it should be
max(player_play$fumbleOutOfBounds) # it's 1, as it should be
min(player_play$fumbleOutOfBounds) # it's 0, as it should be
max(player_play$quarterbackHit) # it's 1, as it should be
min(player_play$quarterbackHit) # it's 0, as it should be
max(player_play$safetyAsDefense) # it's 1, as it should be
min(player_play$safetyAsDefense) # it's 0, as it should be
max(player_play$hadInterception) # it's 1, as it should be
min(player_play$hadInterception) # it's 0, as it should be

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
TrackingWithStats <- merge(x = player_play, y = tracking_std,
                             by = c("gameId", "playId", "nflId"), all.y = TRUE)

TrackingWithStats_PlayerNames <- merge(x = TrackingWithStats, y = players,
                                         by = c("nflId"))
# View(TrackingWithStats_PlayerNames %>% filter(is.na(club)))
# If we wanted to keep club == "football", we could use all.x = TRUE here
rm(players, player_play, tracking_std, TrackingWithStats)

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

# Check if any FGs are included in merged NFLVerse data
# View(PlaysAndGames_NFLVerse %>% filter(!is.na(kick_distance))); it's empty
# Same with timeouts
# View(PlaysAndGames_NFLVerse %>% filter(timeout > 0)); only includes challenges
# And general special teams plays
# View(PlaysAndGames_NFLVerse %>% filter(special_teams_play > 0)); it's empty
# And kneels/spikes
# View(PlaysAndGames_NFLVerse %>% filter(qb_kneel > 0 | qb_spike > 0)); empty
# And two-point conv
# View(PlaysAndGames_NFLVerse %>% filter(two_point_attempt > 0)); it's empty

colnames(TrackingWithStats_PlayerNames)
colnames(PlaysAndGames_NFLVerse)
# Make a new version of PlaysAndGames_NFLVerse with fewer columns, to save memory space 
# Note that home_score and away_score in NFLVerse refer to final scores
# Whereas total_home_score and total_away_score are entering that specific play
# Also passLocationType refers to QB's location, pass_location refers to where ball was thrown
# time includes game date for the tracking version, but only game clock for NFLVerse version
NFLVerse_Reduced <- PlaysAndGames_NFLVerse %>% 
  select(-c(3, 7, 10:11, 15:19, 31, 43:44, 54:55, 60:63, 67, 69, 73:74, 77, 79:85, 88:90, 
            98:105, 109:112, 119:135, 138:147, 149:151, 153:175, 184, 186:195, 204:205,
            214:218, 226:325, 329:331, 335:340, 347:348, 352, 355:377, 382, 385, 396:401,
            405, 407:416, 418:424))
TrackingWithStats_PlayerNames <- TrackingWithStats_PlayerNames %>% select(-"time", -"penaltyYards", -"Frame1_Event")

rm(PlaysAndGames_NFLVerse)
MergedData <- merge(x = NFLVerse_Reduced, y = TrackingWithStats_PlayerNames,
                    by = c("gameId", "playId"))
rm(TrackingWithStats_PlayerNames, NFLVerse_Reduced)

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

# And check for duplicate cells, e.g. two separate frames on the same play where Aaron Rodgers has frameId == 4
Duplicates_Merged <- MergedData %>% 
  group_by(nflId, playId, gameId, frameId) %>%
  summarize(Copies = n()) %>% arrange(desc(Copies))
# It's clean now
rm(Duplicates_Merged)

# Add a WP success column
MergedData <- MergedData %>% 
  mutate(WPSuccess = ifelse(wpa > 0, 1, 0))
# Check for NAs: View(MergedData %>% filter(is.na(WPSuccess)))

# And do the same for defensive WPA (just negative offensive WPA)
MergedData <- MergedData %>% mutate(DefWPA = (-1) * wpa)

# Add a column for "Is Ball Carrier"
MergedData <- MergedData %>% mutate(IsBallCarrier = 
   ifelse(!is.na(hadRushAttempt) & !is.na(hadPassReception) & (hadRushAttempt %in% 1 | hadPassReception %in% 1), TRUE, 
          ifelse(!is.na(hadRushAttempt) & !is.na(hadPassReception) & hadRushAttempt %in% 0 & hadPassReception %in% 0, FALSE, NA)))

# Diagnose if any plays have more than one "IsBallCarrier"
Frame1_DF <- MergedData %>% filter(frameId == 1)
BallCarriers_Snap <- Frame1_DF %>% 
  group_by(playId, gameId) %>%
  summarize(Players = n(), BallCarriers = sum(IsBallCarrier)) %>%
  arrange(desc(BallCarriers), desc(Players))
# No play has more than one
# But some plays do have zero, such as incompletions: View(MergedData %>% filter(gameId == 2022091101, playId == 63))
rm(BallCarriers_Snap, Frame1_DF)

# This is NOT empty, but all penalties
# View(MergedData %>% filter(is.na(pass_oe)))
# This is empty
# View(MergedData %>% filter(is.na(pass_oe) & is.na(penaltyYards)))

# Success_NA_Label <- MergedData %>% filter(is.na(success))
# it's empty, recall that this refers to EPA

# Find other ways to filter the data, e.g. excluding garbage time
# MergedData <- MergedData %>% filter(wp >= 0.05 & wp <= 0.95)
# Here's how to filter in a data table
MergedData <- MergedData[wp >= 0.05 & wp <= 0.95]

# Other possible modifications for this specific project:
MergedData <- MergedData[xpass <= 0.95]
MergedData <- MergedData[half_seconds_remaining >= 30]
# MergedData <- MergedData[half_seconds_remaining >= 45 | yardline_100 <= 50]

# Turn weather into a numeric variable using str_extract
# The given "temp" variable is all NAs
MergedData <- MergedData %>% 
  mutate(Temperature = (str_extract(MergedData$weather, "\\b\\d+")))
class(MergedData$Temperature) <- "numeric"
MergedData <- MergedData %>% select(-"weather")

# Similarly, change height into inches, rather than feet-inches
convert_to_inches <- function(height) {
  parts <- strsplit(height, "-")[[1]]
  feet <- as.numeric(parts[1])
  inches <- as.numeric(parts[2])
  total_inches <- feet * 12 + inches
  return(total_inches)
}

MergedData <- MergedData %>% 
  mutate(height_inches = sapply(height, convert_to_inches))
MergedData <- MergedData %>% select(-"height")

# Calculate player age by using birth date and game date
# MergedData <- MergedData %>% 
# mutate(NumericBirthDate = as.Date(birthDate, origin = "1970-01-01"))
# MergedData <- MergedData %>% 
#   mutate(NumericGameDate = as.Date(gameDate, origin = "1970-01-01"))
# MergedData <- MergedData %>% 
#   mutate(Age_Days = NumericGameDate - NumericBirthDate)
# MergedData <- MergedData %>% mutate(Age_Years = Age_Days / 365.25)
# class(MergedData$Age_Years) <- "numeric"
MergedData <- MergedData %>% select(-c("birthDate", "gameDate")) # "Age_Days" if needed

# Also code for each player's maximum speed ... could be useful b/c maybe defenses play 2-high against fastest WRs?
TopSeasonSpeeds <- MergedData %>% 
  group_by(nflId, displayName) %>%
  mutate(Indiv_SpeedRank = rank(-s, ties.method = "first")) %>%
  ungroup()
TopSeasonSpeeds <- TopSeasonSpeeds %>% select(nflId, displayName, s, Indiv_SpeedRank)  
TopSeasonSpeeds <- TopSeasonSpeeds %>% filter(Indiv_SpeedRank == 1)
TopSeasonSpeeds <- TopSeasonSpeeds %>% select(-"Indiv_SpeedRank")
TopSeasonSpeeds <- TopSeasonSpeeds %>% rename(Season_MaxSpeed = s)

MergedData <- merge(x = MergedData, y = TopSeasonSpeeds,
                    by = c("nflId", "displayName"))
rm(TopSeasonSpeeds)

# Here's list of plays with any penalty yardage
# View(MergedData %>% filter(!is.na(penaltyYards)))

# View(MergedData %>% filter(!is.na(penaltyYards) & playNullifiedByPenalty == "N"))
# These are cases where yardage was added after a play that counted
# E.G. horse collar tackle, or unnecessary roughness after play was done

# View(MergedData %>% filter(!is.na(penaltyYards) & playNullifiedByPenalty == "Y"))
# These are cases where the yardage gained on a play didn't count b/c of penalty
# E.G. big completion negated by holding or ineligible man downfield

# View(MergedData %>% filter(is.na(penaltyYards) & playNullifiedByPenalty == "Y"))
# This is empty, as it should be (can't have a penalty with no penalty yards)

# Make sure we have both offensive and defensive players
table(MergedData$position)

# Make broader "PosGroup" label from there
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

# Get rid of some more columns not needed for our specific models (can add back later if desired)
# View(MergedData %>% filter(playId != order_sequence)) ... order_sequence is almost always the same as playId
colnames(MergedData)
MergedData <- MergedData %>% 
  select(-c("week", "collegeName", "rushLocationType", "height_inches", "temp", "Season_MaxSpeed", "qbSneak",
            "drive", "run_location", "run_gap", "home_coach", "away_coach",
            "air_epa", "yac_epa", "rush_attempt", "order_sequence", "stadium", "stadium_id"))
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)

# Add distance ranks for each side of ball
# E.G. who is closest to ball-carrier at any given point of the play
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId, PlayerSideOfBall) %>%
  mutate(TotDistFromBall_Rank_BySideOfBall = rank(TotDistFromBall, ties.method = "first"),
         Y_AbsDistFromBall_Rank_BySide = rank(Y_AbsDistFromBall, ties.method = "first"),
         X_AbsDistFromBall_Rank_BySide = rank(X_AbsDistFromBall, ties.method = "first"),
         Y_NetDistFromBall_Rank_BySide = rank(Y_DistFromBall, ties.method = "first"),
         X_NetDistFromBall_Rank_BySide = rank(X_DistFromBall, ties.method = "first")) %>%
  ungroup()
# table(MergedData$TotDistFromBall_Rank_BySideOfBall); rank is never higher than 11

# And same idea for "overall" ranks (i.e., doesn't matter if player is on offense or defense)
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(TotDistFromBall_Rank_OVR = rank(TotDistFromBall, ties.method = "first"),
         Y_AbsDistFromBall_Rank_OVR = rank(Y_AbsDistFromBall, ties.method = "first"),
         X_AbsDistFromBall_Rank_OVR = rank(X_AbsDistFromBall, ties.method = "first"),
         Y_NetDistFromBall_Rank_OVR = rank(Y_DistFromBall, ties.method = "first"),
         X_NetDistFromBall_Rank_OVR = rank(X_DistFromBall, ties.method = "first")) %>%
  ungroup()
# table(MergedData$TotDistFromBall_Rank_OVR); rank is never higher than 22

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

# Create a Player_Role variable (i.e. defender, ball-carrier, or other offensive player)
MergedData <- MergedData %>%
  mutate(Player_Role = case_when(
    IsBallCarrier == 1 ~ "Ball Carrier",
    posteam != club & displayName != "football" ~ "Defense",
    posteam == club & IsBallCarrier < 1 ~ "Offense",
    displayName == "football" ~ "Football")) 

# Now, add ball-carrier traits to all frames
# This was more relevant in 2023-24, but still can be useful for defenders' orientation similarity to QB
# Start by writing a function for calculating distance
calc_distance <- function(x, y, x_baseline = 0, y_baseline = 0) {
  sqrt((x-x_baseline)^2 + (y - y_baseline)^2)
}

# Each player's distance to the ball carrier
# ball_x won't be exact same as X_ball_carrier (e.g. arm extended with ball)
# And recall that not every play does have a ball-carrier (e.g. incompletions), must account for that
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(X_ball_carrier = ifelse(!is.na(IsBallCarrier), x[which(IsBallCarrier == 1)], NA),
         Y_ball_carrier = ifelse(!is.na(IsBallCarrier), y[which(IsBallCarrier == 1)], NA),
         dist_to_ball_carrier = ifelse(!is.na(IsBallCarrier), 
            calc_distance(x, y, x_baseline = X_ball_carrier, y_baseline = Y_ball_carrier), NA)) %>%
  ungroup()

BallCarrier_Traits <- MergedData %>% 
  filter(IsBallCarrier > 0) %>% 
  select(gameId, playId, frameId, s, a, dis, o, dir, Season_MaxSpeed) %>% 
  rename(ball_carrier_speed = s, ball_carrier_acc = a, ball_carrier_dist_ran = dis,
         ball_carrier_orient = o, ball_carrier_direction = dir,
         # ball_carrier_weight = weight, ball_carrier_height = height_inches, 
         BC_Season_MaxSpeed = Season_MaxSpeed)
# BallCarrier_Traits <- BallCarrier_Traits %>% mutate(ball_carrier_momentum = ball_carrier_speed * ball_carrier_weight)

MergedData <- MergedData %>% 
  left_join(BallCarrier_Traits, by = c("playId", "gameId", "frameId"))

MergedData <- MergedData %>% 
  mutate(Rel_Speed_ToBC = s - ball_carrier_speed, Rel_Acc_ToBC = a - ball_carrier_acc,
         # Rel_Weight_ToBC = weight - ball_carrier_weight, Rel_Height_ToBC = height_inches - ball_carrier_height,
         Rel_Orient_ToBC = o - ball_carrier_orient, Rel_Dir_ToBC = dir - ball_carrier_direction,
         Rel_SeasonMaxSpeed_ToBC = Season_MaxSpeed - BC_Season_MaxSpeed) 
         # Momentum = s * weight, Rel_Momentum_ToBC = Momentum - ball_carrier_momentum)
rm(BallCarrier_Traits)

# Now get cosine similarity for the direction and orientation
# This gives 1 if you're in same direction, -1 if opposite direction, 0 if perpendicular
MergedData <- MergedData %>% 
  mutate(CosSimilarity_Orient_ToBC = cos(Rel_Orient_ToBC*pi/180),
         CosSimilarity_Dir_ToBC = cos(Rel_Dir_ToBC*pi/180))
MergedData <- MergedData %>% select(-c("Rel_Orient_ToBC", "Rel_Dir_ToBC"))

# Relative velocity accounts for direction, relative speed does NOT
# E.G., if Players X and Y are moving 10 yds/sec in opposite directions, relative speed is 0
# But, relative velocity is 10 - (-1 * 10), or 20
MergedData <- MergedData %>%
  mutate(Rel_Velocity_ToBC = s - (ball_carrier_speed * CosSimilarity_Dir_ToBC))

# Writing function for cosine similarity, if needed
# cosine_similarity_raw <- function(x, y) {
#   cos((x*pi/180) - (y*pi/180))
# }

# If needed, here's how to export to a CSV
write.csv(MergedData, "MergedData.csv")
