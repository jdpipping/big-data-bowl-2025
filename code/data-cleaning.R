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
      o = ifelse(playDirection == 'left', (o + 180) %% 360, o %% 360)
    )
  # return standardized tracking
  return(tracking_std)
}

###########################
### CLEAN TRACKING DATA ###
###########################

# # read in data
# tracking_all = read_tracking(weeks = 1:9)
# # write to file
# write_csv(tracking_all, 'raw-data/tracking_all.csv')
# 
# # standardize tracking data
# tracking_std = standardize_tracking(tracking_all)
# # save to file
# write_csv(tracking_std, 'processed-data/tracking_std.csv')

# read cleaned data
tracking_std = read_csv('processed-data/tracking_std.csv')

##########################
### DATA VISUALIZATION ###
##########################

# extract example play: https://www.youtube.com/watch?v=2mPxPOjnAg0
example_play_Davis_TD = tracking_std |> 
  filter(gameId == 2022100901, playId == 117) |> 
  mutate(color = case_when(club == 'PIT' ~ 'gold',
                           club == 'BUF' ~ 'blue',
                           club == 'football' ~ 'brown'))

# play visualization
play_animation_Davis_TD = example_play_Davis_TD |> 
  ggplot() +
  geom_point(aes(x = x, y = y, color = color, size = 3)) +
  # ensure colors display as assigned
  scale_color_identity() +
  # omit size from legend ... COMMENT OUT THE guides() LINE IF ANIMATE() FUNCTION DOESN'T WORK
  guides(size = FALSE) +
  theme_minimal() +
  transition_time(frameId) +
#   scale_x_continuous(breaks = seq(0, 120, 20)) +
#   scale_y_continuous(breaks = seq(0, 60, 10)) +
  labs(x = " ", y = " ",
       title = "Josh Allen 98-Yard TD Pass to Gabe Davis",
       subtitle = "First Quarter of Bills' 38-3 win over Steelers in Week 5, 2022",
       caption = "Data provided by Kaggle") +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) 
# Could add geom_abline() for each team's goal line, LOS, etc. if/when we want to

# show animation
animate(play_animation_Davis_TD, nframes = max(example_play_Davis_TD$frameId), fps = 10, renderer = gifski_renderer())

#############################
### BACK TO CLEANING DATA ###
#############################

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

rm(BallSnap_Events, MultiBallSnap_Plays)
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
Plays_WithHuddleBreak <- tracking_std %>% filter(HuddleBreak_OnFullPlay == 1)
  
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
Plays_WithHuddleStart <- tracking_std %>% filter(HuddleStart_OnFullPlay == 1)

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

# Then use rank() to retroactively fix frameId for all plays?? (i.e. make them start at 1)
# This might not even really be necessary, but here's how to do it
# Probably helpful for incorporating video, i.e. having any play start at Frame 1
tracking_std <- tracking_std %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(frameId = rank(frameId, ties.method = "first")) %>%
  ungroup() 
table(tracking_std$frameId)

# Now create line of scrimmage for each play using ball data
# Obviously, where the ball is during the ball_snap event is the LOS
Snap_Ball_Location <- tracking_std %>%
  filter(club == "football", event %in% c("ball_snap", "snap_direct")) %>%
  select(gameId, playId, x, y) %>%
  rename(Ball_X_Snap = x, Ball_Y_Snap = y)

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
plays <- plays %>% select(-"absoluteYardlineNumber", -"yardlineNumber")

# Fix one column that is spelled wrong
plays <- plays %>% rename(visitorTeamWinProbabilityAdded = `visitorTeamWinProbilityAdded`)

# View(plays %>% filter(is.na(expectedPointsAdded)))
# One play to fix here, a David Montgomery run on 10/09/22
# Since it's only one play, just find the right answer from nflverse
# View(nflverse_pbp %>% filter(old_game_id == 2022100904, qtr == 2) %>% select(1:35, 73, 74))
plays <- plays %>% mutate(expectedPointsAdded =
                            ifelse(is.na(expectedPointsAdded), -0.29447450, expectedPointsAdded)) 
