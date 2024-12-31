setwd("C:/Users/justi/OneDrive/Penn/BDB(2025)")
library(tidyverse)

#includes information on each game:
games <- read_csv('games.csv')

#includes information on all of the plays:
plays <- read_csv("plays.csv")

#contains player-level information from players that participated in any of the tracking data files.
players <- read_csv('players.csv')

# file contains player-level stats for each game and play
player_play_data <- read_csv("player_play.csv")

#tracking data:
week1 <- read_csv("tracking_week_1.csv")

# If needed, can read in MergedData file that was defined earlier in data engineering file
MergedData <- read_csv("MergedData.csv")

# view(games)
# 
# View(plays %>%
#        filter(gameId %in% unique(week1$gameId)))
# 
# View(players)
# 
# View(player_play_data %>%
#        filter(gameId==2022091110 &
#               playId==1868))
# 
# 
# View(plays %>%
#        filter(gameId==2022091110 &
#                 playId==1868))
# 
# View(week1 %>%
#        filter(gameId==2022091110 &
#                 playId==1868))

#let's test out some code with a sample play:
#Mahomes catch, 160 frame play (16 seconds)
sample_play <- week1 %>%
  filter(gameId==2022091110 & playId==1868)

#joining data together, selecting some variables of personal interest:
sample_play <- sample_play %>%
  #player data:
  left_join(players %>% select(nflId, position),by = 'nflId') %>% 
  #play information
  left_join(plays %>% select (gameId, 
                              playId, 
                              playDescription, 
                              quarter, 
                              down, 
                              yardsToGo,
                              possessionTeam,
                              defensiveTeam,
                              passResult,
                              passLocationType), by = c('gameId', 'playId')) %>%
  #player play data/pff assignments
  left_join(player_play_data %>%
              select(gameId,
                     playId,
                     nflId,
                     wasInitialPassRusher,
                     causedPressure,
                     timeToPressureAsPassRusher,
                     inMotionAtBallSnap,
                     shiftSinceLineset,
                     motionSinceLineset,
                     wasRunningRoute,
                     routeRan,
                     blockedPlayerNFLId1,
                     blockedPlayerNFLId2,
                     blockedPlayerNFLId3,
                     pressureAllowedAsBlocker,
                     timeToPressureAllowedAsBlocker,
                     pff_defensiveCoverageAssignment,
                     pff_primaryDefensiveCoverageMatchupNflId,
                     pff_secondaryDefensiveCoverageMatchupNflId),
            by = c('gameId', 'playId', 'nflId'))

#standardizing field:
sample_play <- sample_play %>%
  mutate(
    # make all plays go from left to right
    x = ifelse(playDirection == 'left', 120 - x, x),
    y = ifelse(playDirection == 'left', 160 / 3 - y, y),
    # flip player direction, orientation
    dir = ifelse(playDirection == 'left', (dir + 180) %% 360, dir %% 360),
    o = ifelse(playDirection == 'left', (o + 180) %% 360, o %% 360)
  )

###############################################################################
# This is code to project players' future locations (more relevant for 2023-24 project, but could be useful)
# Frames for how long we want to project forward, if needed (e.g. where is safety projected to be in X.X seconds?)
frame_length <- 0.5

# Recall, when making MergedData in data cleaning file, we standardized so highest "y" is always to offense's left
# I.e., make it so that it doesn't matter which end zone the offense is aiming at
# Likewise, adjust "o" and "dir" were adjusted so 0 is always to offense's left
# So 90 is always toward the EZ offense is aiming at, 180 is to offense's right, etc.
# Same adjustment for "x" - high "x" is always where offense is aiming at

# This is a basic visualization of which "dir" values are most common, based on play direction
MergedData %>%
  ggplot(aes(x = dir)) +
  geom_density(fill = 'dodgerblue') +
  facet_wrap(~playDirection)

# Here's same concept, but based on which side of ball player is on
MergedData %>%
  ggplot(aes(x = dir)) +
  geom_density(fill = 'dodgerblue') +
  facet_wrap(~PlayerSideOfBall)

# Adding projections for each player's location going forward (by 0.5 seconds)
# First, test out whether the physics-based approach or simple speed-based approach is more accurate
MergedData <- MergedData %>%
  mutate(X_proj_A = x + ((s*frame_length + 0.5*a*(frame_length)^2)*cos((90-dir)*pi/180)),
         Y_proj_A = y + ((s*frame_length + 0.5*a*(frame_length)^2)*sin((90-dir)*pi/180)),
         X_proj_B = x + (s*frame_length*cos((90-dir)*pi/180)),
         Y_proj_B = y + (s*frame_length*sin((90-dir)*pi/180)))

AccuracyTest <- MergedData %>% mutate(X_Actual_0.5SecAhead = 
                                        ifelse(displayName == lead(displayName, 5), lead(x, 5), NA))
AccuracyTest <- AccuracyTest %>% mutate(Y_Actual_0.5SecAhead = 
                                          ifelse(displayName == lead(displayName, 5), lead(y, 5), NA))

AccuracyTest <- AccuracyTest %>%
  mutate(X_proj_ErrorA = X_proj_A - X_Actual_0.5SecAhead,
         X_proj_ErrorB = X_proj_B - X_Actual_0.5SecAhead,
         Y_proj_ErrorA = Y_proj_A - Y_Actual_0.5SecAhead,
         Y_proj_ErrorB = Y_proj_B - Y_Actual_0.5SecAhead)

sd(AccuracyTest$X_proj_ErrorA, na.rm = TRUE)
sd(AccuracyTest$X_proj_ErrorB, na.rm = TRUE)
sd(AccuracyTest$Y_proj_ErrorA, na.rm = TRUE)
sd(AccuracyTest$Y_proj_ErrorB, na.rm = TRUE)

mean(AccuracyTest$X_proj_ErrorA, na.rm = TRUE)
mean(AccuracyTest$X_proj_ErrorB, na.rm = TRUE)
mean(AccuracyTest$Y_proj_ErrorA, na.rm = TRUE)
mean(AccuracyTest$Y_proj_ErrorB, na.rm = TRUE)

sqrt(mean((AccuracyTest$X_proj_ErrorA)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$X_proj_ErrorB)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$Y_proj_ErrorA)^2, na.rm = TRUE))
sqrt(mean((AccuracyTest$Y_proj_ErrorB)^2, na.rm = TRUE))
# Method B (non-physics approach) has significantly lower RMSE

# Now that we know these, we can get rid of both of them
# Then use non-physics approach to get each player's projected location over span of next 0.5 seconds
MergedData <- MergedData %>% select(-c("X_proj_A", "Y_proj_A", "X_proj_B", "Y_proj_B"))
rm(AccuracyTest)

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

# And, now, also add the ball-carrier's projected location within the next 0.5 seconds, if it's useful
BallCarrier_ProjDist <- MergedData %>% 
  filter(IsBallCarrier > 0) %>% 
  select(gameId, playId, frameId, s, a, o, dir, x, y) %>% 
  rename(ball_carrier_speed = s, ball_carrier_acc = a,
         ball_carrier_orient = o, ball_carrier_direction = dir,
         ball_carrier_x = x, ball_carrier_y = y)
BallCarrier_ProjDist <- BallCarrier_ProjDist %>% 
  mutate(ball_carrier_X_proj_1 = ball_carrier_x + (ball_carrier_speed*.1*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_X_proj_2 = ball_carrier_x + (ball_carrier_speed*.2*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_X_proj_3 = ball_carrier_x + (ball_carrier_speed*.3*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_X_proj_4 = ball_carrier_x + (ball_carrier_speed*.4*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_X_proj_5 = ball_carrier_x + (ball_carrier_speed*.5*cos((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_1 = ball_carrier_y + (ball_carrier_speed*.1*sin((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_2 = ball_carrier_y + (ball_carrier_speed*.2*sin((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_3 = ball_carrier_y + (ball_carrier_speed*.3*sin((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_4 = ball_carrier_y + (ball_carrier_speed*.4*sin((90-ball_carrier_direction)*pi/180)),
         ball_carrier_Y_proj_5 = ball_carrier_y + (ball_carrier_speed*.5*sin((90-ball_carrier_direction)*pi/180)))
BallCarrier_ProjDist <- BallCarrier_ProjDist %>% 
  select(c("playId", "gameId", "frameId", "ball_carrier_X_proj_1", 
           "ball_carrier_X_proj_2", "ball_carrier_X_proj_3", "ball_carrier_X_proj_4",
           "ball_carrier_X_proj_5", "ball_carrier_Y_proj_1", "ball_carrier_Y_proj_2",
           "ball_carrier_Y_proj_3", "ball_carrier_Y_proj_4", "ball_carrier_Y_proj_5"))
MergedData <- MergedData %>% 
  left_join(BallCarrier_ProjDist, by = c("playId", "gameId", "frameId"))

# MergedData <- MergedData %>% arrange(gameId, playId, nflId, frameId)
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(ball_carrier_X_proj_1 = ball_carrier_X_proj_1,
         ball_carrier_X_proj_2 = ball_carrier_X_proj_2,
         ball_carrier_X_proj_3 = ball_carrier_X_proj_3,
         ball_carrier_X_proj_4 = ball_carrier_X_proj_4,
         ball_carrier_X_proj_5 = ball_carrier_X_proj_5,
         ball_carrier_Y_proj_1 = ball_carrier_Y_proj_1,
         ball_carrier_Y_proj_2 = ball_carrier_Y_proj_2,
         ball_carrier_Y_proj_3 = ball_carrier_Y_proj_3,
         ball_carrier_Y_proj_4 = ball_carrier_Y_proj_4,
         ball_carrier_Y_proj_5 = ball_carrier_Y_proj_5,
         proj_dist_to_ball_carrier_1 = calc_distance(X_proj_1, 
                                                     Y_proj_1, 
                                                     x_baseline = ball_carrier_X_proj_1, 
                                                     y_baseline = ball_carrier_Y_proj_1),
         proj_dist_to_ball_carrier_2 = calc_distance(X_proj_2, 
                                                     Y_proj_2, 
                                                     x_baseline = ball_carrier_X_proj_2, 
                                                     y_baseline = ball_carrier_Y_proj_2),
         proj_dist_to_ball_carrier_3 = calc_distance(X_proj_3, 
                                                     Y_proj_3, 
                                                     x_baseline = ball_carrier_X_proj_3, 
                                                     y_baseline = ball_carrier_Y_proj_3),
         proj_dist_to_ball_carrier_4 = calc_distance(X_proj_4, 
                                                     Y_proj_4, 
                                                     x_baseline = ball_carrier_X_proj_4, 
                                                     y_baseline = ball_carrier_Y_proj_4),
         proj_dist_to_ball_carrier_5 = calc_distance(X_proj_5, 
                                                     Y_proj_5, 
                                                     x_baseline = ball_carrier_X_proj_5, 
                                                     y_baseline = ball_carrier_Y_proj_5)) %>%
  ungroup()
rm(BallCarrier_ProjDist)

# Now mutate for the minimum projected distance to ball-carrier over the next 0.5 seconds
MergedData <- MergedData %>% mutate(min_proj_dist_to_ball_carrier =
                                      pmin(proj_dist_to_ball_carrier_1, proj_dist_to_ball_carrier_2, proj_dist_to_ball_carrier_3,
                                           proj_dist_to_ball_carrier_4, proj_dist_to_ball_carrier_5))
MergedData <- MergedData %>% select(c(-"proj_dist_to_ball_carrier_1", -"proj_dist_to_ball_carrier_2", 
                                      -"proj_dist_to_ball_carrier_3", -"proj_dist_to_ball_carrier_4", -"proj_dist_to_ball_carrier_5"))

###############################################################################
# Let's calculate distance to X closest players (and obtain the nflId of that closest)
## Distance Formula
calc_distance <- function(x, y, x_baseline = 0, y_baseline = 0) {
  sqrt((x-x_baseline)^2 + (y - y_baseline)^2)
}

# vectorized bit of code:
#WE CAN PROBABLY CLEAN THIS UP, BUT THIS IS HOW THIS WORKS:
#1) we use map_dbl. .x is a vector, and in this case it's the row of each frame (excludes the football, so should be 1 - 22)
#2) the first column (min_dist), basically calculates the distance for each row number in the frame to all other points on the opposing team (besides the ball), and calculates the minimum
#3) the second column (num_same_dist), takes the number of unique min distances from the previous column to determine if there are any ties
#4) excludes football from that calculation
#5) takes the position in the vector of 

sample_play <- sample_play %>% 
  group_by(gameId, playId, frameId) %>%
 mutate(min_dist_opp_player = map_dbl(.x=row_number(), ~min(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                           y = y[which(club[.x]!=club & club!='football')],
                                                                           x_baseline = x[.x],
                                                                           y_baseline = y[.x]))),
         num_opp_players_same_dist = map_dbl(.x=row_number(), ~11 - length(unique(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                y = y[which(club[.x]!=club & club!='football')],
                                                                                                x_baseline = x[.x],
                                                                                                y_baseline = y[.x])))),
         num_opp_players_same_dist = ifelse(displayName == 'football', 0, num_opp_players_same_dist),
         min_dist_opp_index = map_dbl(.x=row_number(), ~which(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                            y = y[which(club[.x]!=club & club!='football')],
                                                                            x_baseline = x[.x],
                                                                            y_baseline = y[.x])== min(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                                    y = y[which(club[.x]!=club & club!='football')],
                                                                                                                    x_baseline = x[.x],
                                                                                                                    y_baseline = y[.x])))[1]),
         second_closest_dist_opp_player = map_dbl(.x=row_number(), ~Rfast::nth(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                             y = y[which(club[.x]!=club & club!='football')],
                                                                                             x_baseline = x[.x],
                                                                                             y_baseline = y[.x]), 2, descending = F)),
         second_closest_opp_index = map_dbl(.x=row_number(), ~which(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                  y = y[which(club[.x]!=club & club!='football')],
                                                                                  x_baseline = x[.x],
                                                                                  y_baseline = y[.x])== Rfast::nth(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                                                 y = y[which(club[.x]!=club & club!='football')],
                                                                                                                                 x_baseline = x[.x],
                                                                                                                                 y_baseline = y[.x]),2,descending = F))[1]) # Where I account for duplicates
  ) %>%
  ungroup()

# We can also repeat this process using MergedData, as defined in the data cleaning file
# Recall in MergedData, we already have TotDistFromBall
# Also Y_DistFromBall, X_DistFromBall, Y_AbsDistFromBall, X_AbsDistFromBall
# And the overall distance from ball-carrier

# For this vectorization code, arrange like this so all 11 players on one team show up before other team
MergedData <- MergedData %>% arrange(gameId, playId, frameId, club, nflId)

## Calculating distances to closest players on opposing teams
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(min_dist_opp_player = map_dbl(.x=row_number(), ~min(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                           y = y[which(club[.x]!=club & club!='football')],
                                                                           x_baseline = x[.x],
                                                                           y_baseline = y[.x]))),
         num_opp_players_same_dist = map_dbl(.x=row_number(), ~11 - length(unique(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                y = y[which(club[.x]!=club & club!='football')],
                                                                                                x_baseline = x[.x],
                                                                                                y_baseline = y[.x])))),
         num_opp_players_same_dist = ifelse(displayName == 'football', 0, num_opp_players_same_dist),
         min_dist_opp_index = map_dbl(.x=row_number(), ~which(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                            y = y[which(club[.x]!=club & club!='football')],
                                                                            x_baseline = x[.x],
                                                                            y_baseline = y[.x])== min(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                                    y = y[which(club[.x]!=club & club!='football')],
                                                                                                                    x_baseline = x[.x],
                                                                                                                    y_baseline = y[.x])))[1]),
         second_closest_dist_opp_player = map_dbl(.x=row_number(), ~Rfast::nth(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                             y = y[which(club[.x]!=club & club!='football')],
                                                                                             x_baseline = x[.x],
                                                                                             y_baseline = y[.x]), 2, descending = F)),
         second_closest_opp_index = map_dbl(.x=row_number(), ~which(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                  y = y[which(club[.x]!=club & club!='football')],
                                                                                  x_baseline = x[.x],
                                                                                  y_baseline = y[.x])== Rfast::nth(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                                                 y = y[which(club[.x]!=club & club!='football')],
                                                                                                                                 x_baseline = x[.x],
                                                                                                                                 y_baseline = y[.x]),2,descending = F))[1]) # Where I account for duplicates
  ) %>%
  ungroup()

# Justin's code is adjusted here to not include "football" (so 13 changes to 12, bracketed 12 changes to 11)
MergedData <- MergedData %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    closest_opp_player_name = case_when(
      row_number()<=11 ~ displayName[11+min_dist_opp_index],
      row_number()>=12 ~ displayName[min_dist_opp_index]
    ),
    closest_opp_player_nflID = case_when(
      row_number()<=11 ~ nflId[11+min_dist_opp_index],
      row_number()>=12 ~ nflId[min_dist_opp_index]
    ),
    second_closest_opp_player_name = case_when(
      row_number()<=11 ~ displayName[11+second_closest_opp_index],
      row_number()>=12 ~ displayName[second_closest_opp_index]
    ),
    second_closest_opp_player_nflID = case_when(
      row_number()<=11 ~ nflId[11+second_closest_opp_index],
      row_number()>=12 ~ nflId[second_closest_opp_index]
    )
  ) %>%
  ungroup()

## grabbing directions of first and second closest players on opposing teams:
MergedData <- MergedData %>%
  arrange(gameId, playId, frameId, club, nflId) %>%
  group_by(gameId, playId, frameId) %>%
  mutate(
    dir_of_closest_opp_player = case_when(
      row_number()<=11 ~ dir[11+min_dist_opp_index],
      row_number()>=12 ~ dir[min_dist_opp_index]
    ),
    dir_of_second_closest_opp_player = case_when(
      row_number()<=11 ~ dir[11+second_closest_opp_index],
      row_number()>=12 ~ dir[second_closest_opp_index]
    )
  ) %>%
  ungroup()

# Now, here's code that gives each player's directional speed and acceleration
# This example is from the df_tracking_A DF, defined in the code_2 GitHub folder
modeling_df <- data.table::fread('df_tracking_A.csv')

wagner <- modeling_df %>%
  filter(gameId==2022090800 &
           playId==692 &
           displayName=='Bobby Wagner')

# plotting Bobby Wagner's movements on this play
plotly::ggplotly(wagner %>%
  ggplot(aes(x = x,
             y = y,
             color = as.factor(t_after_snap))) +
  geom_point())

wagner <- wagner %>%
  mutate(x_vel_component = (s*cos((90-dir)*pi/180)),
         y_vel_component = (s*sin((90-dir)*pi/180)),
         x_acc_component = (a*cos((90-dir)*pi/180)),
         y_acc_component = (a*sin((90-dir)*pi/180)))

# However, here it is with the MergedData set, originally defined in data cleaning file
MergedData <- MergedData %>%
  mutate(x_vel_component = (s*cos((90-dir)*pi/180)),
         y_vel_component = (s*sin((90-dir)*pi/180)),
         x_acc_component = (a*cos((90-dir)*pi/180)),
         y_acc_component = (a*sin((90-dir)*pi/180)))

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

# With 1-high plays in mind, add Max_Y_AbsSpeed_AtSnap_AnySafety, Max_Y_AbsAcc_AtSnap_AnySafety, and Max_Y_AbsDistFromBall_AtSnapAnySafety
MergedData <- MergedData %>% mutate(Max_Y_AbsSpeed_AtSnap_AnySafety = pmax(abs(Safety1_y_vel_component_AtSnap), abs(Safety2_y_vel_component_AtSnap), na.rm = TRUE))
MergedData <- MergedData %>% mutate(Max_Y_AbsAcc_AtSnap_AnySafety = pmax(abs(Safety1_y_acc_component_AtSnap), abs(Safety2_y_acc_component_AtSnap), na.rm = TRUE))
MergedData <- MergedData %>% mutate(Max_Y_AbsDistFromBall_AtSnap_AnySafety = pmax(Safety1_Y_AbsDistFromBall_AtSnap, Safety2_Y_AbsDistFromBall_AtSnap, na.rm = TRUE))

# AND don't forget difference in the absolute distances from MOF between the safeties at the snap
# In other words, if one safety is 2 yards to his left of the ball, and other is 12 yards to his right, that's 10, not 14
# So a very big number indicates that one safety is very near MOF, the other is not
# In contrast, a small number means they're roughly equally far from the ball, in their respective directions
MergedData <- MergedData %>% mutate(Y_AbsDistFromBall_Diff_BetweenSafeties_AtSnap = abs(Safety1_Y_AbsDistFromBall_AtSnap - Safety2_Y_AbsDistFromBall_AtSnap))

# Keep minSafetyDistToMOF in mind too (part of df_safety_movement_1 and 2 DFs)

# And create a data table for just dropbacks as well
Dropbacks_Merged <- MergedData %>% filter(isDropback == 1)
Dropbacks_Merged <- Dropbacks_Merged %>% select(-"isDropback")
setDT(Dropbacks_Merged)
setkey(Dropbacks_Merged, gameId, playId, nflId, frameId)
Dropbacks_Merged <- Dropbacks_Merged %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

Dropbacks_2High_PreSnap <- Dropbacks_Merged %>% filter(num_safeties_pre_snap == 2)
Dropbacks_2High_PreSnap <- Dropbacks_2High_PreSnap %>% select(-"num_safeties_pre_snap")

# If needed, here's how to export to a CSV
write.csv(MergedData, "MergedData.csv")
write.csv(Dropbacks_Merged, "Dropbacks_Merged.csv")

