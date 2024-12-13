setwd("C:/Users/justi/OneDrive/Penn/BDB(2025)")
library(tidyverse)

#includes information on each game:
games <- read_csv('games.csv')

#includes information on all of the plays:
plays <- read_csv("plays.csv")

#contains player-level information from players that participated in any of the tracking data files.
players <- read_csv('players.csv')

# file contains player-level stats for each game and play
player_play_data<- read_csv("player_play.csv")

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

#vectorized bit of code:
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

# Justin's code is adjusted here to not include "football" (so 13 changes to 12, etc.)
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

#plotting Bobby Wagner's movements on this play
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

