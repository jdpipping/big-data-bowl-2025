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
  mutate(min_dist = map_dbl(.x=row_number(), ~min(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                y = y[which(club[.x]!=club & club!='football')],
                                                                x_baseline = x[.x],
                                                                y_baseline = y[.x]))),
         num_same_dist = map_dbl(.x=row_number(), ~11 - length(unique(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                    y = y[which(club[.x]!=club & club!='football')],
                                                                                    x_baseline = x[.x],
                                                                                    y_baseline = y[.x])))),
         num_same_dist=ifelse(displayName=='football', NA, num_same_dist),
         min_dist_pos = map_dbl(.x=row_number(), ~which(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                      y = y[which(club[.x]!=club & club!='football')],
                                                                      x_baseline = x[.x],
                                                                      y_baseline = y[.x])== min(calc_distance(x = x[which(club[.x]!=club & club!='football')],
                                                                                                                  y = y[which(club[.x]!=club & club!='football')],
                                                                                                                  x_baseline = x[.x],
                                                                                                                  y_baseline = y[.x])))[1])) %>%
  ungroup()
