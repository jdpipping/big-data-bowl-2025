#################
### LIBRARIES ###
#################

# install.packages('stringr')
library(stringr)
# install.packages('tidyverse')
library(tidyverse)

####################
### READ IN DATA ###
####################

# games
games = read_csv('raw-data/games.csv')
# players
players = read_csv('raw-data/players.csv')
# plays
plays = read_csv('raw-data/plays.csv')
# individual player roles
player_play = read_csv('raw-data/player_play.csv')
# tracking data
tracking = read_csv('processed-data/tracking_std.csv')

###################
### CREATING V1 ###
###################

# define mofo / mofc
# NOTE: If using the MergedData data table established in the data-cleaning GitHub file, this already exists as PostSnap_MOF
mofo = c("Cover-0", "Cover-2", "Quarters", "Cover-4", "2-Man", "Cover-6", "Cover-6 Left", "Cover-6 Right")
mofc = c("Cover-1", "Cover-1 Double", "Cover-3", "Cover-3 Cloud Left", "Cover-3 Cloud Right", "Cover-3 Double Cloud", "Cover-3 Seam")

# play info data
# NOTE: If using the MergedData data table established in the data-cleaning GitHub file, much of this data already exists (e.g. quarter_seconds_remaining instead of time_left_quarter)
play_info = games |> 
  # omit unnecessary game info
  select(-season, -week, -gameDate, -gameTimeEastern) |> 
  # join with plays data
  left_join(plays, by = 'gameId') |> 
  # filter for 1st/10+, 2nd/5+
  filter((down == 1 & yardsToGo >= 10) | (down == 2 & yardsToGo >= 5)) |> 
  # add features
  mutate(
    # mofo
    mofo = case_when(pff_passCoverage %in% mofo ~ T,
                     pff_passCoverage %in% mofc ~ F),
    # number of receivers on play
    num_receivers = as.numeric(gsub('x', '', receiverAlignment)) %% 10 + floor(as.numeric(gsub('x', '', receiverAlignment)) / 10),
    # pre-play score differential (w.r.t. offense)
    pre_score_diff = if_else(possessionTeam == homeTeamAbbr, preSnapHomeScore - preSnapVisitorScore, preSnapVisitorScore - preSnapHomeScore),
    # time remaining in quarter (seconds)
    time_left_quarter = as.numeric(str_sub(gameClock, 1, 2)) * 60 + as.numeric(str_sub(gameClock, 4, 5)),
    # time remaining in game (seconds)
    time_left_game = if_else(quarter == 5, time_left_quarter, (4 - quarter) * 15 * 60 + time_left_quarter)
    )

# GSIS position data
gsis_positions = players |>
  # filter for assigned positions
  select(nflId, gsis_position = position)

# ids of qbs on each play
qb_ids_plays = tracking |> 
  # join with GSIS positions
  left_join(gsis_positions, by = 'nflId') |> 
  # filter for qbs
  filter(gsis_position == 'QB') |> 
  # select needed columns
  select(gameId, playId, nflId) |> 
  # get distinct qb ids
  distinct() |>
  # group by game and play
  group_by(gameId, playId) |>
  # sort by nfl id
  arrange(nflId) |>
  # define qb id's
  mutate(qb_id = row_number()) |>
  # pivot to wide
  pivot_wider(names_from = qb_id, values_from = nflId, names_prefix = 'qb_')
  
# define line of scrimmage
los = tracking |> 
  # select needed columns
  select(gameId, playId, displayName, frameType, los = x) |> 
  # filter for snap
  filter(frameType == 'SNAP', displayName == 'football') |> 
  # drop unneded columns
  select(-displayName, -frameType)

# pre-snap safety
pre_snap_safety = tracking %>% 
  # filter so that we only have frames before snap
  filter(frameType != 'AFTER_SNAP') %>% 
  # join with line of scrimmage
  left_join(los, by = c('gameId', 'playId')) %>% 
  # indicator for beyond 10 yards deep
  mutate(beyond_10 = if_else(x >= los + 10, T, F)) %>% 
  # group by game, play, player
  group_by(gameId, playId, nflId) %>% 
  # define pre-snap safety
  summarise(safety = any(beyond_10, na.rm = T)) %>% 
  # ungroup players
  ungroup(nflId) %>% 
  # count number of safeties
  mutate(num_safeties_on_play = sum(safety)) %>% 
  # ungroup all columns
  ungroup()

# ids of pre-snap safeties on each play
safety_ids_pre_snap = pre_snap_safety %>% 
  # filter out non-safeties
  filter(safety) %>% 
  # drop safety column
  select(-safety) %>%
  # group by game and play
  group_by(gameId, playId) %>% 
  # sort by nfl id
  arrange(nflId) %>% 
  # define safety id's
  mutate(safety_id = row_number()) %>%
  # pivot to wide
  pivot_wider(names_from = safety_id, values_from = nflId, names_prefix = 'pre_snap_safety_')

# increase max vector size to 64 GB
mem.maxVSize(vsize = 49152 * 1.5)

# Now, create a variable for whether a player had safety responsibilities on the play
# Even if that person wasn't aligned as a safety before the snap
# Do it for both MergedData (from data-cleaning file) and for player_play CSV directly
MergedData <- MergedData %>% mutate(post_snap_safety =
   ifelse(!is.na(pff_defensiveCoverageAssignment) & pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), TRUE,
          ifelse(!is.na(pff_defensiveCoverageAssignment) & !pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), FALSE, 
                 ifelse(is.na(pff_defensiveCoverageAssignment) & PlayerSideOfBall %in% "defense", FALSE, NA))))

player_play <- player_play %>% mutate(post_snap_safety =
   ifelse(!is.na(pff_defensiveCoverageAssignment) & pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), TRUE,
          ifelse(!is.na(pff_defensiveCoverageAssignment) & !pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), FALSE, 
                 ifelse(is.na(pff_defensiveCoverageAssignment) & PlayerSideOfBall %in% "defense", FALSE, NA))))

# ids of post-snap safeties on each play
safety_ids_post_snap = player_play %>% 
  # filter out non-safeties
  filter(post_snap_safety == TRUE) %>% 
  # drop safety column
  select(-"post_snap_safety") %>%
  # group by game and play
  group_by(gameId, playId) %>% 
  # sort by nfl id
  arrange(nflId) %>% 
  # define safety id's
  mutate(safety_id = row_number()) %>%
  # pivot to wide
  pivot_wider(names_from = safety_id, values_from = nflId, names_prefix = 'post_snap_safety_')
   
# merge to create v1, which is a frame-by-frame data set rather than play-by-play
v1 = play_info |> 
  # join tracking
  left_join(tracking, by = c('gameId', 'playId')) |> 
  # join qb ids
  left_join(qb_ids_plays, by = c('gameId', 'playId')) |>
  # join pre-snap safety ids
  left_join(safety_ids_pre_snap, by = c('gameId', 'playId'))
# join post-snap safety ids
  left_join(safety_ids_post_snap, by = c('gameId', 'playId'))
# save to file
write_csv(v1, 'processed-data/v1.csv')

# wipe r memory
rm(list = ls())
