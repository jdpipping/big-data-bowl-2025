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
# tracking data
tracking = read_csv('processed-data/tracking_std.csv')

####################
### V1 (DEFUNCT) ###
####################

# define mofo / mofc
mofo = c("Cover-0", "Cover-2", "Quarters", "Cover-4", "2-Man", "Cover-6", "Cover-6 Left", "Cover-6 Right")
mofc = c("Cover-1", "Cover-1 Double", "Cover-3", "Cover-3 Cloud Left", "Cover-3 Cloud Right", "Cover-3 Double Cloud", "Cover-3 Seam")

# play info data
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
  ) |> 
  # arrange by gameId, playId
  arrange(gameId, playId)

# pff position data
pff_positions = players |>
  # filter for assigned positions
  select(nflId, pff_position = position)

# ids of qbs on each play
qbs = tracking |> 
  # join with pff positions
  left_join(pff_positions, by = 'nflId') |> 
  # filter for qbs
  filter(pff_position == 'QB') |> 
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
  select(gameId, playId, displayName, frameType, x) |> 
  # filter for snap
  filter(frameType == 'SNAP', displayName == 'football') |> 
  # rename x as los
  rename(los = x) |> 
  # drop unneded columns
  select(-displayName, -frameType)

# # safety on play
# safety_on_play = tracking |> 
#   # filter before snap
#   filter(frameType != 'AFTER_SNAP') |> 
#   # join with line of scrimmage
#   left_join(los, by = c('gameId', 'playId')) |> 
#   # indicator for beyond 10 yards deep
#   mutate(beyond_10 = if_else(x >= los + 10, T, F)) |> 
#   # group by game, play, player
#   group_by(gameId, playId, nflId) |> 
#   # define safety on play
#   summarise(safety = any(beyond_10, na.rm = T)) |> 
#   # ungroup players
#   ungroup(nflId) |> 
#   # count number of safeties
#   mutate(num_safeties = sum(safety)) |> 
#   # ungroup all columns
#   ungroup()

# # ids of safeties on each play
# safety_ids_plays = safety_on_play |> 
#   # filter out non-safeties
#   filter(safety) |> 
#   # drop safety column
#   select(-safety) |>
#   # group by game and play
#   group_by(gameId, playId) |> 
#   # sort by nfl id
#   arrange(nflId) |> 
#   # define safety id's
#   mutate(safety_id = row_number()) |>
#   # pivot to wide
#   pivot_wider(names_from = safety_id, values_from = nflId, names_prefix = 'safety_')

# # increase max vector size to 64 GB
# mem.maxVSize(vsize = 49152 * 1.5)
# 
# # merge to create v1
# v1 = play_info |> 
#   # join tracking
#   left_join(tracking, by = c('gameId', 'playId')) |> 
#   # join qb ids
#   left_join(qb_ids_plays, by = c('gameId', 'playId')) |>
#   # join safety ids
#   left_join(safety_ids_plays, by = c('gameId', 'playId'))
# # save to file
# write_csv(v1, 'processed-data/v1.csv')
# 
# # wipe r memory
# rm(list = ls())

###################
### RYAN'S DATA ###
###################

# get time of snap for each play
snap_frames = tracking |> 
  group_by(playId) |> 
  filter(event == 'ball_snap') |> 
  # omit duplicates (players)
  distinct(gameId, playId, frameId) |> # LOOK AT THIS
  # rename frameId
  rename(snap_frame = frameId)

# make adjusted tracking
tracking_adj = tracking |> 
  # merge in snap frames
  left_join(snap_frames, by = c('gameId', 'playId')) |> 
  # standardize all frames to snap
  mutate(frame_adj = frameId - snap_frame) |> 
  # drop redundant columns
  select(-frameId, -frameType, -time, -snap_frame)

# define pre-snap safety
pre_snap_safeties = tracking_adj |> 
  # filter frames between (0, 5] seconds
  filter(frame_adj < 0 & frame_adj >= -50) |> 
  # join with line of scrimmage
  left_join(los, by = c('gameId', 'playId')) |> 
  # indicator for beyond 10 yards deep
  mutate(beyond_10 = if_else(x >= los + 10, T, F)) |> 
  # group by game, play, player
  group_by(gameId, playId, nflId) |> 
  # define safety on play
  summarise(is_pre_safety = any(beyond_10, na.rm = T)) |> 
  # ungroup players
  ungroup(nflId) |> 
  # count number of safeties
  mutate(num_safeties = sum(is_pre_safety, na.rm = T)) |> 
  # ungroup all columns
  ungroup() #|> 
  # # filter out non-safeties
  # filter(is_pre_safety) |>
  # # drop safety column
  # select(-is_pre_safety) |>
  # # group by game and play
  # group_by(gameId, playId) |> 
  # # sort by nfl id
  # arrange(nflId) |> 
  # # define safety id's
  # mutate(pre_snap_safety = row_number()) |>
  # # pivot to wide
  # pivot_wider(names_from = pre_snap_safety, values_from = nflId, names_prefix = 'safety_')

# create brill
brill = play_info |> 
  # select relevant columns (can change)
  select(gameId, playId, defensiveTeam, mofo) |> 
  # merge to tracking data
  left_join(tracking_adj, by = c('gameId', 'playId')) |> 
  # merge qb id's, define is_qb, drop redundancy
  left_join(qbs, by = c('gameId', 'playId')) |> 
  mutate(is_qb = (nflId %in% na.omit(c(qb_1, qb_2)))) |> 
  select(-qb_1, -qb_2) |> 
  # merge safety indicators
  left_join(pre_snap_safeties, by = c('gameId', 'playId', 'nflId')) |> 
  # add in pff positions
  left_join(pff_positions, by = 'nflId')
# quick bug check: na's arising from missing snap indicator on some plays
summary(brill)
# write brill.csv
write_csv('processed-data/brill.csv')