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

# Also mutate variables for the #1 receivers for the offense (i.e. the furthest outside) at the time of snap, using MergedData (defined in data cleaning file)
# Recall that Y_NetDistFromBall_Rank_BySide exists already, so whoever ranks 1 and 11 there are the closest to sideline
# And we established that going to offense's left is always positive Y, but the rank goes by SMALLEST distance
# In other words, the person with rank 1 (on offense) is the RIGHT WR, b/c that'll have the smallest/most negative distance
# Whereas if we were using Y_AbsDistFromBall_Rank_BySide, then the person with rank 1 would be closest to the ball, i.e. the center
# Likewise, on defense, the person with rank 1 would be the defense's left CB (i.e. offense's right)
LeftMost_Receivers <- MergedData %>% 
  filter(PlayerSideOfBall == "offense", Y_NetDistFromBall_Rank_BySide == 11, event %in% c("ball_snap", "snap_direct")) 
LeftMost_Receivers <- LeftMost_Receivers %>% select("gameId", "playId", "nflId", "displayName", "x", "y")
LeftMost_Receivers <- LeftMost_Receivers %>% rename(LeftMost_Receiver_ID = `nflId`,
                                                    LeftMost_Receiver_Name = `displayName`,
                                                    LeftMost_Receiver_X_AtSnap = `x`, LeftMost_Receiver_Y_AtSnap = 'y')
MergedData <- MergedData %>% left_join(LeftMost_Receivers, by = c("gameId", "playId"))

RightMost_Receivers <- MergedData %>% 
  filter(PlayerSideOfBall == "offense", Y_NetDistFromBall_Rank_BySide == 1, event %in% c("ball_snap", "snap_direct")) 
RightMost_Receivers <- RightMost_Receivers %>% select("gameId", "playId", "nflId", "displayName", "x", "y")
RightMost_Receivers <- RightMost_Receivers %>% rename(RightMost_Receiver_ID = `nflId`,
                                                      RightMost_Receiver_Name = `displayName`,
                                                      RightMost_Receiver_X_AtSnap = `x`, RightMost_Receiver_Y_AtSnap = 'y')
MergedData <- MergedData %>% left_join(RightMost_Receivers, by = c("gameId", "playId"))

LeftMost_Defenders <- MergedData %>% 
  filter(PlayerSideOfBall == "defense", Y_NetDistFromBall_Rank_BySide == 1, event %in% c("ball_snap", "snap_direct")) 
LeftMost_Defenders <- LeftMost_Defenders %>% select("gameId", "playId", "nflId", "displayName", "x", "y")
LeftMost_Defenders <- LeftMost_Defenders %>% rename(LeftMost_Defender_ID = `nflId`,
                                                    LeftMost_Defender_Name = `displayName`,
                                                    LeftMost_Defender_X_AtSnap = `x`, LeftMost_Defender_Y_AtSnap = 'y')
MergedData <- MergedData %>% left_join(LeftMost_Defenders, by = c("gameId", "playId"))

RightMost_Defenders <- MergedData %>% 
  filter(PlayerSideOfBall == "defense", Y_NetDistFromBall_Rank_BySide == 11, event %in% c("ball_snap", "snap_direct")) 
RightMost_Defenders <- RightMost_Defenders %>% select("gameId", "playId", "nflId", "displayName", "x", "y")
RightMost_Defenders <- RightMost_Defenders %>% rename(RightMost_Defender_ID = `nflId`,
                                                      RightMost_Defender_Name = `displayName`,
                                                      RightMost_Defender_X_AtSnap = `x`, RightMost_Defender_Y_AtSnap = 'y')
MergedData <- MergedData %>% left_join(RightMost_Defenders, by = c("gameId", "playId"))

setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")
rm(LeftMost_Receivers, RightMost_Receivers, LeftMost_Defenders, RightMost_Defenders)

# Here is code for determining a pre-snap safety, using previously defined MergedData table
# Recall that we've already eliminated all frames before the "line_set" event of each play
pre_snap_safety <- MergedData %>% filter(frameType != 'AFTER_SNAP') %>%
  # indicator for whether the player was a pre-snap safety, based on location
  mutate(safety_location = if_else(x >= Ball_X_Snap + 8 & PlayerSideOfBall == "defense" & displayName != RightMost_Defender_Name & displayName != LeftMost_Defender_Name, TRUE, FALSE)) %>% 
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

# To briefly check data, see the maximum number of pre-snap safeties using this method
# View(MergedData %>% filter(frameId == 1) %>% arrange(desc(num_safeties_pre_snap)))
# Example of play that has a lot of pre-snap safeties show up: View(MergedData %>% filter(gameId == 2022091105, playId == 4308))
# 3rd-and-22, makes sense 

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

# increase max vector size to 64 GB
mem.maxVSize(vsize = 49152 * 1.5)

# Some plays have multiple players listed at QB, such as Taysom Hill plays: View(MergedData %>% filter(gameId == 2022091100, playId == 301, PlayerSideOfBall == "offense", frameId == 1))
# To account for those, look into whichever QB was closest horizontally (i.e. Y coordinate) to the ball during snap
# Here's how to account for that using the previously defined MergedData data table (from the data cleaning file)
QBs_AtSnap <- MergedData %>% filter(position %in% "QB", event %in% c("ball_snap", "snap_direct")) %>%
  group_by(gameId, playId) %>%
  mutate(QB_DistFromBall_Rank_AtSnap = rank(Y_AbsDistFromBall, ties.method = "first")) %>%
  ungroup()
QBs_AtSnap <- QBs_AtSnap %>% 
  select(c("gameId", "playId", "nflId", "displayName", "QB_DistFromBall_Rank_AtSnap"))
MultiQB_Plays <- QBs_AtSnap %>% 
  group_by(gameId, playId) %>% 
  summarize(n = n()) %>% arrange(desc(n))
MergedData <- MergedData %>%
  left_join(QBs_AtSnap, by = c("gameId", "playId", "nflId", "displayName"))

table(MergedData$position)
# For any "QB_DistFromBall_Rank_AtSnap" bigger than 1, change the position name to RB
# This isn't always exactly right, e.g. Taysom could be lined up wide instead of RB, but the point is to avoid multiple QBs
MergedData <- MergedData %>% mutate(position =
                                      ifelse(is.na(QB_DistFromBall_Rank_AtSnap), position,
                                             ifelse(!is.na(QB_DistFromBall_Rank_AtSnap) & QB_DistFromBall_Rank_AtSnap > 1 & position == "QB", "RB", position)))

rm(QBs_AtSnap, MultiQB_Plays)
MergedData <- MergedData %>% select(-"QB_DistFromBall_Rank_AtSnap")
# And use arrange() again, but with the data table format
setDT(MergedData)
setkey(MergedData, gameId, playId, nflId, frameId)
MergedData <- MergedData %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")

# Quick test that there are no plays left with multiple
QB_Multiples <- MergedData %>% filter(position %in% "QB", frameId == 1) %>%
  group_by(gameId, playId) %>%
  summarize(n = n()) %>% arrange(desc(n))
rm(QB_Multiples)

# Now, create a variable for whether a player had safety responsibilities on the play
# Even if that person wasn't aligned as a safety before the snap
# Do it for both MergedData (from data-cleaning file) and for player_play CSV directly
MergedData <- MergedData %>% mutate(is_post_snap_safety =
                                      ifelse(!is.na(pff_defensiveCoverageAssignment) & pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), TRUE,
                                             ifelse(!is.na(pff_defensiveCoverageAssignment) & !pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), FALSE, 
                                                    ifelse(is.na(pff_defensiveCoverageAssignment) & PlayerSideOfBall %in% "defense", FALSE, NA))))

player_play <- player_play %>% mutate(is_post_snap_safety =
                                        ifelse(!is.na(pff_defensiveCoverageAssignment) & pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), TRUE,
                                               ifelse(!is.na(pff_defensiveCoverageAssignment) & !pff_defensiveCoverageAssignment %in% c("2R", "2L", "3M", "4IL", "4IR", "DF", "PRE"), FALSE, 
                                                      ifelse(is.na(pff_defensiveCoverageAssignment) & PlayerSideOfBall %in% "defense", FALSE, NA))))

# Out of curiosity, glance at whether coverage assignments are assigned by PFF on designed runs
DesignedRuns_Merged <- MergedData %>% filter(isDropback == 0)
table(DesignedRuns_Merged$pff_defensiveCoverageAssignment)
# View(DesignedRuns_Merged %>% filter(PlayerSideOfBall == "defense" & is.na(pff_defensiveCoverageAssignment)))
# They typically are NAs, with some exceptions on RPOs that are handed off
rm(DesignedRuns_Merged)

# Repeat the safety diagnosing process with post-snap safeties
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

# Check to make sure we have FALSE values ... any defensive players who aren't safeties should have FALSE
# But any offensive players should have NA
table(MergedData$is_pre_snap_safety)
table(MergedData$is_post_snap_safety)
MergedData <- MergedData %>% mutate(is_pre_snap_safety = ifelse(PlayerSideOfBall == "offense", NA, is_pre_snap_safety))
MergedData <- MergedData %>% mutate(is_post_snap_safety = ifelse(PlayerSideOfBall == "offense", NA, is_post_snap_safety))

# merge to create v1, which is a frame-by-frame data set rather than play-by-play
v1 = MergedData|> 
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
