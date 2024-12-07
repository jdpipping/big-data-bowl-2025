
### SET YOUR WORKING DIRECTORY TO THIS FILE (IN `code_2`)

#################
### LIBRARIES ###
#################

library(tidyverse)
library(stringr)

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
    file_path = paste0('../raw-data/tracking_week_', week, '.csv') 
    # read week data in
    tracking_week <- read_csv(file_path, show_col_types = F)
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

####################
### READ IN DATA ###
####################

# games
games = read_csv('../raw-data/games.csv', show_col_types = F)
games
# plays
plays = read_csv('../raw-data/plays.csv', show_col_types = F)
plays
# players
players = read_csv('../raw-data/players.csv', show_col_types = F)
players

# tracking data
WEEKS = 1:9 ### forreal dogg
# WEEKS = 1:2 ## fo-shizzle just to make things faster & not crash me R
weeks_str = paste0("_w",min(WEEKS),"-",max(WEEKS))
filename = paste0("../processed-data/tracking_std",weeks_str,".csv")
filename
file_exists = file.exists(filename)
file_exists
if (file_exists) {
  # read cleaned data
  # bewareth: takes for-fucking-ever to read (5-ish min)
  tracking = read_csv(filename, show_col_types = F)
} else {
  # read in data
  tracking_all = read_tracking(weeks = WEEKS)
  # standardize tracking data
  tracking = standardize_tracking(tracking_all)
  # ehhh not really feeling saving 10 GB to my drive
  # # save to file
  # write_csv(tracking, filename)
}
tracking
dim(tracking)
names(tracking)
object.size(tracking)/10**9 ### gigabytes

######################
### AUXILIARY DATA ###
######################

# player position dataframe (for sanity checking)
players_pos = players |> distinct(nflId, displayName, position) |>
  arrange(position, displayName) |> rename(pos_official = position)
players_pos

# get time of snap for each play
snap_frames = tracking |> 
  group_by(playId) |> 
  filter(frameType == 'SNAP') |> 
  # omit duplicates (players)
  distinct(gameId, playId, frameId) |> # LOOK AT THIS
  # rename frameId
  rename(snap_frame = frameId) |>
  ungroup()
snap_frames

# define line of scrimmage
los = tracking |> 
  # select needed columns
  select(gameId, playId, displayName, frameType, los = x) |> 
  # filter for snap
  filter(frameType == 'SNAP', displayName == 'football') |> 
  # drop unneded columns
  select(-displayName, -frameType)
los

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
  )
play_info

###################
### RYAN'S DATA ###
###################

# make adjusted tracking
tracking_adj = tracking |> 
  # merge in snap frames
  left_join(snap_frames, by = c('gameId', 'playId')) |> 
  # standardize all frames to snap, in the units of seconds
  mutate(t_after_snap = (frameId - snap_frame)/10) |> 
  relocate(t_after_snap, .after = playId) |>
  # drop redundant columns
  select(-frameId, -frameType, -time, -snap_frame,
         -jerseyNumber) |>
  # add each player's general position
  left_join(players_pos) |>
  relocate(pos_official, .after = displayName)
tracking_adj
names(tracking_adj)

### for each play, get each player's post-snap position, say, t_postsnap = 2.5 seconds after the snap
t_postsnap = 2.5
df_postsnap_locs = 
  tracking_adj %>%
  group_by(gameId, playId, nflId) %>%
  filter(t_after_snap == t_postsnap) %>%
  ungroup() %>%
  select(gameId, playId, nflId, x_postsnap = x, y_postsnap = y) %>%
  drop_na()
df_postsnap_locs

# define pre-snap safety from tracking data
pre_snap_safeties_0 = tracking_adj |> 
  # filter frames between (0, 5] seconds
  filter(t_after_snap < 0 & t_after_snap >= -5) |> 
  # add some indicies
  group_by(gameId, playId) |> 
  mutate(i = cur_group_id()) |>
  relocate(i, .after = playId) |>
  ungroup() |>
  # join with line of scrimmage
  left_join(los, by = c('gameId', 'playId')) |> 
  # indicator for beyond 10 yards deep
  mutate(beyond_10 = if_else(x >= los + 10, T, F)) 
pre_snap_safeties_0

pre_snap_safeties = 
  pre_snap_safeties_0 |>
  # define safety on play
  group_by(gameId, playId, i, nflId, pos_official) |> 
  reframe(is_pre_safety = any(beyond_10, na.rm = T)) |> 
  # count number of safeties
  group_by(gameId, playId) |> 
  mutate(num_safeties = sum(is_pre_safety, na.rm = T)) |> 
  ungroup() %>%
  select(-i)
pre_snap_safeties

# check
pre_snap_safeties_per_play = pre_snap_safeties %>% distinct(gameId, playId, num_safeties)
pre_snap_safeties_per_play
table(pre_snap_safeties_per_play$num_safeties)

# plays with one or two safeties
plays_with_1or2_safeties = 
  pre_snap_safeties |>
  distinct(gameId, playId, num_safeties) |>
  filter(num_safeties %in% c(1,2)) |> select(-num_safeties)
plays_with_1or2_safeties
nrow(plays_with_1or2_safeties)

# non run plays 
plays_pass = 
  plays |>
  select(gameId, playId, pff_runConceptPrimary, pff_runConceptSecondary, 
         pff_runPassOption, dropbackType, pff_passCoverage) |>
  # remove run plays
  filter(is.na(pff_runConceptPrimary) & is.na(pff_runConceptSecondary) & pff_runPassOption == 0) |>
  distinct(gameId, playId)
plays_pass
nrow(plays_pass)

# combine tracking data with safety info 
df_tracking_1 = 
  play_info |> 
  # select relevant columns (can change)
  select(gameId, playId, defensiveTeam, mofo) |>
  # keep only the plays that have 1 or 2 safeties and aren't run plays
  inner_join(plays_with_1or2_safeties) |>
  inner_join(plays_pass) |>
  # merge to tracking data
  left_join(tracking_adj) |> 
  relocate(t_after_snap, .after = playId) |>
  rename(mofo_postsnap = mofo) |>
  mutate(mofo_postsnap = as.numeric(mofo_postsnap)) |>
  # merge safety indicators
  left_join(pre_snap_safeties) |>
  relocate(is_pre_safety, .after = pos_official) |>
  relocate(num_safeties, .after = is_pre_safety) |>
  select(-c(club, playDirection, dis, o, event)) |>
  # get post-snap locations
  left_join(df_postsnap_locs) |>
  relocate(mofo_postsnap, .after = y_postsnap) 
df_tracking_1
# View(df_tracking_1[1:1000,])
# quick bug check: na's arising from missing snap indicator on some plays
# summary(df_tracking_1)

# safety quality check
#FIXME 
df_safety_quality_check = 
  df_tracking_1 %>%
  filter(pos_official %in% c("SS","FS") | is_pre_safety) %>%
  distinct(gameId, playId, nflId, displayName, pos_official, is_pre_safety)
df_safety_quality_check
# examine the players we assigned to pre-snap-safety who aren't officially safeties
table(
  (df_safety_quality_check %>% filter(is_pre_safety))$pos_official
)
# examine the official safeties who we did not assign to pre-snap-safety
table(
  (df_safety_quality_check %>% filter(pos_official %in% c("SS","FS")))$is_pre_safety
)
# for now, I'll just go with our tracking data definition! Better to be conservative here

# tracking data, but just the safeties, and only the frames in the first, say, 7 seconds before the snap
t_pre_snap_max = 7
df_tracking_2 = 
  df_tracking_1 |>
  # keep just the pre-snap safeties (for now)
  filter(is_pre_safety) |>
  select(-c(pos_official, is_pre_safety)) |>
  # filter frames between (0, 7] seconds
  filter(-t_pre_snap_max <= t_after_snap & t_after_snap < 0) 
df_tracking_2
nrow(df_tracking_2 %>% distinct(gameId, playId))
# View(df_tracking_2[1:2000,])
table(df_tracking_2$t_after_snap)
table(df_tracking_2$mofo_postsnap)

# final tracking dataset to save
df_tracking_F = df_tracking_2
write_csv(df_tracking_F, paste0('../processed-data/df_ryan_modeling_1',weeks_str,'.csv'))


