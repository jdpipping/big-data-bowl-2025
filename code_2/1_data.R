
#############
### BEGIN ###
#############

### SET YOUR WORKING DIRECTORY TO THIS FILE (IN `code_2`)
library(tidyverse)
library(stringr)
library(nflfastR)

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

# output filename as a function of the week number
get_filename <- function(week) {
  prefix = "../processed-data/df_tracking_A"
  if (is.na(week)) {
    paste0(prefix,".csv")
  } else {
    paste0(prefix,"_week",week,".csv")
  }
}

#################################
### READ IN NON-TRACKING DATA ###
#################################

# games
games = read_csv('../raw-data/games.csv', show_col_types = F)
games
# plays
plays = read_csv('../raw-data/plays.csv', show_col_types = F)
plays
# players
players = read_csv('../raw-data/players.csv', show_col_types = F)
players
# player position dataframe (for sanity checking)
players_pos = 
  players |> 
  distinct(nflId, displayName, position) |>
  arrange(position, displayName) |> 
  rename(pos_official = position) 
players_pos
# offensive & defensive positions
table(players_pos$pos_official) 
length(table(players_pos$pos_official)) # num positions
defensive_positions = c(
  "CB", "DB", "DE", "DT","FS","ILB","LB","MLB","OLB","SS"
)
defensive_positions
offensive_positions = c(
  "C", "FB", "G", "NT", "QB", "RB", "T", "TE", "WR"
)
offensive_positions
players_pos = 
  players_pos |> 
  mutate(
    posGroup = ifelse(pos_official %in% defensive_positions, "defense",
                      ifelse(pos_official %in% offensive_positions, "offense", NA)),
  )
# define mofo / mofc
mofo = c("Cover-0", "Cover-2", "Quarters", "Cover-4", "2-Man", "Cover-6", "Cover-6 Left", "Cover-6 Right")
mofc = c("Cover-1", "Cover-1 Double", "Cover-3", "Cover-3 Cloud Left", "Cover-3 Cloud Right", "Cover-3 Double Cloud", "Cover-3 Seam")

# play info data
play_info_0 = games |> 
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
    # WP
    winProbability = ifelse(homeTeamAbbr == possessionTeam, preSnapHomeTeamWinProbability, preSnapVisitorTeamWinProbability),
    # number of receivers on play
    num_receivers = as.numeric(gsub('x', '', receiverAlignment)) %% 10 + floor(as.numeric(gsub('x', '', receiverAlignment)) / 10),
    # pre-play score differential (w.r.t. offense)
    pre_score_diff = if_else(possessionTeam == homeTeamAbbr, preSnapHomeScore - preSnapVisitorScore, preSnapVisitorScore - preSnapHomeScore),
    # time remaining in quarter (seconds)
    time_left_quarter = as.numeric(str_sub(gameClock, 1, 2)) * 60 + as.numeric(str_sub(gameClock, 4, 5)),
    # time remaining in game (seconds)
    time_left_game = if_else(quarter == 5, time_left_quarter, (4 - quarter) * 15 * 60 + time_left_quarter),
    # time remaining in half (seconds)
    time_left_half = if_else(quarter %in% c(1,2), time_left_game - 1800, time_left_game),
  )
play_info_0
# check mofo
temp = play_info_0 %>% select(gameId, playId,pff_passCoverage, mofo)
temp
sum(is.na(temp))
temp1 = temp %>% filter(is.na(mofo))
table(temp1$pff_passCoverage) ### we'll remove these plays later

# non run plays 
plays_dropbacks = 
  plays |>
  select(gameId, playId, isDropback, dropbackType, pff_passCoverage) |>
  # remove run plays
  filter(isDropback) |>
  distinct(gameId, playId)
plays_dropbacks
nrow(plays_dropbacks)

#####################
### NFLFASTR DATA ###
#####################

# nflfastr data
nflverse_pbp <- nflfastR::load_pbp(2022)
nflverse_pbp <- nflverse_pbp %>% filter(week %in% 1:9)
names(nflverse_pbp)
nflverse_pbp_1 = 
  nflverse_pbp %>%
  # select relevant variables
  mutate(playId = as.numeric(play_id), gameId = as.numeric(old_game_id)) %>%
  select(playId, gameId, xpass)
nflverse_pbp_1

# update play info
play_info = play_info_0 %>% left_join(nflverse_pbp_1)
dim(play_info_0)
dim(play_info)

###########################
### LOOP OVER THE WEEKS ###
###########################

# do analysis separately for each week
WEEKS = 1:9
for (week in WEEKS) {
  print(paste0("data time for WEEK ", week, " !!!"))
  filename = get_filename(week)
  filename
  
  #############################
  ### READ IN TRACKING DATA ###
  #############################
  
  # read in data
  tracking_og = read_tracking(weeks = week)
  # standardize tracking data
  tracking = standardize_tracking(tracking_og)
  # examine tracking data
  tracking
  dim(tracking)
  names(tracking)
  object.size(tracking)/10**9 ### gigabytes
  
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
  
  # time of line set
  time_of_line_set = tracking |>
    group_by(gameId, playId) %>%
    filter(event == 'line_set') |>
    distinct(gameId, playId, frameId_of_line_set = frameId) |>
    ungroup()
  time_of_line_set

  # time of end of play
   time_of_end_of_play = tracking |>
    group_by(gameId, playId) %>%
    filter(event %in% c("out_of_bounds", "safety", "pass_outcome_incomplete", "qb_sack", "qb_slide", "tackle", "touchdown", "fumble", "fumble_defense_recovered")) |>
    distinct(gameId, playId, frameId_of_end_of_play = frameId) |>
    ungroup()
  time_of_end_of_play
  
  # ball y
  ball_y_snap = tracking |>
    group_by(gameId, playId) %>%
    filter(event == 'line_set' & displayName == 'football') |>
    distinct(gameId, playId, ball_y_snap = y) |>
    ungroup()
  ball_y_snap
  
  #################################
  ### DISTILL THE TRACKING DATA ###
  #################################
  
  # merge in time_of_line_set
  df_tracking_1 = tracking |>
    inner_join(time_of_line_set) |>
    relocate(frameId_of_line_set, .after=frameId)
  dim(tracking)
  dim(df_tracking_1)
  sum(is.na(df_tracking_1$frameId_of_line_set))
  # View(df_tracking_1[1:1000,])
  
  # in each play, remove all frames before the line is set
  df_tracking_2 = df_tracking_1 |>
    filter(frameId >= frameId_of_line_set) 
  dim(df_tracking_1)
  dim(df_tracking_2)
  # View(df_tracking_2[1:1000,])

  # in each play, remove all frames after play is done
  df_tracking_2.5 = df_tracking_2 |>
    filter(frameId <= frameId_of_end_of_play) 
  dim(df_tracking_1)
  dim(df_tracking_2)
  dim(df_tracking_2.5)
  # View(df_tracking_2[1:1000,])
  
  # manipulate the tracking data s'more
  df_tracking_3 = 
    df_tracking_2.5 |>
    select(-frameId_of_line_set) |>
    # merge in snap frames
    left_join(snap_frames, by = c('gameId', 'playId')) |> 
    # standardize all frames to snap, in the units of seconds
    mutate(t_after_snap = (frameId - snap_frame)/10) |> 
    relocate(t_after_snap, .after = playId) |>
    # drop redundant or irrelevant columns
    select(-frameId, -frameType, -time, -snap_frame, -jerseyNumber) |>
    # add each player's general position
    left_join(players_pos) |>
    relocate(pos_official, .after = displayName) |>
    relocate(posGroup, .after = pos_official)
  df_tracking_3
  names(df_tracking_3)
  
  ### for each play, get each player's post-snap position, say, t_postsnap = 2.5 seconds after the snap
  t_postsnap = 2.5
  df_postsnap_locs = 
    df_tracking_3 %>%
    group_by(gameId, playId, nflId) %>%
    filter(t_after_snap == t_postsnap) %>%
    ungroup() %>%
    select(gameId, playId, nflId, x_postsnap = x, y_postsnap = y) %>%
    drop_na()
  df_postsnap_locs
  
  ### rightmost & leftmost defender
  df_furthest_defenders_0 = 
    df_tracking_3 %>%
    group_by(gameId, playId, nflId) %>%
    filter(t_after_snap == 0 & posGroup == "defense") %>%
    ungroup() %>%
    select(gameId, playId, nflId, pos_official, x_snap = x, y_snap = y) %>%
    arrange(gameId, playId, y_snap) %>%
    group_by(gameId, playId) %>%
    mutate(
      p = ifelse(row_number() == 1, "leftmost_def", ifelse(row_number() == n(), "rightmost_def", NA)),
    ) %>%
    ungroup()
  df_furthest_defenders_0
  
  df_furthest_defenders = 
    df_furthest_defenders_0 %>%
    filter(!is.na(p)) %>%
    pivot_wider(values_from = c(nflId, pos_official, x_snap, y_snap), names_from = p) 
  df_furthest_defenders
  
  # leftmost & rightmost defender quality check
  table(df_furthest_defenders$pos_official_leftmost_def)
  table(df_furthest_defenders$pos_official_rightmost_def)
  
  # define pre-snap safety from tracking data
  pre_snap_safeties_0 = 
    df_tracking_3 |> 
    # add some indicies
    group_by(gameId, playId) |> 
    mutate(i = cur_group_id()) |>
    relocate(i, .after = playId) |>
    ungroup() |>
    # join with line of scrimmage
    left_join(los, by = c('gameId', 'playId')) |> 
    # indicator for beyond 8.5 yards deep
    mutate(beyond_8AndAHalf = if_else(x >= los + 8.5, T, F)) |>
    # join with rightmost & leftmost defenders
    left_join(
      df_furthest_defenders %>% select(-all_of(contains("snap")), -all_of(contains("pos_official")))
    )
  pre_snap_safeties_0
  dim(df_tracking_3)
  dim(pre_snap_safeties_0)
  
  pre_snap_safeties = 
    pre_snap_safeties_0 |>
    # keep data from the time the line is set thru the snap
    filter(pre_snap_safeties_0$t_after_snap < 0) |>
    # define safety on play
    group_by(gameId, playId, i, nflId, nflId_rightmost_def, nflId_leftmost_def, pos_official, posGroup) |> 
    reframe(beyond_8AndAHalf = any(beyond_8AndAHalf, na.rm = T)) |>
    mutate(
      is_pre_safety =
        beyond_8AndAHalf &
        nflId != nflId_rightmost_def &
        nflId != nflId_leftmost_def &
        posGroup == "defense"
    ) |>
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
  
  # combine tracking data with safety info 
  df_tracking_4 = 
    play_info |> 
    # select relevant columns (can change)
    select(
      gameId, playId, defensiveTeam, possessionTeam, mofo, 
      expectedPoints, winProbability, xpass,
      down, yardsToGo, time_left_half, pre_score_diff,
    ) |>
    # remove plays where mofo is NA
    filter(!is.na(mofo)) |>
    # keep only the plays that have 1 or 2 safeties 
    inner_join(plays_with_1or2_safeties) |>
    # keep only dropback plays
    inner_join(plays_dropbacks) |>
    # keep plays where WP is reasonable (not garbage time plays)
    filter(0.05 < winProbability & winProbability < 0.95) |>
    # keep plays that aren't at the end of the half
    filter(time_left_half > 30) |>
    # keep plays whose xpass isn't too high
    filter(xpass < 0.95) |>
    # merge to tracking data
    left_join(df_tracking_3) |> 
    relocate(t_after_snap, .after = playId) |>
    rename(mofo_postsnap = mofo) |>
    mutate(mofo_postsnap = as.numeric(mofo_postsnap)) |>
    # merge line of scrimmage
    left_join(los) |>
    relocate(los, .before = x) |>
    # merge y of ball
    left_join(ball_y_snap) |> 
    relocate(ball_y_snap, .before = los) |>
    # merge safety indicators
    left_join(pre_snap_safeties) |>
    relocate(is_pre_safety, .after = pos_official) |>
    relocate(num_safeties, .after = is_pre_safety) |>
    select(-c(club, playDirection, dis, event)) |>
    # get post-snap locations
    left_join(df_postsnap_locs) |>
    relocate(mofo_postsnap, .after = y_postsnap) 
  df_tracking_4
  # View(df_tracking_4[1:1000,])
  
  # safety quality check
  df_safety_quality_check = 
    df_tracking_4 %>%
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
  
  # write manipulated tracking data
  object.size(df_tracking_4) / 10**9
  write_csv(df_tracking_4, filename)
}

#######################################
### STICH TOGETHER ALL THE DATASETS ###
#######################################

# stitch together the weekly datasets
WEEKS = 1:9
df_tracking_A = tibble()
for (week in WEEKS) {
  print(paste0("week=", week))
  filename = get_filename(week)
  filename
  
  df_w = read_csv(filename, show_col_types = F)
  df_tracking_A = bind_rows(df_tracking_A, df_w)
}

# check
object.size(df_tracking_A) / 10**9
sapply(df_tracking_A, function(col) sum(is.na(col))) # NA counts -- I think it's fine cuz the football itself is NA
nrow(df_tracking_A %>% distinct(gameId, playId)) # num plays
dim(df_tracking_A)
df_tracking_A %>% 
  select(gameId, playId, num_safeties, mofo_postsnap) %>%
  group_by(num_safeties, mofo_postsnap) %>%
  reframe(count = n())

### split into TRACKING, PLAYERS, PLAYS dataframes to save storage space
all_vars = names(df_tracking_A)
all_vars

# tracking dataframe
key_vars = c("gameId", "playId", "nflId", "displayName")
tracking_vars = c(
  "t_after_snap", "x", "y", "s", "a", "o", "dir"
)
df_tracking_C = df_tracking_A %>% select(c(all_of(key_vars), all_of(tracking_vars)))
df_tracking_C

# players dataframe
players_vars = c(
  "pos_official", "posGroup", "is_pre_safety", "x_postsnap", "y_postsnap"
)
df_players_C = df_tracking_A %>% select(c(all_of(key_vars), all_of(players_vars))) %>% distinct() 
df_players_C

# plays dataframe
plays_vars = c("gameId", "playId", setdiff(all_vars, c(
  key_vars, tracking_vars, players_vars, "beyond_8AndAHalf", "nflId_rightmost_def", "nflId_leftmost_def"
)))
plays_vars
df_plays_C = df_tracking_A %>% select(all_of(plays_vars)) %>% distinct() 
df_plays_C

# save data
write_csv(df_tracking_C, "../processed-data/df_C_tracking.csv")
write_csv(df_players_C, "../processed-data/df_C_players.csv")
write_csv(df_plays_C, "../processed-data/df_C_plays.csv")

# delete the intermediate files - DANGEROUS 
filenames_to_delete = sapply(WEEKS, FUN = get_filename)
filenames_to_delete
unlink(filenames_to_delete) 

