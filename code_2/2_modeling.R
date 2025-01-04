
# FDA parameters
# NUM_FOLDS = 2 
NUM_FOLDS = 10
# spline_DF <- 10  # number of spline basis functions (degrees of freedom)
spline_DF = 5
cutoff_time = -3.0 # for training our model, consider frames from this time to -0.1s
use_time_cutoff = TRUE
extrapolateEarlierTrajectories = TRUE

#################
### LIBRARIES ###
#################

### SET YOUR WORKING DIRECTORY TO THIS FILE (IN `code_2`)
library(tidyverse)
library(cvTools)
library(Metrics)
library(scales)
library(splines)

### plotting pre-sets
theme_set(theme_bw())
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5, size=20),
  plot.subtitle = element_text(size=15),
  axis.title = element_text(size=20),
  axis.text = element_text(size=18),
  axis.text.x = element_text(size=18),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20),
  panel.spacing = unit(2, "lines")
) 

################################
### installing Keras package ###
################################

# # https://github.com/rstudio/tensorflow/issues/510
# # https://github.com/rstudio/tfprobability/issues/155
# ### install the development version of packages, in case the
# ### issue is already fixed but not on CRAN yet.
# ######remotes::install_github(sprintf("rstudio/%s", c("reticulate", "tensorflow", "keras")), force = TRUE)
# pkgs = c("reticulate", "tensorflow", "keras")
# remove.packages(pkgs)
# remotes::install_github(sprintf("rstudio/%s", pkgs))
# reticulate::miniconda_uninstall() # start with a blank slate
# reticulate::install_miniconda()
# keras::install_keras()
# tfprobability::install_tfprobability()
# 
# tensorflow::as_tensor("Hello World")
# tfprobability::tfd_multivariate_normal_diag

# need to set random seet BEFORE importing NN librariers to make it reproducible
set.seed(59835)
library(keras)
library(tensorflow)

#################
### READ DATA ###
#################

# read data
df_tracking_C = read_csv("../processed-data/df_C_tracking.csv", show_col_types = F)
df_players_C = read_csv("../processed-data/df_C_players.csv", show_col_types = F)
df_plays_C = read_csv("../processed-data/df_C_plays.csv", show_col_types = F)

# join data into one big dataframe
df_tracking_OG = 
  df_players_C %>%
  left_join(df_tracking_C) %>% 
  left_join(df_plays_C) %>%
  relocate(num_safeties, .after = is_pre_safety)
dim(df_tracking_C)
dim(df_tracking_OG)
# View(df_tracking_OG[1:1000,])

# safety quality check
df_safety_quality_check = 
  df_tracking_OG %>%
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

### percent of plays that are predictable
# x = 0.2
x = 0.1
df_eval %>% 
  reframe(play_is_predictable = p < x | p > 1-x) %>%
  group_by(play_is_predictable) %>%
  reframe(n = n()) %>%
  ungroup() %>%
  mutate(prop = n/sum(n), tot = sum(n))

##############################
### MODIFIED TRACKING DATA ###
##############################

# pre-snap tracking data for prediction/modeling exercise
df_tracking_presnap = 
  df_tracking_OG |>
  # keep just pre-snap data, and don't go too far into the past
  filter(-10 < t_after_snap & t_after_snap < 0) |>
  # make x coordinate relative to the line of scrimmage
  mutate(
    x1 = x - los,
    x1_postsnap = x_postsnap - los,
  )  |>
  relocate(x_postsnap, .after = dir) |>
  relocate(y_postsnap, .after = x_postsnap) |>
  relocate(x1, .after=y) |>
  relocate(x1_postsnap, .after=y_postsnap) |>
  # make y coordinate relative to the center of the field
  mutate(
    y1 = y - 53.3/2,
    y1_postsnap = y_postsnap - 53.3/2,
  ) |>
  relocate(y1, .after=x1) |>
  relocate(y1_postsnap, .after=x1_postsnap) |>
  # min safety distance to center
  group_by(gameId, playId) |>
  mutate(minSafetyDistToMOF = min(abs( ifelse(is_pre_safety, y1, Inf) ))) |>
  ungroup() |>
  # min safety distance to ball line
  group_by(gameId, playId) |>
  mutate(minSafetyHorizDistToBallLine = min(abs( ifelse(is_pre_safety, y - ball_y_snap, Inf) ))) |>
  ungroup() 
df_tracking_presnap
table(df_tracking_presnap$t_after_snap)

df_tracking_presnap =
  df_tracking_presnap |>
  # polar coordinates for the angles o and dir
  mutate(
    o_polar = (90 - o) %% 360,
    dir_polar = (90 - dir) %% 360,
  ) |>
  relocate(o_polar, .after = dir) |>
  relocate(dir_polar, .after = o_polar) |>
  # decompose velo/accel into (x,y) components
  mutate(
    v_x = s * cos(dir_polar * pi / 180),
    v_y = s * sin(dir_polar * pi / 180),
    a_x = a * cos(dir_polar * pi / 180),
    a_y = a * sin(dir_polar * pi / 180)
  ) |>
  relocate(v_x, .after = dir_polar) |>
  relocate(v_y, .after = v_x) |>
  relocate(a_x, .after = v_y) |>
  relocate(a_y, .after = a_x)
df_tracking_presnap
# table(df_tracking_presnap$t_after_snap)
# # check
# temp = df_tracking_presnap %>% filter(pos_official == "RB") %>% filter(s > 1)
# View(temp[1:2000,]) 

#######################
### BASELINE MODELS ###
#######################

# estimate MOFO probability using the overall mean
fit_model_intercept <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ 1, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_intercept(df_tracking_presnap)
# temp

# estimate MOFO probability given just the number of safeties
fit_model_numSafeties <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,num_safeties,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ factor(num_safeties), data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_numSafeties(df_tracking_presnap)
# temp

# estimate MOFO probability given just the defensive team
fit_model_defteam <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,defensiveTeam,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ defensiveTeam, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_defteam(df_tracking_presnap)
# temp

# estimate MOFO probability given just the offensive team
fit_model_offteam <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,possessionTeam,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ possessionTeam, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_offteam(df_tracking_presnap)
# temp

# estimate MOFO probability given just the defensive team and number of safeties
fit_model_defTeamNumSafeties <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,defensiveTeam,num_safeties,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ defensiveTeam:factor(num_safeties), data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_defTeamNumSafeties(df_tracking_presnap)
# temp

# estimate MOFO probability given both team names and number of safeties
fit_model_defTeamNumSafetiesOffTeam <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,possessionTeam,defensiveTeam,num_safeties,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ defensiveTeam:factor(num_safeties) + possessionTeam, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_defTeamNumSafetiesOffTeam(df_tracking_presnap)
# temp

# min distance to center of field
fit_model_minSafetyDistToMOF <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,minSafetyDistToMOF,mofo_postsnap)
  df_plays
  glm(mofo_postsnap ~ minSafetyDistToMOF, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_minSafetyDistToMOF(df_tracking_presnap)
# temp

# min horizontal distance to ball's original location
fit_model_minSafetyHorizDistToBallLine <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,minSafetyHorizDistToBallLine,mofo_postsnap)
  df_plays
  glm(mofo_postsnap ~ minSafetyHorizDistToBallLine, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_minSafetyHorizDistToBallLine(df_tracking_presnap)
# temp

# # multivariable baseline model
# fit_model_best_baseline <- function(df_tracking) {
#   df_plays = df_tracking %>% distinct(
#     gameId,playId,mofo_postsnap,possessionTeam,minSafetyDistToMOF,defensiveTeam,num_safeties
#   )
#   df_plays
#   glm(mofo_postsnap ~ minSafetyDistToMOF + defensiveTeam:factor(num_safeties) + possessionTeam,
#       data = df_plays, family = "binomial")
#   # glm(mofo_postsnap ~ minSafetyDistToMOF + defensiveTeam:factor(num_safeties), data = df_plays, family = "binomial")
# }
# # # example
# # temp = fit_model_best_baseline(df_tracking_presnap)
# # temp

#####################################
### TRACKING DATA - JUST SAFETIES ###
#####################################

# keep just the defensive players 
df_tracking_presnap %>% distinct(pos_official, posGroup) %>% arrange(posGroup) # defensive positions
df_tracking_def = df_tracking_presnap %>% filter(posGroup == "defense")
dim(df_tracking_presnap)
dim(df_tracking_def)
num_def_players_per_play = 
  df_tracking_def %>% 
  distinct(gameId,playId,nflId,pos_official) %>% 
  group_by(gameId,playId) %>%
  reframe(num_def_players = n()) 
table(num_def_players_per_play$num_def_players) # good enough I guess...

df_tracking_safeties_0 = 
  # keep just the pre-snap safeties ... and keep in mind we already limited to only pre-snap frames above
  df_tracking_def |>
  filter(is_pre_safety) |>
  select(-c(pos_official, posGroup, is_pre_safety)) #|>
  # # keep early downs -- LREADY DID THIS IN THE PREVIOUS FILE!!
  # filter(down == 1 | (down == 2 & yardsToGo >= 5))
df_tracking_safeties_0
nrow(df_tracking_safeties_0)
nrow(df_tracking_safeties_0 %>% distinct(gameId, playId))
sum(is.na(df_tracking_safeties_0))
table(df_tracking_safeties_0$t_after_snap)
# View(df_tracking_safeties_0[1:2000,])

# remove NAs
rows_with_NA = which(apply(df_tracking_safeties_0, 1, function(row) any(is.na(row))))
plays_with_NA = df_tracking_safeties_0[rows_with_NA,] %>% distinct(gameId, playId) %>% mutate(playHasNA = TRUE)
plays_with_NA
df_tracking_safeties = 
  df_tracking_safeties_0 %>% 
  left_join(plays_with_NA) %>% 
  mutate(playHasNA = ifelse(is.na(playHasNA), FALSE, playHasNA)) %>%
  filter(!playHasNA) %>% 
  select(-playHasNA)
dim(df_tracking_safeties_0)
dim(df_tracking_safeties)
sum(is.na(df_tracking_safeties))

########################################
### FUNCTIONAL DATA ANALYSIS EXAMPLE ###
########################################

# histogram of each play's earliest t_after_snap
df_min_t_after_snap = 
  df_tracking_safeties %>%
  group_by(gameId, playId) %>%
  reframe(min_t_after_snap = min(t_after_snap)) %>%
  arrange(min_t_after_snap)
# df_min_t_after_snap %>% ggplot(aes(x = min_t_after_snap)) + geom_histogram()
# earliest time t
min_min_t_after_snap = min(df_min_t_after_snap$min_t_after_snap)
min_min_t_after_snap

# Function to extrapolate a trajectory to earlier time points
# by just extending the first location back in to the past
extrapolateEarlierTrajectory <- function(t, values) {
  if (min_min_t_after_snap <= min(t) - 0.1) {
    t_added = seq(min_min_t_after_snap, min(t) - 0.1, by = 0.1)
    values_added = rep(values[1], length(t_added))
    
    t = c(t_added, t)
    values = c(values_added, values)
  }
  
  return(list(t = t, values = values))
}

# Function to compute spline coefficients
compute_spline_coeffs <- function(t, values, spline_DF) {
  # t: Time points (vector)
  # values: Observed values at time points (vector, e.g., x_t or y_t)
  # spline_DF: Degrees of freedom (number of spline basis functions)
  
  # Fit a B-spline basis to the data
  spline_basis <- bs(t, df = spline_DF, intercept = TRUE)
  
  # Compute coefficients by solving a linear regression
  fit <- lm(values ~ 0 + spline_basis)  # No intercept; fit basis directly
  coeffs <- coef(fit)
  
  return(list(coeffs = coeffs, basis = spline_basis))
}

# SHOW_EXAMPLE = TRUE
SHOW_EXAMPLE = FALSE
if (SHOW_EXAMPLE) {
  # set parameters extrapolateEarlierTrajectories, use_time_cutoff, cutoff_time, spline_DF at top of file
  
  # Example trajectory data
  # ex_traj = df_tracking_safeties %>% filter(gameId == 2022090800, playId == 692, nflId == 47844)
  # ex_traj = df_tracking_safeties %>% filter(gameId == 2022090800, playId == 692, nflId == 48026)
  # ex_traj = df_tracking_safeties %>% filter(gameId == 2022110602, playId == 3557, nflId == 46123)
  ex_traj = df_tracking_safeties %>% filter(gameId == 2022092503, playId == 1286, nflId == 53641)
  ex_traj
  
  # trajectory data
  t = ex_traj$t_after_snap
  x_t = ex_traj$x1
  y_t = ex_traj$y1
  if (extrapolateEarlierTrajectories) {
    etx = extrapolateEarlierTrajectory(t, x_t)
    ety = extrapolateEarlierTrajectory(t, y_t)
    t = etx$t
    x_t = etx$values
    y_t = ety$values
  }
  if (use_time_cutoff) {
    x_t = x_t[t >= cutoff_time]
    y_t = y_t[t >= cutoff_time]
    t = t[t >= cutoff_time]
  }
  t
  x_t
  y_t
  
  # Compute spline coefficients for x_t and y_t
  x_spline <- compute_spline_coeffs(t, x_t, spline_DF=spline_DF)
  y_spline <- compute_spline_coeffs(t, y_t, spline_DF=spline_DF)
  
  # Combine coefficients into a feature vector
  feature_vector <- c(x_spline$coeffs, y_spline$coeffs)
  
  # Print results
  cat("Spline coefficients for x_t:", x_spline$coeffs, "\n")
  cat("Spline coefficients for y_t:", y_spline$coeffs, "\n")
  cat("Combined feature vector:", feature_vector, "\n")
  
  # Optional: Reconstruct trajectory for validation
  reconstruct_trajectory <- function(t, coeffs, basis) {
    reconstructed <- as.vector(basis %*% coeffs)
    return(reconstructed)
  }
  
  # Reconstruct x_t and y_t
  reconstructed_x_t <- reconstruct_trajectory(t, x_spline$coeffs, x_spline$basis)
  reconstructed_y_t <- reconstruct_trajectory(t, y_spline$coeffs, y_spline$basis)
  
  # Plot original and reconstructed trajectories
  plot(t, x_t, type = "l", col = "blue", lwd = 2, main = "Original vs. Reconstructed Trajectories (Splines)",
       xlab = "Time", ylab = "Value")
  lines(t, reconstructed_x_t, col = "red", lwd = 2, lty = 2)
  # legend("topright", legend = c("Original x_t", "Reconstructed x_t"),
  #        col = c("blue", "red"), lty = c(1, 2), lwd = 2)
}

##################################################
### FUNCTIONAL DATA ANALYSIS MOVEMENT FEATURES ###
##################################################

# create safety-level dataframe with movement features
groups = df_tracking_safeties %>% distinct(gameId, playId, nflId)
groups
movement_features_lst = list()
for (i in 1:nrow(groups)) {
  print(paste0("generating movement spline features for i=",i,"/",nrow(groups)))
  row_i = groups[i,]
  row_i
  df_i = df_tracking_safeties %>% filter(gameId == row_i$gameId, playId == row_i$playId, nflId == row_i$nflId)
  df_i
  
  # get x and y trajectories
  t = df_i$t_after_snap
  x_t = df_i$x1 
  y_t = df_i$y1 
  v_x = df_i$v_x
  v_y = df_i$v_y
  if (extrapolateEarlierTrajectories) {
    etx = extrapolateEarlierTrajectory(t, x_t)
    ety = extrapolateEarlierTrajectory(t, y_t)
    etvx = extrapolateEarlierTrajectory(t, v_x)
    etvy = extrapolateEarlierTrajectory(t, v_y)
    t = etx$t
    x_t = etx$values
    y_t = ety$values
    v_x = etvx$values
    v_y = etvy$values
  }
  if (use_time_cutoff) {
    x_t = x_t[t >= cutoff_time]
    y_t = y_t[t >= cutoff_time]
    v_x = v_x[t >= cutoff_time]
    v_y = v_y[t >= cutoff_time]
    t = t[t >= cutoff_time] # keep at bottom of this if statement
  }
  # first and last (x,y) positions
  x_first = first(x_t)
  y_first = first(y_t)
  x_last = last(x_t)
  y_last = last(y_t)
  v_x_last = last(v_x)
  v_y_last = last(v_y)
  # locs
  loc_vector = c(x_first, y_first, x_last, y_last)
  names(loc_vector) = c("x_first", "y_first", "x_last", "y_last")
  # velos
  velo_vector = c(v_x_last, v_y_last)
  names(velo_vector) = c("v_x_last", "v_y_last")
  # compute spline coefficients for x_t and y_t
  x_spline <- compute_spline_coeffs(t, x_t, spline_DF)
  y_spline <- compute_spline_coeffs(t, y_t, spline_DF)
  # combine coefficients into a feature vector
  x_spline_coeffs = x_spline$coeffs
  names(x_spline_coeffs) = paste0("x_", names(x_spline_coeffs))
  y_spline_coeffs = x_spline$coeffs
  names(y_spline_coeffs) = paste0("y_", names(y_spline_coeffs))
  feature_vector <- c(x_spline_coeffs, y_spline_coeffs)
  # save features
  movement_features_lst[[length(movement_features_lst)+1]] = c(loc_vector, velo_vector, feature_vector)
}
df_movement_features = do.call(rbind, movement_features_lst)
df_movement_features = as_tibble(df_movement_features)
df_movement_features = bind_cols(groups, df_movement_features)
df_movement_features
df_movement_features_A = 
  df_tracking_safeties %>% 
  distinct(
    gameId, playId, nflId, 
    displayName, num_safeties, mofo_postsnap,
    minSafetyDistToMOF, minSafetyHorizDistToBallLine,
    defensiveTeam, possessionTeam, los
  ) %>% 
  left_join(df_movement_features) %>%
  arrange(gameId, playId, nflId)
df_movement_features_A

# split into plays with 1 and 2 safeties
df_safety_movement_1 = df_movement_features_A %>% filter(num_safeties == 1)
df_safety_movement_2_v0 = df_movement_features_A %>% filter(num_safeties == 2)
nrow(df_safety_movement_1) # num plays with 1 safety
nrow(df_safety_movement_2_v0)/2 # num plays with 2 safeties

# for plays with 2 safeties, make 1 row per play
df_safety_movement_2_v1 <- 
  df_safety_movement_2_v0 %>%
  group_by(gameId, playId) %>%  # Group by play identifier
  mutate(player_num = paste0("p",row_number())) %>%  # Assign unique identifier for each player
  pivot_wider(
    names_from = player_num,   # Use player number to create new column names
    values_from = c(nflId, displayName, starts_with("x_"), starts_with("y_"), starts_with("v_x_"), starts_with("v_y_"))
  ) %>%
  ungroup()
names(df_safety_movement_2_v1)
reordered_names = c(
  names(df_safety_movement_2_v1)[!str_detect(names(df_safety_movement_2_v1), "spline")],
  names(df_safety_movement_2_v1)[str_detect(names(df_safety_movement_2_v1), "spline") & str_detect(names(df_safety_movement_2_v1), "_p1")],
  names(df_safety_movement_2_v1)[str_detect(names(df_safety_movement_2_v1), "spline") & str_detect(names(df_safety_movement_2_v1), "_p2")]
)
reordered_names
df_safety_movement_2 = df_safety_movement_2_v1[,reordered_names]
df_safety_movement_2

# View the transformed dataset
df_safety_movement_1
df_safety_movement_2

# check NA
sum(is.na(df_safety_movement_1))
sum(is.na(df_safety_movement_2))

# save movement features
write_csv(df_safety_movement_1, "df_safety_movement_1.csv")
write_csv(df_safety_movement_2, "df_safety_movement_2.csv")

#########################################################
### NEURAL NETWORK MODEL FOR FUNCTIONAL DATA ANALYSIS ###
#########################################################

# make the NN training reproducible - DIDN'T WORK...
# # Disable Multithreading and Parallelism
# Sys.setenv(OMP_NUM_THREADS = "1")  # Control OpenMP threads
# Sys.setenv(MKL_NUM_THREADS = "1") # Control MKL threads
# Sys.setenv(TF_DETERMINISTIC_OPS = "1")  # Enable deterministic ops
# # Configure TensorFlow
# # # If using GPU:
# # config <- tf$compat$v1$ConfigProto(
# #   gpu_options = tf$compat$v1$GPUOptions(allow_growth = TRUE),
# #   allow_soft_placement = TRUE
# # )
# # sess <- tf$compat$v1$Session(config = config)
# # tf$compat$v1$keras$backend$set_session(sess)
# # If using CPU only:
# Sys.setenv("CUDA_VISIBLE_DEVICES" = "-1")  # Force TensorFlow to use CPU
# # 2. Force Full Determinism (Even for GPU)
# tf$config$experimental$enable_op_determinism()
# # contrl=ol multithrrading 
# Sys.setenv(OMP_NUM_THREADS = "1")
# Sys.setenv(MKL_NUM_THREADS = "1")
# # Enable verbose logging to track if nondeterministic ops are sneaking in:
# tf$debugging$set_log_device_placement(TRUE)

get_movement_X <- function(df) {
  X = df %>%
    select(
      all_of(contains("_first")), 
      all_of(contains("_last"))
    )
  X
}

# get_movement_X <- function(df) {
#   X = df %>%
#     select(
#       minSafetyDistToMOF,
#       all_of(contains("_first")), 
#       all_of(contains("_last")),
#       all_of(contains("spline"))
#     )
#   X
# }

fit_movement_NN <- function(df_train, num_safeties, val_split=0) {
  # training data
  X_train = get_movement_X(df_train)
  X_train
  y_train = df_train$mofo_postsnap
  y_train
  
  # make the NN training reproducible - DIDN'T WORK...
  set.seed(7441883)
  tf$random$set_seed(7441883)
  reticulate::py_run_string("import numpy as np; np.random.seed(42)")
  
  # model
  model <- keras_model_sequential()
  # Add layers to the model. Make sure h is a power of 2.
  if (num_safeties == 1) {
    h = 32
  } else if (num_safeties == 2) {
    h = 64
  }

  model %>%
    layer_dense(units = h, activation = 'silu', input_shape = ncol(X_train)) %>%
    layer_dense(units = 1, activation = 'sigmoid')
  model
  
  {
    # Using a neural network with 1 dense hidden layer of size 64 and a sigmoid output layer 
    # can make sense depending on the complexity of your data and the task at hand. 
    # Here’s a breakdown:
    #  • Pros of Using a Single Hidden Layer with 64 Units:
    #     • Flexible Complexity: A single hidden layer with sufficient units (like 64) 
    #       is a universal approximator and can model reasonably complex relationships.
    #     • Reduced Risk of Overfitting (Compared to Larger Models):
    #       With fewer layers and moderate size, you can limit overfitting, 
    #       especially if regularization is applied.
    #  • When This Architecture Makes Sense:
    #     • Simple Relationships: If the relationship between the input and output
    #       is straightforward or mildly nonlinear.
    #     • Small Dataset: If your dataset is relatively small, 
    #       this compact architecture might perform well without requiring 
    #       too much data to generalize.
    
    # # dropout and regularization did slightly worse:
    # model %>%
    #   layer_dense(units = h, activation = "silu", input_shape = ncol(X_train),
    #               kernel_regularizer = regularizer_l2(0.001)) %>% # L2 regularization
    #   layer_dropout(rate = 0.3) %>%  # Dropout to reduce overfitting
    #   layer_dense(units = 1, activation = "sigmoid")  # Output layer for binary classification
    
    # # this larger network did slightly worse:
    # model %>%
    #   layer_dense(units = h, activation = "silu", input_shape = ncol(X_train),
    #               kernel_regularizer = regularizer_l2(0.001)) %>%  # First hidden layer
    #   layer_dropout(rate = 0.3) %>%  # Dropout for regularization
    #   layer_dense(units = h/2, activation = "silu",
    #               kernel_regularizer = regularizer_l2(0.001)) %>%  # Second hidden layer
    #   layer_dropout(rate = 0.3) %>%  # Dropout for regularization
    #   layer_dense(units = 1, activation = "sigmoid")  # Output layer for binary classification
  }
  
  # train the neural network
  model %>% compile(optimizer = 'adam', loss = 'binary_crossentropy')
  training_history <- 
    model %>%
    fit(
      x = as.matrix(X_train),  
      y = y_train,             
      epochs = 50,                   # Number of training epochs
      batch_size = 32,               # Batch size
      validation_split = val_split,   # Portion of data for validation
      callbacks = list(callback_early_stopping(patience = 10))  # Early stopping
    )
  training_history
  print(training_history)
  
  model
}

predict_movement_NN <- function(df_test, model) {
  X_test = get_movement_X(df_test)
  predict(model, as.matrix(X_test), type="probs")[,1]
}

# examples
# get_movement_X(df_safety_movement_1)
# get_movement_X(df_safety_movement_2)
# fit_movement_NN(df_safety_movement_1, num_safeties = 1, val_split = 0.2)
# fit_movement_NN(df_safety_movement_2, num_safeties = 2, val_split = 0.2)

##########################
### PREDICTION CONTEST ###
##########################

# dataframes with tracking data features
df_safety_movement_1
df_safety_movement_2 
nrow(df_safety_movement_1) + nrow(df_safety_movement_2) # num plays
nrow(df_safety_movement_1) # num plays
nrow(df_safety_movement_2) # num plays

# play indices
plays_all = 
  bind_rows(
    df_safety_movement_1 %>% select(gameId,playId,num_safeties),
    df_safety_movement_2 %>% select(gameId,playId,num_safeties)
  ) %>%
  arrange(gameId,playId) %>%
  mutate(i = 1:n()) %>%
  relocate(i, .before=gameId)
plays_all

# K-fold cross validation
TEST_NN = TRUE
set.seed(12345) # for reproducibility of the folds
# set variable NUM_FOLDS at top of file
folds <- cvFolds(n = nrow(plays_all), K = NUM_FOLDS, type = "random")
plays_all_f = 
  tibble(i = tibble(folds$subsets)[[1]][,1], FOLD = folds$which) %>% 
  arrange(i) %>%
  left_join(plays_all)
plays_all_f
table(plays_all_f$FOLD)

# cross validation prediction contest
df_losses = tibble()
df_preds_outOfSample = tibble()
for (fold in 1:NUM_FOLDS) {
  print(paste0("fold=",fold))
  
  # train-test split
  plays_train = plays_all_f %>% filter(FOLD != fold)
  plays_test  = plays_all_f %>% filter(FOLD == fold)
  
  # train dataframes 
  df_train_1 = plays_train %>% filter(num_safeties==1) %>% left_join(df_safety_movement_1) 
  df_train_2 = plays_train %>% filter(num_safeties==2) %>% left_join(df_safety_movement_2) 
  nrow(df_train_1) + nrow(df_train_2) == nrow(plays_train)
  nrow(plays_train)
  vars_for_12 = c(
    "i","FOLD","gameId","playId","num_safeties","mofo_postsnap",
    "minSafetyHorizDistToBallLine","minSafetyDistToMOF","defensiveTeam","possessionTeam","los"
  )
  df_train_12 = bind_rows(
    df_train_1 %>% select(all_of(vars_for_12)),
    df_train_2 %>% select(all_of(vars_for_12)),
  )
  nrow(df_train_12) == nrow(plays_train)
  
  # test dataframes
  df_test_1 = plays_test %>% filter(num_safeties==1) %>% left_join(df_safety_movement_1) 
  df_test_2 = plays_test %>% filter(num_safeties==2) %>% left_join(df_safety_movement_2) 
  nrow(df_test_1) + nrow(df_test_2) == nrow(plays_test)
  nrow(plays_test)
  df_test_12 = bind_rows(
    df_test_1 %>% select(all_of(vars_for_12)),
    df_test_2 %>% select(all_of(vars_for_12)),
  )
  nrow(df_test_12) == nrow(plays_test)
  
  # fit baseline models
  fit_intercept = fit_model_intercept(df_train_12)
  fit_numSafeties = fit_model_numSafeties(df_train_12)
  fit_defteam = fit_model_defteam(df_train_12)
  fit_offteam = fit_model_offteam(df_train_12)
  fit_defTeamNumSafeties = fit_model_defTeamNumSafeties(df_train_12)
  fit_defTeamNumSafetiesOffTeam = fit_model_defTeamNumSafetiesOffTeam(df_train_12)
  fit_minSafetyDistToMOF = fit_model_minSafetyDistToMOF(df_train_12)
  fit_minSafetyHorizDistToBallLine = fit_model_minSafetyHorizDistToBallLine(df_train_12)
  # fit_best_baseline = fit_model_best_baseline(df_train_12)
  
  # fit NN models
  if (TEST_NN) {
    fit_nn_1 = fit_movement_NN(df_train_1, num_safeties = 1, val_split = 0)
    fit_nn_2 = fit_movement_NN(df_train_2, num_safeties = 2, val_split = 0)
  }
  
  # predictions
  df_preds_1B = 
    df_test_1 %>%
    mutate(
      pred_fairCoin = 0.5,
      pred_intercept = predict(fit_intercept, ., type = "response"),
      pred_numSafeties = predict(fit_numSafeties, ., type = "response"),
      pred_defteam = predict(fit_defteam, ., type = "response"),
      pred_offteam = predict(fit_offteam, ., type = "response"),
      pred_defTeamNumSafeties = predict(fit_defTeamNumSafeties, ., type = "response"),
      pred_defTeamNumSafetiesOffTeam = predict(fit_defTeamNumSafetiesOffTeam, ., type = "response"),
      pred_minSafetyDistToMOF = predict(fit_minSafetyDistToMOF, ., type = "response"),
      pred_minSafetyHorizDistToBallLine = predict(fit_minSafetyHorizDistToBallLine, ., type = "response"),
      # pred_best_baseline = predict(fit_best_baseline, ., type = "response"),
    ) %>%
    select(gameId, playId, mofo_postsnap, all_of(starts_with("pred"))) %>%
    distinct()
  df_preds_1B
  
  df_preds_2B = 
    df_test_2 %>%
    mutate(
      pred_fairCoin = 0.5,
      pred_intercept = predict(fit_intercept, ., type = "response"),
      pred_numSafeties = predict(fit_numSafeties, ., type = "response"),
      pred_defteam = predict(fit_defteam, ., type = "response"),
      pred_offteam = predict(fit_offteam, ., type = "response"),
      pred_defTeamNumSafeties = predict(fit_defTeamNumSafeties, ., type = "response"),
      pred_defTeamNumSafetiesOffTeam = predict(fit_defTeamNumSafetiesOffTeam, ., type = "response"),
      pred_minSafetyDistToMOF = predict(fit_minSafetyDistToMOF, ., type = "response"),
      pred_minSafetyHorizDistToBallLine = predict(fit_minSafetyHorizDistToBallLine, ., type = "response"),
      # pred_best_baseline = predict(fit_best_baseline, ., type = "response"),
    ) %>%
    select(gameId, playId, mofo_postsnap, all_of(starts_with("pred"))) %>%
    distinct()
  df_preds_2B
  
  df_preds = bind_rows(df_preds_1B, df_preds_2B)
  df_preds
  nrow(df_preds)
  
  if (TEST_NN) {
    df_preds_1NN = 
      df_test_1 %>%
      mutate(
        pred_ourModel = predict_movement_NN(., fit_nn_1)
      ) %>%
      select(gameId, playId, mofo_postsnap, all_of(starts_with("pred"))) %>%
      distinct()
    df_preds_1NN
    
    df_preds_2NN = 
      df_test_2 %>%
      mutate(
        pred_ourModel = predict_movement_NN(., fit_nn_2)
      ) %>%
      select(gameId, playId, mofo_postsnap, all_of(starts_with("pred"))) %>%
      distinct()
    df_preds_2NN
    
    df_preds_NN = bind_rows(df_preds_1NN, df_preds_2NN)
    df_preds_NN
    nrow(df_preds_NN)
     
    df_preds = left_join(df_preds, df_preds_NN)
    df_preds
    nrow(df_preds)
  }
  
  df_preds_outOfSample = bind_rows(df_preds_outOfSample, df_preds)

  df_preds_A = 
    df_preds %>%
    pivot_longer(-c(gameId, playId, mofo_postsnap), names_to="model", values_to = "pred") %>%
    mutate(logloss = ll(actual = mofo_postsnap, predicted = pred))
  df_preds_A
  
  # losses results for this fold
  df_losses_f = 
    df_preds_A %>%
    group_by(model) %>%
    reframe(logloss = mean(logloss)) %>%
    arrange(logloss) %>%
    mutate(fold = fold)
  logloss_ref = (df_losses_f %>% filter(str_detect(model, "fairCoin")))$logloss
  df_losses_f = 
    df_losses_f %>% 
    mutate(
      logloss_ref = logloss_ref,
      RIE = - (logloss - logloss_ref) / logloss_ref, # reduction in error 
    )
  df_losses_f
  
  # save losses
  df_losses = bind_rows(df_losses, df_losses_f)
}
df_losses = 
  df_losses %>% 
  group_by(model) %>%
  mutate(
    mean_logloss = mean(logloss),
    mean_RIE = mean(RIE)
  ) %>%
  ungroup()
df_losses

# plot results
plot_results_logloss = 
  df_losses %>%
  ggplot(aes(y = reorder(model, mean_logloss), x = logloss)) +
  geom_vline(xintercept = -log(1/2), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_boxplot() +
  xlab("out-of-sample logloss") +
  ylab("model")
ggsave("results_loss_plot_logloss.png", width=12, height=4)

plot_results_RIE = 
  df_losses %>%
  ggplot(aes(y = reorder(model, mean_logloss), x = RIE)) +
  geom_boxplot() +
  scale_x_continuous(labels = scales::percent) +
  ylab("model") +
  xlab("out-of-sample reduction in error")
ggsave("results_loss_plot_RIE.png", width=12, height=4)

plot_results_logloss_pScale = 
  df_losses %>%
  mutate(p1 = exp(-logloss)) %>%
  ggplot(aes(y = reorder(model, mean_logloss), x = p1)) +
  geom_vline(xintercept = 1/2, color="gray60", linetype="dashed", linewidth=0.5) +
  geom_boxplot() +
  xlab("q = exp(-logloss)") +
  labs(caption = "the predictor has the same predictive power (out-of-sample logloss),\n over average, as always predicting the correct outcome with prob. q") +
  ylab("model")
ggsave("results_loss_plot_logloss_pScale.png", width=12, height=4)

beepr::beep(3)

##########################

# get out-of-sample predictions for each play
df_preds_outOfSample_A =
  df_preds_outOfSample %>%
  select(gameId,playId,mofo_postsnap,pred_ourModel) %>%
  rename(p = pred_ourModel)
df_preds_outOfSample_A
df_eval_0 =
  df_tracking_safeties %>%
  distinct(
    gameId,playId,nflId,displayName,num_safeties,defensiveTeam,
    possessionTeam,expectedPoints,winProbability,
  ) %>%
  left_join(df_preds_outOfSample_A) %>%
  relocate(p, .after = num_safeties) %>%
  group_by(gameId, playId) %>%  # Group by play identifier
  mutate(player_num = paste0("p",row_number())) %>%  # Assign unique identifier for each player
  pivot_wider(
    names_from = player_num,   # Use player number to create new column names
    values_from = c(nflId, displayName)
  ) %>%
  ungroup() %>%
  rename(p_cv = p)
df_eval_0

######################################
### FULL MODEL TRAINED ON ALL DATA ###
######################################

# train full NNs
fit_nn_1_full = fit_movement_NN(df_safety_movement_1, num_safeties = 1, val_split = 0)
fit_nn_2_full = fit_movement_NN(df_safety_movement_2, num_safeties = 2, val_split = 0)
# get predictions of full NNs
preds_nn_1_full = predict_movement_NN(df_safety_movement_1, fit_nn_1_full)
preds_nn_2_full = predict_movement_NN(df_safety_movement_2, fit_nn_2_full)
preds_nn_full =
  bind_rows(
    df_safety_movement_1 %>% select(gameId,playId,num_safeties) %>% mutate(p = preds_nn_1_full),
    df_safety_movement_2 %>% select(gameId,playId,num_safeties) %>% mutate(p = preds_nn_2_full),
  )
df_eval = df_eval_0 %>% left_join(preds_nn_full) %>% relocate(p, .before = p_cv)

# save
write_csv(df_eval, "results_df_preds.csv")
# df_eval = read_csv("results_df_preds.csv")

########################
### CALIBRATION PLOT ###
########################

NUM_SAFETIES_OPTION = list(c(1,2), 1, 2)
NUM_SAFETIES_OPTION
num_bins = 10
# num_bins = 8
# num_bins = 5
j = 1

{
  # for (j in 1:length(NUM_SAFETIES_OPTION)) {
  NUM_SAFETIES = NUM_SAFETIES_OPTION[[j]]
  NUM_SAFETIES

  p_breaks = seq(0,1,length.out=num_bins+1)
  df_cal =
    df_eval %>%
    select(num_safeties, p, mofo_postsnap) %>%
    filter(num_safeties %in% NUM_SAFETIES) %>%
    mutate(
      p_bin = cut(p, breaks=p_breaks, include.lowest = TRUE),
      L = as.numeric(sub("\\((.+),.*", "\\1", p_bin)),
      L = ifelse(is.na(L),0,L),
      R = as.numeric(sub(".*,\\s*([0-9.]+)]", "\\1", p_bin)),
      R = ifelse(is.na(R),1,R),
      M = (L+R)/2
    )
  df_cal

  df_cal_1 =
    df_cal %>%
    group_by(p_bin) %>%
    reframe(
      M = unique(M),
      n = n(),
      emp_prop = mean(mofo_postsnap),
    )
  df_cal_1

  plot_calibration = 
    df_cal_1 %>%
    # ggplot(aes(x = p_bin, y = emp_prop, size = n)) +
    ggplot(aes(x = M, y = emp_prop, size = n)) +
    geom_abline(intercept=0,slope=1) +
    geom_point() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15)) +
    scale_x_continuous(limits = c(0,1), breaks=p_breaks) +
    xlab("predicted probability p(MOFO)") +
    ylab("actual MOFO rate") +
    ylim(c(0,1)) +
    labs(title="Calibration Plot")
  ggsave(paste0("results_plot_calibration_",j,".png"),plot_calibration,width=7,height=5)
}

beepr::beep(3)

##########################

