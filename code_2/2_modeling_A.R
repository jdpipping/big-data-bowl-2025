
#################
### LIBRARIES ###
#################

### SET YOUR WORKING DIRECTORY TO THIS FILE (IN `code_2`)
library(tidyverse)
library(cvTools)
library(Metrics)
library(scales)
library(splines)
library(keras)
library(tensorflow)

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

#################
### READ DATA ###
#################

# read data
df_tracking_A = read_csv('../processed-data/df_tracking_A.csv', show_col_types = F)
df_tracking_A
object.size(df_tracking_A) / 10**9

##############################
### MODIFIED TRACKING DATA ###
##############################

# pre-snap tracking data for prediction/modeling exercise
df_tracking_presnap = 
  df_tracking_A |>
  # keep just pre-snap data, and don't go too far into the past
  filter(-10 < t_after_snap & t_after_snap < 0) |>
  # make x coordinate relative to the line of scrimmage
  mutate(
    x1 = x - los,
    x1_postsnap = x_postsnap - los,
  )  |>
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
  mutate(minSafetyDistToCenter = min(abs( ifelse(is_pre_safety, y1, Inf) ))) |>
  ungroup() |>
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
table(df_tracking_presnap$t_after_snap)

# # check velo,accel components
# temp = df_tracking_presnap %>% filter(pos_official == "RB") %>% filter(s > 1)
# View(temp[1:2000,]) 

#######################
### BASELINE MODELS ###
#######################

# estimate MOFO probability using the overall mean
fit_model_overallMean <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ 1, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_overallMean(df_tracking_presnap)
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

# estimate MOFO probability given just the defensive team
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

# estimate MOFO probability given just the defensive team and number of safeties
fit_model_defTeamNumSafetiesOffTeam <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,possessionTeam,defensiveTeam,num_safeties,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ defensiveTeam:factor(num_safeties) + possessionTeam, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_defTeamNumSafetiesOffTeam(df_tracking_presnap)
# temp

# min distance to center of field
fit_model_minSafetyDistToCenter <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,minSafetyDistToCenter,mofo_postsnap)
  df_plays
  glm(mofo_postsnap ~ minSafetyDistToCenter, data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_minSafetyDistToCenter(df_tracking_presnap)
# temp

# # multivariable baseline model
# fit_model_best_baseline <- function(df_tracking) {
#   df_plays = df_tracking %>% distinct(
#     gameId,playId,mofo_postsnap,possessionTeam,minSafetyDistToCenter,defensiveTeam,num_safeties
#   )
#   df_plays
#   glm(mofo_postsnap ~ minSafetyDistToCenter + defensiveTeam:factor(num_safeties) + possessionTeam,
#       data = df_plays, family = "binomial")
#   # glm(mofo_postsnap ~ minSafetyDistToCenter + defensiveTeam:factor(num_safeties), data = df_plays, family = "binomial")
# }
# # # example
# # temp = fit_model_best_baseline(df_tracking_presnap)
# # temp

#####################################################
### FILTERED TRACKING DATA - MOVEMENT OF SAFETIES ###
#####################################################

# defensive positions
table(df_tracking_presnap$pos_official) 
length(table(df_tracking_presnap$pos_official)) # num positions
defensive_positions = c(
  "CB", "DB", "DE", "DT","FS","ILB","LB","MLB","OLB","SS"
)
defensive_positions

# keep just the defensive players 
df_tracking_def = df_tracking_presnap %>% filter(pos_official %in% defensive_positions)
dim(df_tracking_presnap)
dim(df_tracking_def)
num_def_players_per_play = 
  df_tracking_def %>% 
  distinct(gameId,playId,nflId,pos_official) %>% 
  group_by(gameId,playId) %>%
  reframe(num_def_players = n()) 
table(num_def_players_per_play$num_def_players) # good enough I guess...

# keep just the pre-snap safeties
df_tracking_safeties = 
  df_tracking_def |>
  filter(is_pre_safety) |>
  select(-c(pos_official, is_pre_safety))
df_tracking_safeties
nrow(df_tracking_safeties %>% distinct(gameId, playId))
table(df_tracking_safeties$t_after_snap)
# View(df_tracking_safeties[1:2000,])

########################################
### FUNCTIONAL DATA ANALYSIS EXAMPLE ###
########################################

# histogram of each play's earliest t_after_snap
df_min_t_after_snap = 
  df_tracking_safeties %>%
  group_by(gameId, playId) %>%
  reframe(min_t_after_snap = min(t_after_snap)) %>%
  arrange(min_t_after_snap)
df_min_t_after_snap %>% ggplot(aes(x = min_t_after_snap)) + geom_histogram()
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

# parameters
extrapolateEarlierTrajectories = TRUE
use_time_cutoff = TRUE
cutoff_time = -3.0

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

# Compute spline coefficients for x_t and y_t
spline_DF <- 10  # Number of spline basis functions (degrees of freedom)
x_spline <- compute_spline_coeffs(t, x_t, spline_DF)
y_spline <- compute_spline_coeffs(t, y_t, spline_DF)

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

##################################################
### FUNCTIONAL DATA ANALYSIS MOVEMENT FEATURES ###
##################################################

# parameters
spline_DF <- 10  # number of spline basis functions (degrees of freedom)
cutoff_time = -3.0
use_time_cutoff = TRUE
extrapolateEarlierTrajectories = TRUE

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
  x_t = df_i$x1 
  y_t = df_i$y1 
  t = df_i$t_after_snap
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
  # first and last (x,y) positions
  x_first = first(x_t)
  y_first = first(y_t)
  x_last = last(x_t)
  y_last = last(y_t)
  loc_vector = c(x_first, y_first, x_last, y_last)
  names(loc_vector) = c("x_first", "y_first", "x_last", "y_last")
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
  movement_features_lst[[length(movement_features_lst)+1]] = c(loc_vector, feature_vector)
}
df_movement_features = do.call(rbind, movement_features_lst)
df_movement_features = as_tibble(df_movement_features)
df_movement_features = bind_cols(groups, df_movement_features)
df_movement_features
df_movement_features_A = 
  df_tracking_safeties %>% 
  distinct(
    gameId, playId, nflId, 
    displayName, num_safeties, mofo_postsnap, minSafetyDistToCenter, 
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
    values_from = c(nflId, displayName, starts_with("x_"), starts_with("y_"))
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

#########################################################
### NEURAL NETWORK MODEL FOR FUNCTIONAL DATA ANALYSIS ###
#########################################################

get_movement_X <- function(df) {
  X = df %>%
    select(
      minSafetyDistToCenter,
      all_of(contains("_first")), all_of(contains("_last")),
      all_of(contains("spline"))
    )
  X
}

fit_movement_NN <- function(df_train, num_safeties, val_split=0) {
  # training data
  X_train = get_movement_X(df_train)
  X_train
  y_train = df_train$mofo_postsnap
  y_train
  
  # model
  model <- keras_model_sequential()
  # Add layers to the model
  if (num_safeties == 1) {
    h = 32
  } else if (num_safeties == 2) {
    h = 64
  }
  
  model %>%
    layer_dense(units = h, activation = 'silu', input_shape = ncol(X_train)) %>%
    layer_dense(units = 1, activation = 'sigmoid')
  model
  
  model %>% compile(optimizer = 'adam', loss = 'binary_crossentropy')
  set.seed(529735)
  tensorflow::tf$random$set_seed(711883)
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
# fit_movement_NN(df_safety_movement_1, num_safeties = 1, val_split = 0.2)
# fit_movement_NN(df_safety_movement_2, num_safeties = 2, val_split = 0.2)

##########################
### PREDICTION CONTEST ###
##########################

# dataframes with tracking data features
df_safety_movement_1
df_safety_movement_2

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
NUM_FOLDS = 10 #FIXME
set.seed(98247) # for reproducibility of the folds
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
    "minSafetyDistToCenter","defensiveTeam","possessionTeam","los"
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
  fit_overallMean = fit_model_overallMean(df_train_12)
  fit_numSafeties = fit_model_numSafeties(df_train_12)
  fit_defteam = fit_model_defteam(df_train_12)
  fit_offteam = fit_model_offteam(df_train_12)
  fit_defTeamNumSafeties = fit_model_defTeamNumSafeties(df_train_12)
  fit_defTeamNumSafetiesOffTeam = fit_model_defTeamNumSafetiesOffTeam(df_train_12)
  fit_minSafetyDistToCenter = fit_model_minSafetyDistToCenter(df_train_12)
  # fit_best_baseline = fit_model_best_baseline(df_train_12)
  
  # fit NN models
  if (TEST_NN) {
    fit_nn_1 = fit_movement_NN(df_safety_movement_1, num_safeties = 1, val_split = 0)
    fit_nn_2 = fit_movement_NN(df_safety_movement_2, num_safeties = 2, val_split = 0)
  }
  
  # predictions
  df_preds_1B = 
    df_test_1 %>%
    mutate(
      pred_fairCoin = 0.5,
      pred_overallMean = predict(fit_overallMean, ., type = "response"),
      pred_numSafeties = predict(fit_numSafeties, ., type = "response"),
      pred_defteam = predict(fit_defteam, ., type = "response"),
      pred_offteam = predict(fit_offteam, ., type = "response"),
      pred_defteamNumSafeties = predict(fit_defTeamNumSafeties, ., type = "response"),
      pred_defteamNumSafetiesOffteam = predict(fit_defTeamNumSafetiesOffTeam, ., type = "response"),
      pred_minSafetyDistToCenter = predict(fit_minSafetyDistToCenter, ., type = "response"),
      # pred_best_baseline = predict(fit_best_baseline, ., type = "response"),
    ) %>%
    select(gameId, playId, mofo_postsnap, all_of(starts_with("pred"))) %>%
    distinct()
  df_preds_1B
  
  df_preds_2B = 
    df_test_2 %>%
    mutate(
      pred_fairCoin = 0.5,
      pred_overallMean = predict(fit_overallMean, ., type = "response"),
      pred_numSafeties = predict(fit_numSafeties, ., type = "response"),
      pred_defteam = predict(fit_defteam, ., type = "response"),
      pred_offteam = predict(fit_offteam, ., type = "response"),
      pred_defteamNumSafeties = predict(fit_defTeamNumSafeties, ., type = "response"),
      pred_defteamNumSafetiesOffteam = predict(fit_defTeamNumSafetiesOffTeam, ., type = "response"),
      pred_minSafetyDistToCenter = predict(fit_minSafetyDistToCenter, ., type = "response"),
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
        pred_FDA = predict_movement_NN(., fit_nn_1)
      ) %>%
      select(gameId, playId, mofo_postsnap, all_of(starts_with("pred"))) %>%
      distinct()
    df_preds_1NN
    
    df_preds_2NN = 
      df_test_2 %>%
      mutate(
        pred_FDA = predict_movement_NN(., fit_nn_2)
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
ggsave("plot_results_logloss.png", width=12, height=4)

plot_results_RIE = 
  df_losses %>%
  ggplot(aes(y = reorder(model, mean_logloss), x = RIE)) +
  geom_boxplot() +
  scale_x_continuous(labels = scales::percent) +
  ylab("model") +
  xlab("out-of-sample reduction in error")
ggsave("plot_results_RIE.png", width=12, height=4)

plot_results_logloss_pScale = 
  df_losses %>%
  mutate(p1 = exp(-logloss)) %>%
  ggplot(aes(y = reorder(model, mean_logloss), x = p1)) +
  geom_vline(xintercept = 1/2, color="gray60", linetype="dashed", linewidth=0.5) +
  geom_boxplot() +
  xlab("p = exp(-logloss)") +
  labs(caption = "the predictor has the same predictive power (out-of-sample logloss),\n over average, as always predicting the correct outcome with prob. p") +
  ylab("model")
ggsave("plot_results_logloss_pScale.png", width=12, height=4)

##########################