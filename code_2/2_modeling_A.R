
#################
### LIBRARIES ###
#################

### SET YOUR WORKING DIRECTORY TO THIS FILE (IN `code_2`)
library(tidyverse)
library(cvTools)
library(Metrics)
library(scales)

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

# estimate MOFO probability given just the defensive team and number of safeties
fit_model_defTeamNumSafeties <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,defensiveTeam,num_safeties,mofo_postsnap) 
  df_plays
  glm(mofo_postsnap ~ defensiveTeam:factor(num_safeties), data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_defTeamNumSafeties(df_tracking_presnap)
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

# multivariable baseline model
fit_model_best_baseline <- function(df_tracking) {
  df_plays = df_tracking %>% distinct(gameId,playId,mofo_postsnap,
    minSafetyDistToCenter,defensiveTeam,num_safeties) 
  df_plays
  glm(mofo_postsnap ~ minSafetyDistToCenter + defensiveTeam:factor(num_safeties), data = df_plays, family = "binomial")
}
# # example
# temp = fit_model_best_baseline(df_tracking_presnap)
# temp

#################################################
### DATAFRAME FOR MODELING WITH TRACKING DATA ###
#################################################

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

#########################################################
### TRACKING DATA MODELING - FUNCTIONAL DATA ANALYSIS ###
#########################################################

#FIXME
# from the safety movement data
df_tracking_safeties
# predict post_snap mofo
# using functional data analysis,
# which accounts for the paths/movements that safeties take



##########################
### PREDICTION CONTEST ###
##########################

df_all = df_tracking_safeties #FIXME
df_all

# K-fold cross validation
NUM_FOLDS = 20 #FIXME
plays_all = df_all %>% distinct(gameId, playId) %>% mutate(i = 1:n())
plays_all
set.seed(98247) # for reproducibility
folds <- cvFolds(n = nrow(plays_all), K = NUM_FOLDS, type = "random")
plays_all_f = 
  tibble(i = tibble(folds$subsets)[[1]][,1], FOLD = folds$which) %>% 
  arrange(i) %>%
  left_join(plays_all)
plays_all_f
table(plays_all_f$FOLD)

# cross validation prediction contest
df_losses = tibble()
for (fold in 1:NUM_FOLDS) {
  print(paste0("fold=",fold))
  
  # train-test split
  plays_train = plays_all_f %>% filter(FOLD != fold)
  plays_test  = plays_all_f %>% filter(FOLD == fold)
  df_train = plays_train %>% left_join(df_all) 
  df_test = plays_test %>% left_join(df_all) 
  
  # fit baseline models
  fit_overallMean = fit_model_overallMean(df_train)
  fit_numSafeties = fit_model_numSafeties(df_train)
  fit_defteam = fit_model_defteam(df_train)
  fit_defTeamNumSafeties = fit_model_defTeamNumSafeties(df_train)
  fit_minSafetyDistToCenter = fit_model_minSafetyDistToCenter(df_train)
  fit_best_baseline = fit_model_best_baseline(df_train)
  
  # predictions
  #FIXME
  df_preds_0 = 
    df_test %>%
    mutate(
      pred_fairCoin = 0.5,
      pred_overallMean = predict(fit_overallMean, ., type = "response"),
      pred_numSafeties = predict(fit_numSafeties, ., type = "response"),
      pred_defteam = predict(fit_defteam, ., type = "response"),
      pred_defTeamNumSafeties = predict(fit_defTeamNumSafeties, ., type = "response"),
      pred_minSafetyDistToCenter = predict(fit_minSafetyDistToCenter, ., type = "response"),
      pred_best_baseline = predict(fit_best_baseline, ., type = "response"),
    ) %>%
    select(gameId, playId, mofo_postsnap, all_of(starts_with("pred"))) %>%
    distinct()
  df_preds_0
  
  df_preds_1 = 
    df_preds_0 %>%
    pivot_longer(-c(gameId, playId, mofo_postsnap), names_to="model", values_to = "pred") %>%
    mutate(logloss = ll(actual = mofo_postsnap, predicted = pred))
  df_preds_1
  
  # losses results for this fold
  df_losses_f = 
    df_preds_1 %>%
    group_by(model) %>%
    reframe(logloss = mean(logloss)) %>%
    arrange(logloss) %>%
    mutate(fold = fold)
  logloss_ref = (df_losses_f %>% filter(str_detect(model, "fairCoin")))$logloss
  df_losses_f = 
    df_losses_f %>% 
    mutate(
      logloss_ref = logloss_ref,
      # reduction in error 
      RIE = - (logloss - logloss_ref) / logloss_ref,
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
df_losses %>%
  ggplot(aes(y = reorder(model, mean_logloss), x = logloss)) +
  geom_vline(xintercept = -log(1/2), color="gray60", linetype="dashed", linewidth=0.5) +
  geom_boxplot() +
  xlab("out-of-sample logloss") +
  ylab("model")

df_losses %>%
  mutate(p1 = exp(-logloss)) %>%
  ggplot(aes(y = reorder(model, mean_logloss), x = p1)) +
  geom_vline(xintercept = 1/2, color="gray60", linetype="dashed", linewidth=0.5) +
  geom_boxplot() +
  xlab("p = exp(-logloss)") +
  labs(caption = "the predictor has the same predictive power (logloss), over average,\nas always predicting the correct outcome with prob. p") +
  ylab("model")

df_losses %>%
  ggplot(aes(y = reorder(model, mean_logloss), x = RIE)) +
  geom_boxplot() +
  scale_x_continuous(labels = scales::percent) +
  ylab("model") +
  xlab("out-of-sample reduction in error\n(above the overall mean predictor)")

##########################