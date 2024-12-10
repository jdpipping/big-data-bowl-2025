
#################
### LIBRARIES ###
#################

### SET YOUR WORKING DIRECTORY TO THIS FILE (IN `code_2`)
library(tidyverse)
library(glmnet)

### load stan stuff
library(rstan)
rstan_options(auto_write = TRUE)
cores = 1
NUM_ITS = 5000

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
df_eval0 = 
  read_csv("results_df_preds_outOfSample.csv", show_col_types = F) %>%
  mutate(entropy = -p*log(p,base=2) - (1-p)*log(1-p,base=2)) %>%
  relocate(entropy, .after=p) %>%
  mutate(j = 1:n()) %>%
  relocate(j, .before=gameId) 
df_eval0

# create pid 1,...,M (needed for Stan model)
all_pids = 
  bind_rows(
    df_eval0 %>% 
      group_by(nflId_p1, displayName_p1) %>% 
      reframe(n = n(), defensiveTeam = last(defensiveTeam)) %>%
      rename(nflId = nflId_p1, displayName = displayName_p1) %>%
      arrange(nflId) %>%
      drop_na(),
    df_eval0 %>% 
      group_by(nflId_p2, displayName_p2) %>% 
      reframe(n = n(), defensiveTeam = last(defensiveTeam)) %>%
      rename(nflId = nflId_p2, displayName = displayName_p2) %>%
      arrange(nflId) %>%
      drop_na()
  ) %>%
  group_by(nflId, displayName, defensiveTeam) %>%
  reframe(n = sum(n)) %>%
  arrange(nflId) %>%
  mutate(pid = 1:n()) 
all_pids
sum(all_pids$n)

# merge eval with pid's
df_eval =
  df_eval0 %>% 
  left_join(
    all_pids %>% select(nflId_p1 = nflId, pid1 = pid)
  ) %>%
  left_join(
    all_pids %>% select(nflId_p2 = nflId, pid2 = pid)
  ) %>%
  mutate(pid2 = ifelse(is.na(pid2), 0, pid2)) %>% # label NAs as player 0
  mutate(tid = as.numeric(as.factor(defensiveTeam)))
df_eval

####################################
### DEFENSIVE TEAM EFFECTS MODEL ###
####################################

# plot mean entropy by team
plot_team_mean_entropy = 
  df_eval %>%
  group_by(defensiveTeam) %>%
  reframe(mean_entropy = mean(entropy)) %>%
  arrange(mean_entropy) %>%
  ggplot(aes(x = mean_entropy, y = reorder(defensiveTeam, mean_entropy))) +
  geom_point(size=4) +
  ylab("defensive team") +
  xlab("mean entropy")
ggsave("results_plot_team_mean_entropy.png", width=6, height=8)

# but is it significant?

### data for stan
df_stan_train_list_t <- list(
  n = nrow(df_eval),
  num_teams = max(df_eval$tid),
  tid = df_eval$tid,
  y = df_eval$entropy
)

### stan file
filename_t = "team_effects.stan"
STANMODEL_t <- stan_model(file = filename_t, model_name = filename_t)
STANMODEL_t

### train or load the stan model
TRAIN_ME_t = TRUE
# TRAIN_ME_t = FALSE
model_filename_t = paste0(str_remove_all(filename_t, "\\.stan"), "_fit", ".rds")
if (TRAIN_ME_t | !file.exists(model_filename_t)) {
  # Train the model
  seed = 12345
  set.seed(seed)
  NUM_ITERS_IN_CHAIN = NUM_ITS
  model_fit_t <-
    sampling(
      STANMODEL_t,
      data = df_stan_train_list_t,
      iter = NUM_ITERS_IN_CHAIN,
      chains = cores, 
      cores = cores, 
      seed = seed
    )
  ### save model
  saveRDS(model_fit_t, model_filename_t)
} else {
  model_fit_t = readRDS(model_filename_t)
}
model_fit_t

### check convergence
df_summary = summary(model_fit_t)$summary
df_summary
vec_rhats = df_summary[,"Rhat"]
vec_rhats
vec_rhats1 = vec_rhats[!str_detect(names(vec_rhats), "_new") & !str_detect(names(vec_rhats), "lp__")]
vec_rhats1
all(vec_rhats1 < 1.1)
# hist(vec_rhats1)

### posterior summary
post_summary_vals_t = summary(model_fit_t)$summary
post_summary_vals_t =
  tibble(
    name = rownames(post_summary_vals_t),
    L = post_summary_vals_t[,"2.5%"],
    M = post_summary_vals_t[,"mean"],
    U = post_summary_vals_t[,"97.5%"]
  )
# df_post_vals
plot_team_level_effects = 
  post_summary_vals_t %>% 
  filter(str_detect(name, "beta\\[") ) %>%
  mutate(
    param = str_remove_all(str_replace_all(str_remove_all(name, "_new"), "[0-9]*", ""), "\\[|\\]"),
    tid = extract_numeric(name)
  ) %>%
  left_join(df_eval %>% distinct(tid, defensiveTeam)) %>%
  ggplot(aes(y = reorder(defensiveTeam, M))) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_point(aes(x=M), size=4) +
  geom_errorbar(aes(xmin=L, xmax=U), linewidth=0.5) +
  # theme(axis.text.y = element_text(size=8)) +
  ylab("defensive team") +
  xlab("coefficient (entropy above average)")
ggsave("results_plot_team_level_effects.png", width=6, height=8)

############################
### PLAYER EFFECTS MODEL ###
############################

### data for stan
df_stan_train_list <- list(
  n = nrow(df_eval),
  num_players = nrow(all_pids),
  num_safeties = df_eval$num_safeties,
  pid1 = df_eval$pid1,
  pid2 = df_eval$pid2,
  y = df_eval$entropy
)

### stan file
filename = "player_effects.stan"
STANMODEL <- stan_model(file = filename, model_name = filename)
STANMODEL

### train or load the stan model
TRAIN_ME = TRUE
# TRAIN_ME = FALSE
model_filename = paste0(str_remove_all(filename, "\\.stan"), "_fit", ".rds")
if (TRAIN_ME | !file.exists(model_filename)) {
  # Train the model
  seed = 12345
  set.seed(seed)
  NUM_ITERS_IN_CHAIN = NUM_ITS
  model_fit <-
    sampling(
      STANMODEL,
      data = df_stan_train_list,
      iter = NUM_ITERS_IN_CHAIN,
      chains = cores, 
      cores = cores, 
      seed = seed
    )
  ### save model
  saveRDS(model_fit, model_filename)
} else {
  model_fit = readRDS(model_filename)
}
model_fit

### check convergence
df_summary = summary(model_fit)$summary
df_summary
vec_rhats = df_summary[,"Rhat"]
vec_rhats
vec_rhats1 = vec_rhats[!str_detect(names(vec_rhats), "_new") & !str_detect(names(vec_rhats), "lp__")]
vec_rhats1
all(vec_rhats1 < 1.1)
# hist(vec_rhats1)

### posterior summary
post_summary_vals = summary(model_fit)$summary
df_post_vals =
  tibble(
    name = rownames(post_summary_vals),
    L = post_summary_vals[,"2.5%"],
    M = post_summary_vals[,"mean"],
    U = post_summary_vals[,"97.5%"]
  )
# df_post_vals
min_num_plays = 60
plot_player_level_effects = 
  df_post_vals %>% 
  filter(str_detect(name, "beta\\[") ) %>%
  mutate(
    param = str_remove_all(str_replace_all(str_remove_all(name, "_new"), "[0-9]*", ""), "\\[|\\]"),
    pid = extract_numeric(name)
  ) %>%
  left_join(all_pids) %>%
  mutate(name = paste(displayName, defensiveTeam)) %>%
  filter(n > min_num_plays) %>%
  ggplot(aes(y = reorder(name, M))) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_point(aes(x=M), size=4) +
  geom_errorbar(aes(xmin=L, xmax=U), linewidth=0.5) +
  # theme(axis.text.y = element_text(size=8)) +
  ylab("player") +
  xlab("coefficient (entropy above average)")
ggsave("results_plot_player_level_effects.png", width=10, height=14)

##############################