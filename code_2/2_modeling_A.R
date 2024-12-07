
### SET YOUR WORKING DIRECTORY TO THIS FILE (IN `code_2`)
library(tidyverse)

#################
### READ DATA ###
#################

# read data
df_tracking_A = read_csv('../processed-data/df_tracking_A.csv', show_col_types = F)
df_tracking_A
object.size(df_tracking_A) / 10**9

# pre-snap tracking data for prediction/modeling exercise
df_tracking_presnap = 
  df_tracking_A |>
  filter(-10 < t_after_snap & t_after_snap < 0)
df_tracking_presnap
table(df_tracking_presnap$t_after_snap)

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

###############################
### DATAFRAME FOR MODELING  ###
###############################

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




# # read data
# WEEKS = 1:9 ### forreal dogg
# # WEEKS = 1:2 ## fo-shizzle just to make things faster & not crash me R
# weeks_str = paste0("_w",min(WEEKS),"-",max(WEEKS))
# df_tracking_2 = read_csv(paste0('../processed-data/df_ryan_tracking_2',weeks_str,'.csv'))
# 
# # tracking data, but just the safeties
# df_tracking_safeties = 
#   df_tracking_2 |>
#   # keep just the pre-snap safeties (for now)
#   filter(is_pre_safety) |>
#   select(-c(pos_official, is_pre_safety))
# df_tracking_safeties
# nrow(df_tracking_safeties %>% distinct(gameId, playId))
# table(df_tracking_safeties$t_after_snap)
# table(df_tracking_safeties$mofo_postsnap)
# # View(df_tracking_safeties[1:2000,])
# 
# # # final tracking dataset to save
# # write_csv(df_tracking_safeties, paste0('../processed-data/df_df_tracking_safeties',weeks_str,'.csv'))
# 
# 
