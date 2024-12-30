#cleaned visualizations:
setwd("C:/Users/justi/OneDrive/Penn/BDB(2025)")
library(tidyverse)
library(gganimate)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")
memory.limit(size=10000)


#these are the files related directly to the 
safety_movement1 <- read_csv('df_safety_movement_1.csv')
safety_movement2 <- read_csv('df_safety_movement_2.csv')
out_of_sample_preds <- read_csv('results_df_preds_outOfSample.csv')

#tracking the entire plays:
df_C_players <- read_csv('df_C_players.csv')
df_C_plays <- read_csv('df_C_plays.csv')
df_C_tracking <- read_csv('df_C_tracking.csv')
player_play <- read_csv('player_play.csv')
all_players <- read_csv('players.csv')
plays <- read_csv('plays.csv')

all_safety_movements_preds1 <- out_of_sample_preds %>%
  filter(num_safeties==1) %>%
  left_join(safety_movement1 %>%
              select(gameId,
                     playId,
                     #nflId,
                     #displayName,
                     minSafetyDistToMOF,
                     minSafetyHorizDistToBallLine,
                     los,
                     x_first,
                     y_first,
                     x_last,
                     y_last
                     #,
                     #x_first:y_spline_basis10
              ),
            by = c('gameId', 'playId')) %>%
  rename(x_first_p1 = x_first,
         y_first_p1 = y_first,
         x_last_p1 = x_last,
         y_last_p1 = y_last) %>%
  mutate(x_first_p2 = NA,
         y_first_p2 = NA,
         x_last_p2 = NA,
         y_last_p2 = NA)

all_safety_movements_preds2 <- out_of_sample_preds %>%
  filter(num_safeties==2) %>%
  left_join(safety_movement2 %>%
              select(gameId,
                     playId,
                     #nflId,
                     #displayName,
                     minSafetyDistToMOF,
                     minSafetyHorizDistToBallLine,
                     los,
                     x_first_p1,
                     y_first_p1,
                     x_last_p1,
                     y_last_p1,
                     x_first_p2,
                     y_first_p2,
                     x_last_p2,
                     y_last_p2#,
                     #x_first:y_spline_basis10
              ),
            by = c('gameId', 'playId'))

all_safety_movements_preds <- rbind(all_safety_movements_preds1, all_safety_movements_preds2)
###############################################################
## identify the most diff plays:
all_safety_movements_preds1 <- all_safety_movements_preds1 %>%
  mutate(pred_binned = cut(p, breaks = 20),
         x_first_binned = cut(x_first_p1, breaks = 5),
         y_first_binned = cut(y_first_p1, breaks = 5),
         x_last_binned = cut(x_last_p1, breaks = 5),
         y_last_binned = cut(y_last_p1, breaks = 5),
         # x_change = x_last - x_first,
         # y_change = y_last - y_first,
         # x_change_binned = cut(x_change, 10),
         # y_change_binned = cut(y_change, 10)
  )

all_safety_movements_preds2 <- all_safety_movements_preds2 %>%
  mutate(pred_binned = cut(p, breaks = 20),
         x_first_binned_p1 = cut(x_first_p1, breaks = 5),
         y_first_binned_p1 = cut(y_first_p1, breaks = 5),
         x_last_binned_p1 = cut(x_last_p1, breaks = 5),
         y_last_binned_p1 = cut(y_last_p1, breaks = 5),
         
         x_first_binned_p2 = cut(x_first_p2, breaks = 5),
         y_first_binned_p2 = cut(y_first_p2, breaks = 5),
         x_last_binned_p2 = cut(x_last_p2, breaks = 5),
         y_last_binned_p2 = cut(y_last_p2, breaks = 5),
         
  )

View(all_safety_movements_preds1 %>%
       group_by(
         x_first_binned,
         y_first_binned,
         x_last_binned,
         y_last_binned) %>%
       summarize(count = n(),
                 min_p = min(p),
                 max_p = max(p),
                 max_game = gameId[which.max(p)],
                 max_play = playId[which.max(p)],
                 min_game = gameId[which.min(p)],
                 min_play = playId[which.min(p)]) %>%
       ungroup())

View(all_safety_movements_preds2 %>%
       group_by(
         x_first_binned_p1,
         y_first_binned_p1,
         x_last_binned_p1,
         y_last_binned_p1,
         x_first_binned_p2,
         y_first_binned_p2,
         x_last_binned_p2,
         y_last_binned_p2) %>%
       summarize(count = n(),
                 min_p = min(p),
                 max_p = max(p),
                 max_game = gameId[which.max(p)],
                 max_play = playId[which.max(p)],
                 min_game = gameId[which.min(p)],
                 min_play = playId[which.min(p)]
       ) %>%
       ungroup())


#############################################################
# data for the animations:
all_data_joined <- all_safety_movements_preds %>%
  left_join(df_C_tracking %>%
              # filter(gameId %in% c(games_for_comp) &
              #          playId %in% c(plays_for_comp)) %>%
              select(playId,
                     gameId,
                     nflId,
                     displayName,
                     t_after_snap,
                     x,
                     y,
                     s,
                     a,
                     o,
                     dir), by = c('gameId', 'playId'))
rm(df_C_tracking)

all_data_joined <- all_data_joined %>%
  left_join(player_play %>% 
              # filter(gameId %in% c(games_for_comp) &
              #          playId %in% c(plays_for_comp)) %>%
              select(gameId,
                     playId,
                     nflId,
                     teamAbbr), by = c('gameId', 'playId', 'nflId')) %>%
  left_join(plays %>%
              select(gameId,
                     playId,
                     playDescription),
            by = c('gameId', 'playId'))

all_data_joined <- all_data_joined %>%
  mutate(club = case_when(
    displayName=='football' ~ 'football',
    displayName==displayName_p1 ~ 'Safety #1',
    displayName==displayName_p2 ~ 'Safety #2',
    teamAbbr==possessionTeam ~ 'Offense',
    teamAbbr==defensiveTeam ~ 'Defense'
  ),
  t_after_snap=round(t_after_snap,3))

tracking_week_3 <- data.table::fread("tracking_week_3.csv")
Week3_NamesAndNumbers <- tracking_week_3 %>% select(c(nflId, displayName, jerseyNumber))
Week3_NamesAndNumbers <- unique(Week3_NamesAndNumbers)
rm(tracking_week_3)

tracking_week_2 <- data.table::fread("tracking_week_2.csv")
Week2_NamesAndNumbers <- tracking_week_2 %>% select(c(nflId, displayName, jerseyNumber))
Week2_NamesAndNumbers <- unique(Week2_NamesAndNumbers)
rm(tracking_week_2)

names_and_nums <- rbind(Week2_NamesAndNumbers, Week3_NamesAndNumbers) %>%
  unique()
#81%
#anim_save('high_prob_2.gif', anim_func(all_data_joined,game=2022091804, play=2572))

#18%:
#anim_save('low_prob_2.gif',anim_func(all_data_joined,game=2022092200, play=2112))

all_data_joined <- all_data_joined %>%
  group_by(gameId, playId) %>%
  mutate(first_t = min(t_after_snap)) %>%
  mutate(Period = case_when(
    t_after_snap< -3 ~ 'Before Model',
    (t_after_snap>=-3 & t_after_snap<=-0.1) | (t_after_snap>=first_t & t_after_snap<=-0.1) ~ 'Model',
    t_after_snap>=0 ~ 'Play in Progress'
  )) %>%
  mutate(start_indicator = ifelse(t_after_snap==-3 | (t_after_snap==first_t & first_t>-3), 'Model Start', 'Other')) %>%
  mutate(start_of_model_time = ifelse(first_t> -3, first_t, -3)) %>%
  ungroup()


sample_data <- all_data_joined %>%
  filter(gameId==2022091804,
         playId==2572)


sample_data <- sample_data %>%
  left_join(names_and_nums, by = c('nflId', 'displayName'))

#adding in pauses:
sample_data <- sample_data %>%
  mutate(show_time = case_when(start_indicator == 'Model Start' | t_after_snap==-0.1 ~ 100,
                               TRUE ~ 1),
         text_for_anim = case_when(
           t_after_snap<start_of_model_time ~ 'Before Line is Set',
           t_after_snap==start_of_model_time ~ 'Start of Model Window',
           t_after_snap>start_of_model_time & t_after_snap< -0.1 ~'Tracking Safety Movement',
           t_after_snap==-0.1 ~ 'End of Model Window',
           t_after_snap>-0.1 ~ 'Play in Progress'
         )) %>%
  uncount(show_time) %>%
  group_by(gameId,
           playId,
           nflId) %>%
  mutate(reveal_time = row_number()) %>%
  ungroup()

View(sample_data %>%
       filter(club %in% c('Safety #1', 'Safety #2')))


plot_title <- paste0(sample_data$playDescription[1], 
                     '\n', 'Probability: ', round(100*sample_data$p[1], 1), '%',
                     '\n', 'MOFO Open: ',sample_data$mofo_postsnap[1])

anim <- ggplot() +
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 120) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank())

high_prob <- anim +
  geom_point(data = sample_data,
             aes(x = x,
                 y = y,
                 colour = club,
                 group = nflId,
                 size = club)) +
  geom_line(data = sample_data %>%
              filter(club %in% c('Safety #1', 'Safety #2')),
            aes(x = x,
                y = y,
                group = club),
            color = 'red')+
  geom_text(data = sample_data,aes(x = 60, y = 55, label = text_for_anim), color = 'white') +
  geom_text(data = sample_data, aes(x = x, y = y, group = nflId, label = jerseyNumber), colour = "white",
            vjust = 0.36, size = 4.5) +
  scale_color_manual(values = c('Offense' = "dodgerblue", 'Defense'="red", 'football'="brown", 'Safety #1'='navyblue', 'Safety #2'='navyblue'))+
  scale_size_manual(values = c('Offense' = 6, 'Defense' = 6, 'football' = 4, 'Safety #1' = 7, 'Safety #2' = 7)) +
  transition_reveal(reveal_time)  +
  labs(title = plot_title)

anim_save('high_prob_2.gif', high_prob)

unique(sample_data$nflId)  
