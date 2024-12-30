#cleaned visualizations:
setwd("C:/Users/justi/OneDrive/Penn/BDB(2025)")
library(tidyverse)
library(gganimate)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")
memory.limit(size=10000)

# these are the files related directly to the visualizations
safety_movement1 <- read_csv('df_safety_movement_1.csv')
safety_movement2 <- read_csv('df_safety_movement_2.csv')
out_of_sample_preds <- read_csv('results_df_preds_outOfSample.csv')

# tracking the entire plays:
# df_C_players <- read_csv('df_C_players.csv')
# df_C_plays <- read_csv('df_C_plays.csv')
df_C_tracking <- read_csv('df_C_tracking.csv')
player_play <- read_csv('player_play.csv')
players <- read_csv('players.csv')
plays <- read_csv('plays.csv')

# Add frameId to df_C_tracking
df_C_tracking <- df_C_tracking %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(frameId = rank(t_after_snap, ties.method = "first")) %>%
  ungroup() 
table(df_C_tracking$frameId)

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

rm(all_safety_movements_preds1, all_safety_movements_preds2)

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

rm(plays, player_play)

all_data_joined <- all_data_joined %>%
  mutate(club = case_when(
    displayName == 'football' ~ 'football',
    displayName == displayName_p1 ~ 'Safety #1',
    displayName == displayName_p2 ~ 'Safety #2', # Make sure these lines are separate b/c we want different red lines for their paths
    teamAbbr == possessionTeam ~ 'Offense',
    teamAbbr == defensiveTeam ~ 'Defense'
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
rm(Week2_NamesAndNumbers, Week3_NamesAndNumbers)

# NOTE: high_prob is the Week 2 TB-NO play, i.e. gameId == 2022091804, playId == 2572
# low_prob is the Week 3 PIT-CLE play, i.e. gameId =- 2022092200, playId == 21112

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

sample_data_3 <- all_data_joined %>%
  filter(gameId==2022091804,
         playId==2572)

sample_data_3 <- sample_data_3 %>%
  left_join(names_and_nums, by = c('nflId', 'displayName'))

sample_data_4 <- all_data_joined %>%
  filter(gameId==2022092200,
         playId==2112)

sample_data_4 <- sample_data_4 %>%
  left_join(names_and_nums, by = c('nflId', 'displayName'))

rm(all_data_joined)

# Add verbal versions of PostSnap_MOF for graphing purposes
sample_data_3 <- sample_data_3 %>% 
  mutate(PostSnap_MOF = ifelse(!is.na(mofo_postsnap) & mofo_postsnap %in% 1, "MOF Open",
                               ifelse(!is.na(mofo_postsnap) & mofo_postsnap %in% 0, "MOF Closed", NA)))

sample_data_4 <- sample_data_4 %>% 
  mutate(PostSnap_MOF = ifelse(!is.na(mofo_postsnap) & mofo_postsnap %in% 1, "MOF Open",
                               ifelse(!is.na(mofo_postsnap) & mofo_postsnap %in% 0, "MOF Closed", NA)))

# adding in pauses, starting with sample_data_3:
sample_data_3 <- sample_data_3 %>%
  mutate(show_time = case_when(start_indicator == 'Model Start' | t_after_snap==-0.1 ~ 100,
                               TRUE ~ 1),
         text_for_anim = case_when(
           t_after_snap < start_of_model_time ~ 'Before Line is Set',
           t_after_snap == start_of_model_time ~ 'Start of Model Window',
           t_after_snap > start_of_model_time & t_after_snap< -0.1 ~'Tracking Safety Pre-Snap Movement',
           t_after_snap == -0.1 ~ 'End of Model Window (0.1 Sec Before Snap)',
           t_after_snap > -0.1 ~ 'Play in Progress'
         )) %>%
  uncount(show_time) %>%
  group_by(gameId,
           playId,
           nflId) %>%
  mutate(reveal_time = row_number()) %>%
  ungroup()

# nFrames <- max(sample_data_3$frameId)

plot_title <- paste0(sample_data_3$playDescription[1], 
                          '\n', 'MOFO Probability: ', round(100*sample_data_3$p[1], 1), '%',
                          '\n', 'Pre-Snap Safeties (in Gold): ',sample_data_3$num_safeties[1],
                          '\n', 'Actual MOFO vs. MOFC: ',sample_data_3$PostSnap_MOF[1],
                          '\n', 'Coverage Scheme: ', 'Tampa 2')

anim <- ggplot() +
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 120) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank())

  play_animation_3_pauses <- anim +
  geom_point(data = sample_data_3,
             aes(x = x,
                 y = y,
                 colour = club,
                 group = nflId,
                 size = club)) + 
    geom_line(data = sample_data_3 %>%
                filter(club %in% c('Safety #1', 'Safety #2')),
            aes(x = x,
                y = y,
                group = club),
            color = 'red')+
  geom_text(data = sample_data_3, aes(x = 60, y = 55, label = text_for_anim), color = 'white') +
    geom_text(aes(x = 100, y = -2, label = paste0("Time Since Snap: ", t_after_snap)), 
              data = sample_data_3, 
              inherit.aes = FALSE, 
              color = 'black') +
  geom_text(data = sample_data_3, aes(x = x, y = y, group = nflId, label = jerseyNumber), colour = "white",
            vjust = 0.36, size = 4.5) +
  scale_color_manual(values = c('Offense' = "red", 'Defense'="black", 'football'="brown", 'Safety #1'='goldenrod', 'Safety #2'='goldenrod')) +
    scale_fill_manual(values = c('Offense' = "red", 'Defense'="black", 'football'="brown", 'Safety #1'='goldenrod', 'Safety #2'='goldenrod')) +
    scale_size_manual(values = c('Offense' = 8, 'Defense' = 8, 'football' = 6, 'Safety #1'= 8, 'Safety #2'= 8)) +
    scale_shape_manual(values = c('Offense' = 21, 'Defense' = 21, 'football' = 16, 'Safety #1'= 21, 'Safety #2'= 21)) +
  transition_reveal(reveal_time) +
    labs(title = plot_title) + 
    ease_aes("linear") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), # Keep title centered
        plot.margin = unit(c(5, 0, 0, 0), "lines"), # Increase top margin if title is too high (first value in `c(...)`)
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        legend.position = "none") # obviously scrap this if we want to keep the "club" legend

gif_animation_3_pauses <- animate(play_animation_3_pauses, duration = 15)
# Note: we don't want duration to be 0.1 * amount of frames, because of the built-in pauses
  
anim_save('play_animation_3_pauses.gif', gif_animation_3_pauses)

rm(sample_data_3)

# Now repeat it all for sample_data_4
sample_data_4 <- sample_data_4 %>%
  mutate(show_time = case_when(start_indicator == 'Model Start' | t_after_snap==-0.1 ~ 100,
                               TRUE ~ 1),
         text_for_anim = case_when(
           t_after_snap < start_of_model_time ~ 'Before Line is Set',
           t_after_snap == start_of_model_time ~ 'Start of Model Window',
           t_after_snap > start_of_model_time & t_after_snap< -0.1 ~'Tracking Safety Pre-Snap Movement',
           t_after_snap == -0.1 ~ 'End of Model Window (0.1 Sec Before Snap)',
           t_after_snap > -0.1 ~ 'Play in Progress'
         )) %>%
  uncount(show_time) %>%
  group_by(gameId,
           playId,
           nflId) %>%
  mutate(reveal_time = row_number()) %>%
  ungroup()

# nFrames <- max(sample_data_4$frameId)

plot_title <- paste0(sample_data_4$playDescription[1], 
                     '\n', 'MOFO Probability: ', round(100*sample_data_4$p[1], 1), '%',
                     '\n', 'Pre-Snap Safeties (in Red): ',sample_data_4$num_safeties[1],
                     '\n', 'Actual MOFO vs. MOFC: ',sample_data_4$PostSnap_MOF[1],
                     '\n', 'Coverage Scheme: ', 'Cover 1')

anim <- ggplot() +
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 120) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank())

play_animation_4_pauses <- anim +
  geom_point(data = sample_data_4,
             aes(x = x,
                 y = y,
                 colour = club,
                 group = nflId,
                 size = club)) +
  geom_line(data = sample_data_4 %>%
              filter(club %in% c('Safety #1', 'Safety #2')),
            aes(x = x,
                y = y,
                group = club),
            color = 'black')+
  geom_text(data = sample_data_4,aes(x = 60, y = 55, label = text_for_anim), color = 'white') +
  geom_text(aes(x = 100, y = -2, label = paste0("Time Since Snap: ", t_after_snap)), 
            data = sample_data_4, 
            inherit.aes = FALSE, 
            color = 'black') +
  geom_text(data = sample_data_4, aes(x = x, y = y, group = nflId, label = jerseyNumber), colour = "white",
            vjust = 0.36, size = 4.5) +
  scale_color_manual(values = c('Offense' = "black", 'Defense'="orange", 'football'="brown", 'Safety #1'='red', 'Safety #2'='red'))+
  scale_fill_manual(values = c('Offense' = "black", 'Defense'="orange", 'football'="brown", 'Safety #1'='red', 'Safety #2'='red')) +
  scale_size_manual(values = c('Offense' = 8, 'Defense' = 8, 'football' = 6, 'Safety #1'= 8, 'Safety #2'= 8)) +
  scale_shape_manual(values = c('Offense' = 21, 'Defense' = 21, 'football' = 16, 'Safety #1'= 21, 'Safety #2'= 21)) +
  transition_reveal(reveal_time) +
  labs(title = plot_title) +
  ease_aes("linear") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), # Keep title centered
        plot.margin = unit(c(5, 0, 0, 0), "lines"), # Increase top margin if title is too high (first value in `c(...)`)
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12),
        legend.position = "none") # obviously scrap this if we want to keep the "club" legend

gif_animation_4_pauses <- animate(play_animation_4_pauses, duration = 15)
# Note: we don't want duration to be 0.1 * amount of frames, because of the built-in pauses

anim_save('play_animation_4_pauses.gif', gif_animation_4_pauses)

unique(sample_data_4$nflId)

rm(sample_data_4)



  
