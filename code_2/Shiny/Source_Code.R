setwd("C:/Users/justi/OneDrive/Penn/BDB(2025)")
library(tidyverse)
library(gganimate)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")


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
#View(safety_movement1)

# try clustering on the single safety preds:
set.seed(0)

# function to compute total within-cluster sum of square 
wss <- function(df, k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 2:15

clustering_dat1 <- safety_movement1 %>%
  select(gameId, playId,x_first:y_spline_basis10)

# extract wss for 2-15 clusters using sapply 
wss_values <- sapply(k.values, 
                     function(k) kmeans(clustering_dat1[, !colnames(clustering_dat1) %in% c('gameId', 'playId')], centers = k)$tot.withinss)

#let's try three clusters:
clusters4 <- kmeans(clustering_dat1[, !colnames(clustering_dat1) %in% c('gameId', 'playId')], centers = 3)
# or use map_dbl()
#wss_values <- map_dbl(k.values, function(k) wss(payroll[,-1], k))  
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

########################
#adding probs and clusters:
clustering_dat1 <- clustering_dat1 %>%
  left_join(out_of_sample_preds %>% select(playId, gameId,  p, expectedPoints, winProbability, displayName_p1, displayName_p2), by = c('playId', 'gameId'))

clustering_dat1$cluster <- clusters4$cluster

# clustering_dat1 %>%
#   ggplot(aes(x = p)) +
#   geom_density() +
#   facet_wrap(~cluster)

#let's try PCA and clustering:
pca1 <- prcomp(clustering_dat1 %>% select(x_first:y_spline_basis10), scale. = F) # no scaling
pca1$rotation[, 1:2]

#add first two PC scores:
clustering_dat1 <- clustering_dat1 %>%
  cbind(pca1$x[,1:2])

# clustering_dat1 %>%
#   ggplot(aes(x = PC1, y = PC2)) +
#   geom_point(aes(color =p))


# hierarchical clustering:
# first, cluster based on splines; then, within those clusters, cluster on the other four variables
h_clusts <- hclust(dist(clustering_dat1 %>% select(x_first:y_spline_basis10)), method = 'average')
plot(h_clusts)


####################################
## Let's identify plays that are similar in their probabilities and the movements presnap:

clustering_dat1 <- clustering_dat1 %>%
  mutate(pred_binned = cut(p, breaks = 20),
         x_first_binned = cut(x_first, breaks = 10),
         y_first_binned = cut(y_first, breaks = 10),
         x_last_binned = cut(x_last, breaks = 10),
         y_last_binned = cut(y_last, breaks = 10),
         x_change = x_last - x_first,
         y_change = y_last - y_first,
         x_change_binned = cut(x_change, 10),
         y_change_binned = cut(y_change, 10))

# View(clustering_dat1 %>%
#        group_by(
#                 x_first_binned,
#                 y_first_binned,
#                 x_last_binned,
#                 y_last_binned) %>%
#        summarize(count = n(),
#                  min_p = min(p),
#                  max_p = max(p)) %>%
#        ungroup())

# this is the clustering combination with the most plays in the safety # 1 file:
# max prob is ~30%, while the min prob is ~#12%; let's compare those plays
# View(clustering_dat1 %>%
#        filter(x_first_binned=='(14.8,16.7]' &
#               y_first_binned=='(-0.918,1.32]' &
#               x_last_binned=='(14.1,16.1]' &
#               y_last_binned=='(-0.313,1.84]'))

games_for_comp <- c(2022100908, 2022092503, 2022092505)
plays_for_comp <- c(1233, 2783, 348)

#joining in the tracking data/other play dfs:
all_dat_joined <- clustering_dat1 %>%
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
                     dir), by = c('gameId', 'playId')) %>%
  left_join(df_C_players %>%
              # filter(gameId %in% c(games_for_comp) &
              #          playId %in% c(plays_for_comp)) %>%
              select(gameId,
                     playId,
                     nflId,
                     pos_official,
                     posGroup,
                     is_pre_safety,
                     x_postsnap,
                     y_postsnap), by = c('gameId', 'playId', 'nflId')) %>%
  left_join(df_C_plays %>%
              filter(gameId %in% c(games_for_comp) &
                       playId %in% c(plays_for_comp)) %>%
              select(gameId,
                     playId,
                     defensiveTeam,
                     possessionTeam,
                     expectedPoints,
                     winProbability,
                     xpass,
                     down,
                     yardsToGo,
                     time_left_half,
                     pre_score_diff,
                     num_safeties,
                     ball_y_snap,
                     los,
                     mofo_postsnap), by = c('gameId', 'playId')) %>%
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

all_dat_joined <- all_dat_joined %>%
  mutate(club = case_when(
    displayName=='football' ~ 'football',
    displayName==displayName_p1 ~ 'Safety #1',
    displayName==displayName_p2 ~ 'Safety #2',
    teamAbbr==possessionTeam ~ 'Offense',
    teamAbbr==defensiveTeam ~ 'Defense'
  ),
  t_after_snap=round(t_after_snap,3))
  #mutate(teamAbbr=ifelse(displayName=='football', 'football', teamAbbr))


comp_df <- all_dat_joined %>%
  filter(gameId %in% c(games_for_comp) &
           playId %in% c(plays_for_comp))


# let's plot the animation
#attributes used for plot. first is away, second is football, third is home.
cols_fill <- c('Offense' = "dodgerblue", 'Defense'="red", 'football'="brown", 'Safety #1'='navyblue', 'Safety #2'='darkgreen')
cols_col <- c('Offense' = "dodgerblue", 'Defense'="red", 'football'="brown", 'Safety #1'='navyblue', 'Safety #2'='darkgreen')
size_vals <- c(6 , 6, 4, 8, 8)
shape_vals <- c(21,21, 16, 21, 21)
#plot_title <- sample_ceedee_pass$playDescription
#nFrames <- max(sample_ceedee_pass$frameId)

#setting the field:
anim <- ggplot() +
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 122) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank()) +
  
  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = F) + 
  scale_shape_manual(values = shape_vals, guide = F) +
  scale_fill_manual(values = cols_fill) + 
  scale_colour_manual(values = cols_col) 

anim_func <- function(dataset, description) {
  
  data_for_viz <- dataset %>%
    filter(playDescription==description) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(frameId=row_number()) %>%
    ungroup()
  
  plot_title <- paste0(data_for_viz$playDescription[1], 
                       '\n', 'Probability: ', round(100*data_for_viz$p[1], 3), '%',
                       '\n', 'MOFO Open: ',data_for_viz$mofo_postsnap[1])
  
  anim +
  #adding players
  geom_point(data = data_for_viz, aes(x = x,
                                            y = y,
                                            shape = club,
                                            fill = club,
                                            #group = nflId,
                                            size = club,
                                            colour = club),
             alpha = 0.7) +

    #adding jersey numbers

    #titling plot with play description
    #setting animation parameters
    transition_time(t_after_snap)  +
    ease_aes("linear") +
    labs(title = plot_title,
         caption = 'T: {frame_time}') 
  
}

anim_func(dataset = all_dat_joined, description = '(9:31) M.Mariota pass incomplete deep left to F.Franks.')
# anim_func(dataset = comp_df, game = games_for_comp[1], play = plays_for_comp[1])  
# animate(anim_func(dataset = comp_df, game = games_for_comp[2], play = plays_for_comp[2]), duration = 30)
# animate(anim_func(dataset = comp_df, game = games_for_comp[3], play = plays_for_comp[3]), duration = 30)
# 
# 
# df_for_an<-comp_df %>%
#   filter(gameId==games_for_comp[1] & playId==plays_for_comp[1]) %>%
#   group_by(gameId, playId, nflId) %>%
#   mutate(frameId=row_number()) %>%
#   ungroup()
# 
# unique(df_for_an$teamAbbr)


# View(df_for_an)

# View(out_of_sample_preds %>%
#        filter(gameId==games_for_comp[1],
#               playId==plays_for_comp[1]))
# View(df_for_an %>%
#        filter(displayName=='Antoine Winfield'))
