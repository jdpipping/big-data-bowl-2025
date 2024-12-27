setwd("C:/Users/justi/OneDrive/Penn/BDB(2025)")
library(tidyverse)
library(gganimate)
source("https://raw.githubusercontent.com/mlfurman3/gg_field/main/gg_field.R")

# Recall that tracking_std was defined in the data cleaning file
tracking_std = read_csv('processed-data/tracking_std.csv')

# If needed, can read in MergedData and Dropbacks_Merged files that were most recently tweaked in Vectorized_Distances file
MergedData <- read_csv("MergedData.csv")
Dropbacks_Merged <- read_csv("Dropbacks_Merged.csv")

######## THESE ARE THE EXAMPLES THAT WILL ACTUALLY BE USED IN THE PROJECT ################
# This is the best "all-22 dots" method, and it uses original tracking data, and it includes green background and other cool details
# We don't use Dropbacks_Merged here, because we want the football to be included (that's why we read in all the smaller CSVs)

# these are files related directly to the animation code (get these from Google Drive if needed)
df_safety_movement_1 <- read_csv('df_safety_movement_1.csv') # Recall this refers to plays with 1 pre-snap safety
df_safety_movement_2 <- read_csv('df_safety_movement_2.csv') # 2 pre-snap safeties
out_of_sample_preds <- read_csv('results_df_preds_outOfSample.csv') # Neural network model's predictions

# tracking the entire plays:
df_C_players <- fread('df_C_players.csv')
df_C_plays <- fread('df_C_plays.csv')
df_C_tracking <- fread('df_C_tracking.csv')
# the following three files are the raw files given to us from the Kaggle data
player_play <- fread('player_play.csv') 
players <- fread('players.csv')
plays <- fread('plays.csv')
# View(df_safety_movement_1)

# Do the entire process for 1-high and 2-high plays separately
# Start with tracking data, then add the safety movement ... but DON'T join on nflId, b/c one DF only includes safeties
# Change df_safety_movement_1 column names to reflect nflId and displayName just referring to safeties
df_safety_movement_1 <- df_safety_movement_1 %>% rename(nflId_p1 = `nflId`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(displayName_p1 = `displayName`)

# Also add frameId to df_C_tracking
df_C_tracking <- df_C_tracking %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(frameId = rank(t_after_snap, ties.method = "first")) %>%
  ungroup() 
table(df_C_tracking$frameId)

# And also add the jersey numbers by incorporating the original tracking data ... this example was Week 3
tracking_week_3 <- fread("tracking_week_3.csv")
Week3_NamesAndNumbers <- tracking_week_3 %>% select(c(nflId, displayName, jerseyNumber))
Week3_NamesAndNumbers <- unique(Week3_NamesAndNumbers)
df_C_tracking_1 <- df_C_tracking %>% left_join(Week3_NamesAndNumbers, by = c("nflId", "displayName"))  
rm(tracking_week_3, Week3_NamesAndNumbers)

all_dat_joined_1 <- df_C_tracking_1 %>% 
  left_join(df_safety_movement_1, by = c('gameId', 'playId'))

# Now pick the specific play(s) we want
all_dat_joined_1 <- all_dat_joined_1 %>% filter(gameId %in% c(2022092507) & playId %in% c(1836))
  
# Then add MOFO probability for the eventual data visualization
all_dat_joined_1 <- all_dat_joined_1 %>%
  left_join(out_of_sample_preds %>% select(playId, gameId, p, expectedPoints, winProbability), by = c('playId', 'gameId'))

# Now join in the other data frames 
# BUT first, establish which games/plays we want data visualizations for
# 1-high: gameId == 2022092507, playId == 1836
all_dat_joined_1 <- all_dat_joined_1 %>% 
  left_join(df_C_players %>%
              filter(gameId %in% c(2022092507) &
                       playId %in% c(1836)) %>%
              select(gameId,
                     playId,
                     nflId,
                     pos_official,
                     posGroup,
                     is_pre_safety,
                     x_postsnap,
                     y_postsnap), by = c('gameId', 'playId', 'nflId')) %>%
  left_join(df_C_plays %>%
              filter(gameId %in% c(2022092507) &
                       playId %in% c(1836)) %>%
              select(gameId,
                     playId,
                     xpass,
                     down,
                     yardsToGo,
                     time_left_half,
                     pre_score_diff,
                     ball_y_snap,
                     mofo_postsnap), by = c('gameId', 'playId', 'mofo_postsnap')) %>%
  left_join(player_play %>% 
              filter(gameId %in% c(2022092507) &
                       playId %in% c(1836)) %>%
              select(gameId,
                     playId,
                     nflId,
                     teamAbbr), by = c('gameId', 'playId', 'nflId')) %>%
  left_join(plays %>%
              select(gameId,
                     playId,
                     playDescription),
            by = c('gameId', 'playId'))

all_dat_joined_1 <- all_dat_joined_1 %>%
  mutate(club = case_when(
    displayName == 'football' ~ 'football',
    displayName == displayName_p1 ~ 'Safety #1', # Recall that only the 2-high plays have displayName_p1 and displayName_p2
    # Could add a separate one here for ball-carrier
    teamAbbr == possessionTeam ~ 'Offense',
    teamAbbr == defensiveTeam ~ 'Defense'
  ),
  t_after_snap = round(t_after_snap, 3))
# mutate(teamAbbr = ifelse(displayName == 'football', 'football', teamAbbr))

# Also add a verbal "MOFO" column for graph purposes
all_dat_joined_1 <- all_dat_joined_1 %>% 
  mutate(PostSnap_MOF = ifelse(!is.na(mofo_postsnap) & mofo_postsnap %in% 1, "MOF Open",
                               ifelse(!is.na(mofo_postsnap) & mofo_postsnap %in% 0, "MOF Closed", NA)))

# attributes used for plot. first is away, second is football, third is home
cols_fill <- c('Offense' = "black", 'Defense'="blue", 'football'="brown", 'Safety #1'='red') #, 'Safety #2'='red')
cols_col <- c('Offense' = "black", 'Defense'="blue", 'football'="brown", 'Safety #1'='red') #, 'Safety #2'='red')
size_vals <- c(8, 6, 8, 8)
shape_vals <- c(21, 16, 21, 21)
plot_title <- all_dat_joined_1[1, "playDescription"]
nFrames <- max(all_dat_joined_1$frameId)

#setting the field:
anim <- ggplot() +
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 120) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank()) +
  
  # setting size and color parameters
  scale_size_manual(values = size_vals, guide = F) + 
  scale_shape_manual(values = shape_vals, guide = F) +
  scale_fill_manual(values = cols_fill) + 
  scale_colour_manual(values = cols_col) 

anim_func <- function(dataset, play, game) {
  
  data_for_viz <- dataset %>%
    filter(gameId==game & playId==play) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(frameId=row_number()) %>%
    ungroup()
  
  plot_title <- paste0(data_for_viz$playDescription[1], 
                       '\n', 'MOFO Probability: ', round(100*data_for_viz$p[1], 2), '%',
                       '\n', 'Pre-Snap Safeties (in Red): ',data_for_viz$num_safeties[1],
                       '\n', 'Actual MOFO vs. MOFC: ',data_for_viz$PostSnap_MOF[1],
                       '\n', 'Coverage Scheme: ', 'Tampa 2')
  
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
    
    # adding jersey numbers
    geom_text(data = all_dat_joined_1, aes(x = x, y = y, label = jerseyNumber), colour = "white",
              vjust = 0.36, size = 4.5) +
    
    #titling plot with play description
    #setting animation parameters
    transition_time(t_after_snap)  +
    ease_aes("linear") +
    labs(title = plot_title,
         caption = 'Time Since Snap: {frame_time}')  +
    theme(plot.title = element_text(size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 10, hjust = 0.5),
          plot.caption = element_text(size = 8))
  
}

comp_df <- all_dat_joined_1 %>%
  filter(gameId %in% c(2022092507) &
           playId %in% c(1836))

anim_func(dataset = comp_df, game = 2022092507, play = 1836)  
# animate(anim_func(dataset = comp_df, game = 2022092507, play = 1836), duration = 30)
# animate(anim_func(dataset = comp_df, game = 2022092507, play = 1836), duration = 30)

# Now here's how to save it as a GIF
animation_1 <- anim_func(dataset = comp_df, game = 2022092507, play = 1836)

# Use animate to create the GIF object
gif_animation_1 <- animate(animation_1, 
                         nframes = nFrames,       # Number of frames
                         fps = 10,               # Frames per second
                         width = 800,            # Width of the output GIF
                         height = 600,           # Height of the output GIF
                         renderer = gifski_renderer())

# Save the GIF to a file
anim_save("play_animation_1.gif", animation = gif_animation_1)

# Now repeat that entire process for 2-high plays
# Start with tracking data, then add the safety movement ... but DON'T join on nflId, b/c one DF only includes safeties
# Don't need to change df_safety_movement_2 column names to reflect nflId and displayName just referring to safeties
# And also add the jersey numbers by incorporating the original tracking data ... this example was Week 1
tracking_week_1 <- fread("tracking_week_1.csv")
Week1_NamesAndNumbers <- tracking_week_1 %>% select(c(nflId, displayName, jerseyNumber))
Week1_NamesAndNumbers <- unique(Week1_NamesAndNumbers)
df_C_tracking_2 <- df_C_tracking %>% left_join(Week1_NamesAndNumbers, by = c("nflId", "displayName"))  
rm(tracking_week_1, Week1_NamesAndNumbers)

all_dat_joined_2 <- df_C_tracking_2 %>% 
  left_join(df_safety_movement_2, by = c('gameId', 'playId'))

# Now pick the specific play(s) we want
all_dat_joined_2 <- all_dat_joined_2 %>% filter(gameId %in% c(2022090800) & playId %in% c(1504))

# Then add MOFO probability for the eventual data visualization
all_dat_joined_2 <- all_dat_joined_2 %>%
  left_join(out_of_sample_preds %>% select(playId, gameId, p, expectedPoints, winProbability), by = c('playId', 'gameId'))

# Now join the other DFs in
# 2-high: gameId == 2022090800, playId == 1504
all_dat_joined_2 <- all_dat_joined_2 %>% 
  left_join(df_C_players %>%
              filter(gameId %in% c(2022090800) &
                       playId %in% c(1504)) %>%
              select(gameId,
                     playId,
                     nflId,
                     pos_official,
                     posGroup,
                     is_pre_safety,
                     x_postsnap,
                     y_postsnap), by = c('gameId', 'playId', 'nflId')) %>%
  left_join(df_C_plays %>%
              filter(gameId %in% c(2022090800) &
                       playId %in% c(1504)) %>%
              select(gameId,
                     playId,
                     xpass,
                     down,
                     yardsToGo,
                     time_left_half,
                     pre_score_diff,
                     ball_y_snap,
                     mofo_postsnap), by = c('gameId', 'playId', 'mofo_postsnap')) %>%
  left_join(player_play %>% 
              filter(gameId %in% c(2022090800) &
                       playId %in% c(1504)) %>%
              select(gameId,
                     playId,
                     nflId,
                     teamAbbr), by = c('gameId', 'playId', 'nflId')) %>%
  left_join(plays %>%
              select(gameId,
                     playId,
                     playDescription),
            by = c('gameId', 'playId'))

all_dat_joined_2 <- all_dat_joined_2 %>%
  mutate(club = case_when(
    displayName == 'football' ~ 'football',
    displayName == displayName_p1 ~ 'Safety #1',
    displayName == displayName_p2 ~ 'Safety #2',
    # Could add a separate one here for ball-carrier
    teamAbbr == possessionTeam ~ 'Offense',
    teamAbbr == defensiveTeam ~ 'Defense'
  ),
  t_after_snap = round(t_after_snap,3))
#mutate(teamAbbr = ifelse(displayName == 'football', 'football', teamAbbr))

# Also add a verbal "MOFO" column for graph purposes
all_dat_joined_2 <- all_dat_joined_2 %>% 
  mutate(PostSnap_MOF = ifelse(!is.na(mofo_postsnap) & mofo_postsnap %in% 1, "MOF Open",
                               ifelse(!is.na(mofo_postsnap) & mofo_postsnap %in% 0, "MOF Closed", NA)))

# attributes used for plot. first is away, second is football, third is home
cols_fill <- c('Offense' = "red", 'Defense'="blue", 'football'="brown", 'Safety #1'='goldenrod', 'Safety #2'='goldenrod')
cols_col <- c('Offense' = "red", 'Defense'="blue", 'football'="brown", 'Safety #1'='goldenrod', 'Safety #2'='goldenrod')
size_vals <- c(8, 6, 8, 8, 8)
shape_vals <- c(21, 16, 21, 21, 21)
plot_title <- all_dat_joined_2[1, "playDescription"]
nFrames <- max(all_dat_joined_2$frameId)

#setting the field:
anim <- ggplot() +
  
  #creating field underlay
  gg_field(yardmin = 0, yardmax = 120) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank()) +
  
  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = F) + 
  scale_shape_manual(values = shape_vals, guide = F) +
  scale_fill_manual(values = cols_fill) + 
  scale_colour_manual(values = cols_col) 

anim_func <- function(dataset, play, game) {
  
  data_for_viz <- dataset %>%
    filter(gameId==game & playId==play) %>%
    group_by(gameId, playId, nflId) %>%
    mutate(frameId=row_number()) %>%
    ungroup()
  
  plot_title <- paste0(data_for_viz$playDescription[1], 
                       '\n', 'MOFO Probability: ', round(100*data_for_viz$p[1], 2), '%',
                       '\n', 'Pre-Snap Safeties (in Gold): ',data_for_viz$num_safeties[1],
                       '\n', 'Actual MOFO vs. MOFC: ',data_for_viz$PostSnap_MOF[1],
                       '\n', 'Coverage Scheme: ', 'Cover 3 Sky')
  
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
    
    # adding jersey numbers
    geom_text(data = all_dat_joined_2, aes(x = x, y = y, label = jerseyNumber), colour = "white",
              vjust = 0.36, size = 4.5) +
    
    #titling plot with play description
    #setting animation parameters
    transition_time(t_after_snap)  +
    ease_aes("linear") +
    labs(title = plot_title,
         caption = 'Time Since Snap: {frame_time}') + 
    theme(plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        plot.caption = element_text(size = 8))
  
}

comp_df <- all_dat_joined_2 %>%
  filter(gameId %in% c(2022090800) &
           playId %in% c(1504))

anim_func(dataset = comp_df, game = 2022090800, play = 1504)  
# animate(anim_func(dataset = comp_df, game = 2022090800, play = 1504), duration = 30)
# animate(anim_func(dataset = comp_df, game = 2022090800, play = 1504), duration = 30)

# Now here's how to save it as a GIF
animation_2 <- anim_func(dataset = comp_df, game = 2022090800, play = 1504)

# Use animate to create the GIF object
gif_animation_2 <- animate(animation_2, 
                         nframes = nFrames,       # Number of frames
                         fps = 10,               # Frames per second
                         width = 800,            # Width of the output GIF
                         height = 600,           # Height of the output GIF
                         renderer = gifski_renderer())

# Save the GIF to a file
anim_save("play_animation_2.gif", animation = gif_animation_2)

rm(df_C_players, df_C_plays, df_C_tracking, df_C_tracking_1, df_C_tracking_2, df_safety_movement_1, df_safety_movement_2, out_of_sample_preds, all_dat_joined_1, all_dat_joined_2, comp_df)

######## DIFFERENT ANIMATION EXAMPLES THAT AREN'T DIRECTLY USED IN FINAL PROJECT #########

# extract example play: https://www.youtube.com/watch?v=2mPxPOjnAg0
example_play_Davis_TD = tracking_std |> 
  filter(gameId == 2022100901, playId == 117) |> 
  mutate(color = case_when(club == 'PIT' ~ 'gold',
                           club == 'BUF' & displayName %in% "Gabe Davis" ~ 'green',
                           club == 'BUF' & !displayName %in% "Gabe Davis" ~ 'blue',
                           club == 'football' ~ 'brown'))

# play visualization
play_animation_Davis_TD = example_play_Davis_TD |> 
  ggplot() +
  geom_point(aes(x = x, y = y, color = color, size = 3)) +
  # ensure colors display as assigned
  scale_color_identity() +
  # omit size from legend ... COMMENT OUT THE guides() LINE IF ANIMATE() FUNCTION DOESN'T WORK
  guides(size = FALSE) +
  theme_minimal() +
  transition_time(frameId) +
#   scale_x_continuous(breaks = seq(0, 120, 20)) +
#   scale_y_continuous(breaks = seq(0, 60, 10)) +
  labs(x = " ", y = " ",
       title = "Josh Allen 98-Yard TD Pass to Gabe Davis",
       subtitle = "First Quarter of Bills' 38-3 win over Steelers in Week 5, 2022",
       caption = "Data provided by Kaggle") +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5)) 
# Could add geom_abline() for each team's goal line, LOS, etc. if/when we want to

# show animation
animate(play_animation_Davis_TD, nframes = max(example_play_Davis_TD$frameId), fps = 10, renderer = gifski_renderer())

# With a different method, here's code for the all-22 moving dots of an Isaiah McKenzie reception
# Sample plays and corresponding visualizations
set.seed(1128)

# This could be replaced by whichever play you want to look into
# This example is an Isaiah McKenzie 12-yard reception from Josh Allen against Bills in Week 1, 2022

# We could use Dropbacks_Merged, like this, but then the football wouldn't be included
# This is b/c we got rid of the football when making TrackingWithStats_PlayerNames in data cleaning file
# example_play <- Dropbacks_Merged %>% filter(gameId == 2022090800 & playId == 617)

# But here's how to do it from the original tracking data CSV
# Recall that tracking data has Rams' tricode as LA, not LAR (for some reason)
example_play = tracking_week_1 %>%
  filter(gameId == 2022090800 & playId == 617) %>%
  mutate(team_color = case_when(club == 'LA' ~ 'goldenrod',
                           club == 'BUF' & displayName %in% "Isaiah McKenzie" ~ 'green',
                           club == 'BUF' & !displayName %in% "Isaiah McKenzie" ~ 'blue',
                           club == 'football' ~ 'brown'))

example_game_id <- unique(example_play$gameId)
example_play_id <- unique(example_play$playId)
# games <- read_csv("games.csv")
# players <- read_csv("players.csv")
# plays <- read_csv("plays.csv")
#
# merging games data to play
# example_play <- inner_join(example_play,
#                            games,
#                            by = c("gameId" = "gameId")) #gets teams involved, using gameID as an index
#
# merging tracking data to play
# example_play <- inner_join(example_play,
#                            Dropbacks_Merged,
#                            by = c("gameId" = "gameId",
#                                   "playId" = "playId"))

# If we were operating from MergedData, here's how we would get the plot_title
# plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))

# If we aren't operating from MergedData, we can simply type the title manually
plot_title <- "J. Allen 12-yard completion to I. McKenzie in Week 1, 2022 at Rams"

xmin <- 0
xmax <- 53.3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

# Specific boundaries for a given play
ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)

#hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

# plotting
ggplot() +
  
  #setting size and color parameters
  scale_size_manual(values = c(6, 6, 6, 6), guide = FALSE) +
  scale_shape_manual(values = c(21, 21, 21, 21), guide = FALSE) +
  scale_fill_manual(values = c("goldenrod" = "goldenrod", "green" = "green", "blue" = "blue", "brown" = "brown"), guide = TRUE) +
  # scale_colour_manual(values = c("goldenrod" = "goldenrod", "green" = "green", "blue" = "blue", "brown" = "brown"), guide = FALSE) +
  # COMMENT OUT THIS guides() function if plot isn't displaying
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  
  # adding hash marks
  annotate("text", x = df.hash$x[df.hash$x < 55/2],
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) +
  annotate("text", x = df.hash$x[df.hash$x > 55/2],
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
  
  # adding yard lines
  annotate("segment", x = xmin,
           y = seq(max(10, ymin), min(ymax, 110), by = 5),
           xend =  xmax,
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) +
  
  # adding field yardline text
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10),
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"),
           angle = 270, size = 4) +
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10),
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),
           angle = 90, size = 4) +
  
  # adding field exterior
  annotate("segment", x = c(xmin, xmin, xmax, xmax),
           y = c(ymin, ymax, ymax, ymin),
           xend = c(xmin, xmax, xmax, xmin),
           yend = c(ymax, ymax, ymin, ymin), colour = "black") +
  
  # adding players
  geom_point(data = example_play, aes(x = (xmax-y),
                                      y = x,
                                      shape = club,
                                      fill = team_color, # if we used Dropbacks_Merged, this would say Player_Role
                                      group = team_color, # if we used Dropbacks_Merged, this would say Player_Role
                                      size = club),
             alpha = 0.7) +  
  # ggforce::geom_circle(data = example_play, aes(x0=X_ball_carrier, y0=Y_ball_carrier, r=10)) +
  
  # adding jersey numbers
  geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white",
            vjust = 0.36, size = 3.5) +
  
  # applying plot limits
  ylim(ymin, ymax) +
  coord_fixed() +
  #  scale_x_continuous(breaks = seq(0, 60, 20)) +
  #  scale_y_continuous(breaks = seq(0, 100, 20)) +
  
  # applying theme
  # theme_nothing() +
  theme(plot.title = element_text(),
        legend.position = "right") +
  
  # titling plot with play description
  labs(title = plot_title,
       fill = 'Player Role') +
  
  # setting animation parameters
  transition_time(frameId, time = 0.1)  +
  ease_aes('linear') +
  NULL

########################### Some early versions of animation code below ##########################
################################ ALONG WITH HOW TO CREATE CLUSTERS ###############################

# these are files related directly to the animation code
df_safety_movement_1 <- read_csv('df_safety_movement_1.csv')
df_safety_movement_2 <- read_csv('df_safety_movement_2.csv')
out_of_sample_preds <- read_csv('results_df_preds_outOfSample.csv')

# tracking the entire plays:
df_C_players <- read_csv('df_C_players.csv')
df_C_plays <- read_csv('df_C_plays.csv')
df_C_tracking <- read_csv('df_C_tracking.csv')
# the following three files are the raw files given to us from the Kaggle data
player_play <- read_csv('player_play.csv') 
players <- read_csv('players.csv')
plays <- read_csv('plays.csv')
#View(df_safety_movement_1)

# try clustering on the single safety preds:
set.seed(0)

# function to compute total within-cluster sum of square 
wss <- function(df, k) {
  kmeans(df, k, nstart = 10)$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 2:15

clustering_dat1 <- df_safety_movement_1 %>%
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

clustering_dat1 %>%
  ggplot(aes(x = p)) +
  geom_density() +
  facet_wrap(~cluster)

#let's try PCA and clustering:
pca1 <- prcomp(clustering_dat1 %>% select(x_first:y_spline_basis10), scale. = F) # no scaling
pca1$rotation[, 1:2]

#add first two PC scores:
clustering_dat1 <- clustering_dat1 %>%
  cbind(pca1$x[,1:2])

clustering_dat1 %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color =p))


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

View(clustering_dat1 %>%
       group_by(
                x_first_binned,
                y_first_binned,
                x_last_binned,
                y_last_binned) %>%
       summarize(count = n(),
                 min_p = min(p),
                 max_p = max(p)) %>%
       ungroup())

# this is the clustering combination with the most plays in the safety # 1 file:
# max prob is ~30%, while the min prob is ~12%; let's compare those plays
View(clustering_dat1 %>%
       filter(x_first_binned == '(14.8,16.7]' &
              y_first_binned == '(-0.918,1.32]' &
              x_last_binned == '(14.1,16.1]' &
              y_last_binned == '(-0.313,1.84]'))

games_for_comp <- c(2022100908, 2022092503, 2022092505)
plays_for_comp <- c(1233, 2783, 348)

#joining in the tracking data/other play dfs:
all_dat_joined <- clustering_dat1 %>%
  left_join(df_C_tracking %>%
              filter(gameId %in% c(games_for_comp) &
                       playId %in% c(plays_for_comp)) %>%
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
              filter(gameId %in% c(games_for_comp) &
                       playId %in% c(plays_for_comp)) %>%
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
              filter(gameId %in% c(games_for_comp) &
                       playId %in% c(plays_for_comp)) %>%
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
    displayName == 'football' ~ 'football',
    displayName == displayName_p1 ~ 'Safety #1',
    displayName == displayName_p2 ~ 'Safety #2',
    teamAbbr == possessionTeam ~ 'Offense',
    teamAbbr == defensiveTeam ~ 'Defense'
  ),
  t_after_snap = round(t_after_snap,3))
  #mutate(teamAbbr = ifelse(displayName == 'football', 'football', teamAbbr))


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
  gg_field(yardmin = 0, yardmax = 120) +
  
  #filling forest green for behind back of endzone
  theme(panel.background = element_rect(fill = "forestgreen",
                                        color = "forestgreen"),
        panel.grid = element_blank()) +
  
  #setting size and color parameters
  scale_size_manual(values = size_vals, guide = F) + 
  scale_shape_manual(values = shape_vals, guide = F) +
  scale_fill_manual(values = cols_fill) + 
  scale_colour_manual(values = cols_col) 

anim_func <- function(dataset, play, game) {
  
  data_for_viz <- dataset %>%
    filter(gameId==game & playId==play) %>%
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

anim_func(dataset = comp_df, game = games_for_comp[1], play = plays_for_comp[1])  
animate(anim_func(dataset = comp_df, game = games_for_comp[2], play = plays_for_comp[2]), duration = 30)
animate(anim_func(dataset = comp_df, game = games_for_comp[3], play = plays_for_comp[3]), duration = 30)

# This data frame tests Antonio Winfield specifically
df_for_an<-comp_df %>%
  filter(gameId==games_for_comp[1] & playId==plays_for_comp[1]) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(frameId=row_number()) %>%
  ungroup()

unique(df_for_an$teamAbbr)

View(df_for_an)

View(out_of_sample_preds %>%
       filter(gameId==games_for_comp[1],
              playId==plays_for_comp[1]))
View(df_for_an %>%
       filter(displayName=='Antoine Winfield'))
