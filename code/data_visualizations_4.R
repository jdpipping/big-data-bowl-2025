# Recall that tracking_std was defined in the data cleaning file
tracking_std = read_csv('processed-data/tracking_std.csv')

# extract example play: https://www.youtube.com/watch?v=2mPxPOjnAg0
example_play_Davis_TD = tracking_std |> 
  filter(gameId == 2022100901, playId == 117) |> 
  mutate(color = case_when(club == 'PIT' ~ 'gold',
                           club == 'BUF' ~ 'blue',
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
example_play <- MergedData %>% filter(gameId == 2022090800 & playId == 617)

example_game_id <- unique(example_play$gameId)
example_play_id <- unique(example_play$playId)
# games <- read_csv("games.csv")
# players <- read_csv("players.csv")
# plays <- read_csv("plays.csv")
#
# #merging games data to play
# example_play <- inner_join(example_play,
#                            games,
#                            by = c("gameId" = "gameId")) #gets teams involved, using gameID as an index
#
# #merging tracking data to play
# example_play <- inner_join(example_play,
#                            MergedData,
#                            by = c("gameId" = "gameId",
#                                   "playId" = "playId"))

example_play <- MergedData %>%
  filter(gameId == example_game_id & playId == example_play_id)
plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))

# NOTE: if we wanted to include the football, we could adjust the data cleaning file
# Specifically, would adjust when we created TrackingWithStats_PlayerNames

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

cols_fill <- c("#FB4F14", "#663300", "#A5ACAF")
cols_col <- c("#000000", "#663300", "#000000")

# plotting
ggplot() +
  
  #setting size and color parameters
  scale_size_manual(values = c(6, 6, 6), guide = FALSE) +
  scale_shape_manual(values = c(21, 21, 21), guide = FALSE) +
  scale_fill_manual(values = c("red", 'blue', "black"), guide = TRUE) +
  # COMMENT OUT THIS guides() function if plot isn't displaying
  guides(fill = guide_legend(override.aes = list(shape=21))) +
  # scale_colour_manual(values = c("red", 'blue', "pink"), guide = FALSE) +
  
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
                                      fill = Player_Role,
                                      group = Player_Role,
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
  transition_time(frameId)  +
  ease_aes('linear') +
  NULL
