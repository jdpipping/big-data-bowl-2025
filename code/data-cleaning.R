#################
### LIBRARIES ###
#################

# install.packages('gganimate')
library(gganimate)
# install.packages('gifski')
library(gifski)
# install.packages('tidyverse')
library(tidyverse)

#################
### FUNCTIONS ###
#################

# read tracking data
read_tracking = function(weeks) {
  # require tidyverse
  require(tidyverse)
  # initialize tracking
  tracking = tibble()
  # loop through each week
  for (week in weeks) {
    # construct the file path
    file_path = paste0('raw-data/tracking_week_', week, '.csv') 
    # read week data in
    tracking_week <- read_csv(file_path)
    # append week data to tracking
    tracking = bind_rows(tracking, tracking_week)
  }
  # return final tracking
  return(tracking)
}

# standardize tracking data
standardize_tracking = function(tracking) {
  # require tidyverse
  require(tidyverse)
  # field standardization
  tracking_std = tracking |> 
    mutate(
      # make all plays go from left to right
      x = ifelse(playDirection == 'left', 120 - x, x),
      y = ifelse(playDirection == 'left', 160 / 3 - y, y),
      # flip player direction, orientation
      dir = ifelse(playDirection == 'left', (dir + 180) %% 360, dir %% 360),
      o = ifelse(playDirection == 'left', (o + 180) %% 360, o %% 360)
    )
  # return standardized tracking
  return(tracking_std)
}

###########################
### CLEAN TRACKING DATA ###
###########################

# # read in data
# tracking_all = read_tracking(weeks = 1:9)
# # write to file
# write_csv(tracking_all, 'raw-data/tracking_all.csv')
# 
# # standardize tracking data
# tracking_std = standardize_tracking(tracking_all)
# # save to file
# write_csv(tracking_std, 'processed-data/tracking_std.csv')

# read cleaned data
tracking_std = read_csv('processed-data/tracking_std.csv')

##########################
### DATA VISUALIZATION ###
##########################

# extract example play: https://www.youtube.com/watch?v=2mPxPOjnAg0
example_play = tracking_std |> 
  filter(gameId == 2022100901, playId == 117) |> 
  mutate(color = case_when(club == 'PIT' ~ 'gold',
                           club == 'BUF' ~ 'blue',
                           club == 'football' ~ 'brown'))

# play visualization
play_animation = example_play |> 
  ggplot() +
  geom_point(aes(x = x, y = y, color = color, size = 3)) +
  # ensure colors display as assigned
  scale_color_identity() +
  # omit size from legend
  guides(size = FALSE) +
  theme_minimal() +
  transition_time(frameId)
# show animation
animate(play_animation, nframes = max(example_play$frameId), fps = 10, renderer = gifski_renderer())