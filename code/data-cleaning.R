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
  require(tidyverse, quietly = T)
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
  require(tidyverse, quietly = T)
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

#############################
### BACK TO CLEANING DATA ###
#############################

# Note that the first frame is typically huddle_break_offense: View(tracking_std[1:5000, ])
# But not always: View(tracking_std %>% filter(frameId == 1 & !event %in% "huddle_break_offense"))
FirstFrame_NotHuddleBreak <- tracking_std %>% filter(frameId == 1 & !event %in% "huddle_break_offense")
# Just glance at a few samples ... They often start with NA, then line_set comes a few frames later
# View(tracking_std %>% filter(gameId == 2022091200 & playId == 741 | gameId == 2022091102 & playId == 322 | gameId == 2022091101 & playId == 1785 |  gameId == 2022090800 & playId == 3190 | gameId == 2022091811 & playId == 2348 | gameId == 2022091805 & playId == 79))
# But sometimes first frame is line_set, or huddle_start_offense, or ball_snap, or man_in_motion
table(FirstFrame_NotHuddleBreak$event)

# View(tracking_std %>% filter(is.na(frameId))) - this is empty, as it should be 

# First, find out if the minimum frameId on any given play is always 1, or if there are data entry errors where a play starts w/ higher frameId
OpeningFrame_EachPlay <- tracking_std %>%
  group_by(gameId, playId, nflId, displayName) %>%
  mutate(Frame_Rank = rank(frameId, ties.method = "first")) %>%
  ungroup() 
OpeningFrame_EachPlay <- OpeningFrame_EachPlay %>% filter(Frame_Rank == 1)
OpeningFrame_EachPlay <- OpeningFrame_EachPlay %>% select(-"Frame_Rank")
table(OpeningFrame_EachPlay$frameId) # it's always 1, so there are no "late frameId" errors

# Now that we know this, use group_by() to mutate tracking_std so every frame is labelled with the event from that play's first frame
# In other words, did this play's tracking data begin w/ huddle_break_offense, or ball_snap, or sommething else?
OpeningFrame_Event <- OpeningFrame_EachPlay %>% select("playId", "gameId", "nflId", "displayName", "event")
OpeningFrame_Event <- OpeningFrame_Event %>% rename(Frame1_Event = `event`)
  
tracking_std <- merge(x = tracking_std, y = OpeningFrame_Event, 
                          by = c("playId", "gameId", "nflId", "displayName"))
tracking_std <- tracking_std %>% arrange(gameId, playId, nflId, frameId)
table(tracking_std$Frame1_Event)
rm(FirstFrame_NotHuddleBreak, OpeningFrame_EachPlay, OpeningFrame_Event)
