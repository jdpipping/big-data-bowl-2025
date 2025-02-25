library(tidyverse)

# Stats_ByFullPlay_Final was defined in the "Aggregating Frames to Plays" GitHub file
# Stats_ByFullPlay_All9Weeks is a CSV with same format, defined on JP's computer (see jp-full-9-weeks)

# Read in data if necessary: 
Stats_ByFullPlay_All9Weeks <- read_csv("Stats_ByFullPlay_All9Weeks.csv")

colnames(Stats_ByFullPlay_All9Weeks) # recall that Snap_Entropy is the continuous play-level one (HighEntropy is binary)

# Before we make these matrices, adjust Min_PreSnap_X_vel_component_AnySafety and Min_PreSnap_X_acc_component_AnySafety
# This is to get rid of the "double negative" idea ... in other words, make a high velocity toward the ball show up as a positive number
Stats_ByFullPlay_9Weeks <- Stats_ByFullPlay_9Weeks %>% 
  mutate(Max_PreSnap_X_vel_TowardBall_AnySafety = -1 * Min_PreSnap_X_vel_component_AnySafety)
Stats_ByFullPlay_9Weeks <- Stats_ByFullPlay_9Weeks %>% 
  mutate(Max_PreSnap_X_acc_TowardBall_AnySafety = -1 * Min_PreSnap_X_acc_component_AnySafety)
Stats_ByFullPlay_9Weeks <- Stats_ByFullPlay_9Weeks %>% select(-c("Min_PreSnap_X_vel_component_AnySafety", "Min_PreSnap_X_acc_component_AnySafety"))

# Repeat the same idea to avoid the "double negative" that we used for pre-snap vertical velocity
# I.e., now the vertical velocity will show up as positive if the safety is moving closer to LOS at the snap
Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>% 
  mutate(Max_X_vel_TowardBall_AnySafety_AtSnap = -1 * Min_X_vel_component_AnySafety_AtSnap)
Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>% 
  mutate(Min_X_vel_TowardBall_AnySafety_AtSnap = -1 * Max_X_vel_component_AnySafety_AtSnap)
Stats_ByFullPlay_All9Weeks <- Stats_ByFullPlay_All9Weeks %>% select(-c("Max_X_vel_component_AnySafety_AtSnap", "Min_X_vel_component_AnySafety_AtSnap"))

Stats_ByFullPlay_2High <- Stats_ByFullPlay_All9Weeks %>% filter(num_safeties_pre_snap == 2)
Stats_ByFullPlay_1High <- Stats_ByFullPlay_All9Weeks %>% filter(num_safeties_pre_snap == 1)
# mean(Stats_ByFullPlay_2High$PostSnap_MOF_Num); it's 0.5016
# mean(Stats_ByFullPlay_1High$PostSnap_MOF_Num); it's 0.1559
# mean(Stats_ByFullPlay_2High$MOFO_probability_FDA); it's 0.4976
# mean(Stats_ByFullPlay_1High$MOFO_probability_FDA); it's 0.1581

###
#FIXME MANUALLY ADD THE 3 SEPARATE X AXES...
# facet_x1 = "Vertical Stagger \nb/w Safeties at Snap (Yards)"
# facet_x2 = "Difference in Vert Velocities \nb/w Safeties at Snap (Yds/Sec)"
# facet_x3 = "Highest Vertical 'Creep' Distance \nby Safety (Yards)"
facet_x1 = "Vertical Stagger \nb/w Safeties at Snap"
facet_x2 = "Difference in Vert Velocities \nb/w Safeties at Snap"
facet_x3 = "Highest Vertical 'Creep' Distance \nby Safety"
caption_text = paste0(
  # str_remove_all(facet_x1,"\n"), ": ", " XX,\n",
  # str_remove_all(facet_x2,"\n"), ": ", " XX,\n",
  # str_remove_all(facet_x3,"\n"), ": ", " XX."
  "Vert Velocities: toward LOS is negative, away from LOS is positive\n",
  "'Creep' Distance: safety's distance moved forward b/w OL Set and Snap\n",
  "Vertical Stagger: vert distance b/w safeties at time of snap"
)
final_plot_A = 
  Stats_ByFullPlay_2High %>%
  select(
    MOFO_probability_FDA, 
    PostSnap_MOF_Num,
    X_Diff_BetweenSafeties_AtSnap,
    X_Vel_Diff_BetweenSafeties_AtSnap,
    Max_VertCreptDistance_AnySafety
  ) %>%
  pivot_longer(-c(PostSnap_MOF_Num,MOFO_probability_FDA), names_to="var", values_to = "x") %>%
  mutate(
    featureLab = case_when(
      var == "X_Diff_BetweenSafeties_AtSnap" ~ facet_x1,
      var == "X_Vel_Diff_BetweenSafeties_AtSnap" ~ facet_x2,
      var == "Max_VertCreptDistance_AnySafety" ~ facet_x3,
    ),
  ) %>%
  filter(
    (var == "X_Diff_BetweenSafeties_AtSnap" & 0 <= x & x <= 10) |
    (var == "X_Vel_Diff_BetweenSafeties_AtSnap" & 0 <= x & x <= 9) |
    (var == "Max_VertCreptDistance_AnySafety" & -4 <= x & x <= 8) 
  ) %>%
  ggplot() +
  facet_wrap(~featureLab, scales="free_x") +
  stat_smooth(mapping = 
                aes(
                  x = x, y = MOFO_probability_FDA, 
                  # color = "Estimated \nMOFO \nProbability\n"
                  color = "Estimated MOFO Probability"
                ),
              method = "gam", se = F, span = 0.75, linewidth=2) +
  stat_smooth(mapping = 
                aes(
                  x = x, y = PostSnap_MOF_Num, 
                  # color = "Observed \nMOFO \nRate\n"
                  color = "Observed MOFO Rate"
                ),
              method.args = list(family = "binomial"), 
              method = "gam", se = F, span = 0.75, linewidth=2) +
  scale_x_continuous(breaks = seq(-10, 10, 2)) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  scale_color_manual(values = c("firebrick2", "dodgerblue2")) +
  theme_bw() + 
  labs(
    x = "",
    # x = "Feature",
   y = "MOFO Probability",
   title = "How Estimated & Observed MOFO Rate Depend on Key Features",
   caption = caption_text,
   subtitle = "Early Down 2-High Dropbacks, 5+ Yards to Go: Weeks 1-9, 2022 (n = 3029)",
   # color = "color"
  ) +
  theme(
    plot.title = element_text(size = 30, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 28),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 22),
    axis.text.x = element_text(size = 22),
    strip.text.x  = element_text(size = 20),
    legend.key.width = unit(3, "line"),
    legend.text = element_text(size = 25),
    # legend.title = element_text(size = 25),
    legend.title=element_blank(),
    plot.caption.position = "plot",
    plot.caption = element_text(size = 16, hjust = 0),
    panel.spacing = unit(2, "lines"),
    legend.position = "bottom"
  )
# final_plot_A
ggsave("Cole_2High_Features_3Panel.png", 
       final_plot_A,width=15, height=6.5)


