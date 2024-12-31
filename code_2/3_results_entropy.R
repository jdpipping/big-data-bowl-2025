
#################
### LIBRARIES ###
#################

### SET YOUR WORKING DIRECTORY TO THIS FILE (IN `code_2`)
library(tidyverse)
library(glmnet)
library(nflplotR)

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
  read_csv("results_df_preds.csv", show_col_types = F) %>%
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
df_eval1 =
  df_eval0 %>% 
  left_join(
    all_pids %>% select(nflId_p1 = nflId, pid1 = pid)
  ) %>%
  left_join(
    all_pids %>% select(nflId_p2 = nflId, pid2 = pid)
  ) %>%
  mutate(pid2 = ifelse(is.na(pid2), 0, pid2)) %>% # label NAs as player 0
  mutate(tid = as.numeric(as.factor(defensiveTeam)))
df_eval1

# add play-level EPA from nflfastr data
nflverse_pbp <- nflfastR::load_pbp(2022)
nflverse_pbp <- nflverse_pbp %>% filter(week %in% 1:9)
nflverse_pbp1 = 
  nflverse_pbp %>%
  mutate(gameId = as.numeric(old_game_id), playId = as.numeric(play_id)) %>%
  select(gameId, playId, epa)
nflverse_pbp1
df_eval = 
  df_eval1 %>%
  left_join(nflverse_pbp1) %>%
  relocate(epa, .after = entropy)
df_eval

# num plays
nplays = nrow(df_eval)
nplays
# num games
ngames = length(unique(df_eval$gameId))
ngames
# num dropbacks per team per game
nplays / ngames * 32
nplays / ngames / 2
16 * 0.04

################################
### DEFENSIVE SAFETY ENTROPY ###
################################

plot_entropy_graph =
  tibble(p = seq(0,1,length.out=250)) %>%
  mutate(entropy = -p*log(p,base=2) - (1-p)*log(1-p,base=2)) %>%
  mutate(entropy = ifelse(p==0,0,entropy)) %>%
  mutate(entropy = ifelse(p==1,0,entropy)) %>%
  ggplot(aes(x=p,y=entropy)) +
  geom_line(linewidth=1) +
  labs(title = "entropy versus p")
ggsave("results_plot_entropy_v_prob.png",plot_entropy_graph,width=6,height=5)

# # plot mean entropy by team
# df_team_mean_entropy =
#   df_eval %>%
#   group_by(defensiveTeam) %>%
#   reframe(mean_entropy = mean(entropy)) %>%
#   arrange(mean_entropy) 
# df_team_mean_entropy
# plot_team_mean_entropy = 
#   df_team_mean_entropy %>%
#   ggplot(aes(x = mean_entropy, y = reorder(defensiveTeam, mean_entropy))) +
#   geom_point(size=4) +
#   ylab("defensive team") +
#   xlab("mean entropy")
# plot_team_mean_entropy
# # ggsave("results_plot_team_mean_entropy.png", width=6, height=8)

# but is it significant?

# bootstrap
B = 100
boot_lst = list()
for (b in 1:B) {
  print(paste0("b=",b,"/",B))
  set.seed(234*b+8374)
  idxs_b = sort(sample(1:nrow(df_eval), size = nrow(df_eval), replace = T))
  df_b = df_eval[idxs_b,]
  df_b
  df_eval_b = 
    df_b %>%
    arrange(defensiveTeam) %>%
    group_by(defensiveTeam) %>%
    reframe(mean_entropy = mean(entropy)) %>%
    mutate(b = b)
  df_eval_b
  boot_lst[[length(boot_lst)+1]] = df_eval_b
}
boot_lst
df_boot = do.call(rbind, boot_lst)
df_boot_result = 
  df_boot %>%
  group_by(defensiveTeam) %>%
  reframe(
    mean_entropy_L = quantile(mean_entropy, 0.05),
    mean_entropy_M = quantile(mean_entropy, 0.50),
    mean_entropy_U = quantile(mean_entropy, 0.95),
  ) %>%
  left_join(df_team_mean_entropy)
df_boot_result

# plot mean entropy by team with boot CI
plot_team_mean_entropy_boot =
  df_boot_result %>% 
  arrange(-mean_entropy) %>%
  mutate(
    rank = rank(-mean_entropy),
    y = reorder(defensiveTeam, mean_entropy),
    y_rank = paste0(y," (",rank,")"),
    y_rank = reorder(y_rank, mean_entropy)
  ) %>%
  ggplot(aes(x = mean_entropy, y = y_rank, team_abbr = y)) +
  geom_errorbar(aes(xmin = mean_entropy_L, xmax = mean_entropy_U)) +
  geom_nfl_logos(width=0.08) +
  ylab("defensive team") +
  xlab("mean safety entropy") +
  labs(title = "Ranking teams by safety entropy")
# plot_team_mean_entropy_boot
ggsave("results_plot_team_mean_entropy_boot.png", width=6, height=9)

# plot_team_mean_entropy_boot_1 =
#   df_boot_result %>% 
#   arrange(-mean_entropy) %>%
#   mutate(
#     rank = rank(-mean_entropy),
#     y = reorder(defensiveTeam, mean_entropy),
#     y_rank = paste0(y," (",rank,")"),
#     y_rank = reorder(y_rank, mean_entropy)
#   ) %>%
#   ggplot(aes(x = mean_entropy, y = y_rank, team_abbr = y)) +
#   geom_errorbar(aes(xmin = mean_entropy_L, xmax = mean_entropy_U)) +
#   geom_nfl_logos(width=0.08) +
#   geom_text(aes(x = mean_entropy_U+0.02, label = paste0(round(mean_entropy,2)))) +
#   ylab("defensive team") +
#   xlab("mean safety entropy") +
#   labs(title = "Ranking teams by safety entropy")
# # plot_team_mean_entropy_boot_1
# ggsave("results_plot_team_mean_entropy_boot1.png", width=6, height=9)

#######################################################
### correlation between safety entropy and EPA/play ###
#######################################################

#
df_epa_entropy = 
  df_eval %>%
  group_by(defensiveTeam) %>%
  reframe(mean_entropy = mean(entropy), epa_per_play = mean(epa)) %>%
  arrange(-mean_entropy)
df_epa_entropy

#
sd(df_epa_entropy$epa_per_play)
sd(df_epa_entropy$epa_per_play)*2
df_epa_entropy %>% 
  mutate(numSdsFromMean_epa = abs(epa_per_play - mean(epa_per_play))/sd(epa_per_play)) %>% 
  arrange(-numSdsFromMean_epa)

# correlations
dfs_for_lm = list(
  df_epa_entropy,
  # df_epa_entropy %>% filter(!(defensiveTeam %in% c("PHI"))),
  df_epa_entropy %>% filter(!(defensiveTeam %in% c("PHI", "LV")))
)
for (j in 1:length(dfs_for_lm)) {
  df_for_lm = dfs_for_lm[[j]]
  
  cor_epa_entropy = cor(df_for_lm$mean_entropy, df_for_lm$epa_per_play)
  cor_epa_entropy
  m1 = lm(epa_per_play~mean_entropy, data=df_for_lm)
  m1
  
  # plot
  df_plot_cor = 
    df_for_lm %>%
    ggplot(aes(team_abbr = defensiveTeam, x = mean_entropy, y = epa_per_play)) +
    ###
    annotate("text", x = 0.7, y = 0.15, size=6, color="firebrick",
             label = paste0("corr = ", round(cor_epa_entropy,2))) +
    ggplot2::geom_abline(slope = coef(m1)[2], intercept = coef(m1)[1],
                         linewidth=1.5, color="gray20", linetype="longdash") +
    ###
    nflplotR::geom_mean_lines(aes(x0 = mean_entropy , y0 = epa_per_play)) +
    geom_nfl_logos(width=0.05) +
    annotate(
      "segment", x = 0.62, xend = 0.69, y = -0.16, yend = -0.16, 
      arrow = arrow(type = "closed", length = unit(0.4, "cm")),
      color = "firebrick"
    ) +
    annotate(
      "text", x = 0.62, y = -0.15, hjust=0, vjust = 0.1, size = 4,
      label = "more unpredictable safeties", color = "firebrick",
    ) +
    annotate(
      "segment", x = 0.51, xend = 0.51, y = 0.02, yend = -0.17, 
      arrow = arrow(type = "closed", length = unit(0.4, "cm")),
      color = "firebrick"
    ) +
    annotate(
      "text", x = 0.51, y = -0.15, hjust=0, vjust = -0.5, size = 4,
      label = "more efficient defenses", color = "firebrick", angle = 90
    ) +
    labs(title = "Defensive Success vs. Mean Safety Entropy") +
    ylab("EPA/play") +
    xlab("mean safety entropy")
  ggsave(paste0("results_plot_epa_entropy_corr",j,".png"), df_plot_cor, width=8, height=5)
  
  # # plot
  # if (j == 1) {
  #   df_plot_cor = 
  #     df_for_lm %>%
  #     ggplot(aes(team_abbr = defensiveTeam, x = mean_entropy, y = epa_per_play)) +
  #     # ###
  #     # annotate("text", x = 0.825, y = 0.19, size=6, color="firebrick",
  #     #          label = paste0("corr = ", round(cor_epa_entropy,2))) +
  #     # ggplot2::geom_abline(slope = coef(m1)[2], intercept = coef(m1)[1],
  #     #                      linewidth=1.5, color="gray20", linetype="longdash") +
  #     # ###
  #     nflplotR::geom_mean_lines(aes(x0 = mean_entropy , y0 = epa_per_play)) +
  #     geom_nfl_logos(width=0.05) +
  #     annotate(
  #       "segment", x = 0.78, xend = 0.86, y = -0.16, yend = -0.16, 
  #       arrow = arrow(type = "closed", length = unit(0.4, "cm")),
  #       color = "firebrick"
  #     ) +
  #     annotate(
  #       "text", x = 0.78, y = -0.15, hjust=0, vjust = 0.1, size = 4,
  #       label = "more unpredictable safeties", color = "firebrick",
  #     ) +
  #     annotate(
  #       "segment", x = 0.63, xend = 0.63, y = 0.15, yend = -0.16, 
  #       arrow = arrow(type = "closed", length = unit(0.4, "cm")),
  #       color = "firebrick"
  #     ) +
  #     annotate(
  #       "text", x = 0.63, y = -0.15, hjust=0, vjust = -0.5, size = 4,
  #       label = "more efficient defenses", color = "firebrick", angle = 90
  #     ) +
  #     ylab("EPA/play") +
  #     xlab("mean safety entropy")
  #   ggsave(paste0("results_plot_epa_entropy_corr_A",j,".png"), df_plot_cor, width=8, height=5)
  # }
  
}

###########################################################################
### EPA/play on high entropy plays versus EPA/play on low entropy plays ###
###########################################################################

# entropy functions
entropy <- function(p) -p*log(p,2) - (1-p)*log(1-p,2)
ent_to_p <- function(E) {
  p_cutoff_L = uniroot(function(p) entropy(p) - E, c(0.00000001, 0.5))$root
  p_cutoff_U = uniroot(function(p) entropy(p) - E, c(0.5, 0.9999999))$root
  p_cutoffs = c(p_cutoff_L, p_cutoff_U)
  p_cutoffs
  # entropy(p_cutoffs)
}

### select entropy cutoffs
# hist(df_eval$entropy)
# quantile(df_eval$entropy, seq(0,1,by=0.1))
entropy_cutoff_L = 0.81; entropy_cutoff_U = 0.81;
# entropy_cutoff_L = 0.625; entropy_cutoff_U = 0.94;

ent_to_p(entropy_cutoff_L)
ent_to_p(entropy_cutoff_U)

### select EPA cutoff for def of successful play
# hist(df_eval$epa)
# quantile(df_eval$epa, seq(0,1,by=0.1))
q_epa = 0
# q_epa = -0.25

df_epa_entropy_1A =
  df_eval %>%
  mutate(ent_bin = ifelse(entropy > entropy_cutoff_U, "highEntropy", 
                   ifelse(entropy <= entropy_cutoff_L, "lowEntropy", NA))) %>%
  drop_na(ent_bin) %>%
  # filter(num_safeties == 1) %>% #FIXME
  group_by(defensiveTeam, ent_bin) %>%
  mutate(n_plays = n()) %>%
  reframe(
    mean_entropy = mean(entropy), 
    epa_per_play = mean(epa), 
    prop_succ = mean(epa<q_epa),
    n_plays = n(), 
  ) %>%
  arrange(defensiveTeam, ent_bin)
df_epa_entropy_1A

df_epa_entropy_1B = 
  df_epa_entropy_1A %>%
  group_by(defensiveTeam) %>%
  reframe(
    diff_epa_per_play = diff(-epa_per_play),
    diff_prop_succ = diff(-prop_succ),
  ) %>%
  arrange(diff_epa_per_play) %>%
  mutate(i=1:n()) %>%
  relocate(i, .after=defensiveTeam)
df_epa_entropy_1B

# want diff to be negative
# hist(df_epa_entropy_1B$diff_epa_per_play)
mean(df_epa_entropy_1B$diff_epa_per_play)
median(df_epa_entropy_1B$diff_epa_per_play)
sum(df_epa_entropy_1B$diff_epa_per_play)
mean(df_epa_entropy_1B$diff_epa_per_play < 0)
sum(df_epa_entropy_1B$diff_epa_per_play < 0)

# want diff to be positive
# hist(df_epa_entropy_1B$diff_prop_succ)
mean(df_epa_entropy_1B$diff_prop_succ)
median(df_epa_entropy_1B$diff_prop_succ)
sum(df_epa_entropy_1B$diff_prop_succ)

# # plot
# df_epa_entropy_1B %>%
#   select(-i) %>%
#   pivot_longer(-defensiveTeam, names_to="measure") %>%
#   group_by(measure) %>%
#   mutate(
#     mean = mean(value),
#     med = median(value),
#     m = case_when(
#       measure == "diff_epa_per_play" ~ "EPA/Play diff",
#       measure == "diff_prop_succ" ~ "Play Success Proportion Diff",
#     )
#   ) %>%
#   ggplot(aes(x = value)) +
#   geom_histogram(fill="black") +
#   geom_vline(xintercept=0, linetype="dashed",color="red", linewidth=1) +
#   geom_vline(aes(xintercept=mean),color="cyan", linewidth=1) +
#   # geom_vline(aes(xintercept=med),color="green", linewidth=1) +
#   facet_wrap(~m, scales = "free_x")

# # plot
# df_epa_entropy_1C =
#   df_epa_entropy_1A %>%
#   select(-c(mean_entropy)) %>%
#   pivot_wider(id_cols = defensiveTeam, names_from = ent_bin, values_from = -c(defensiveTeam, ent_bin))
# df_epa_entropy_1C
# 
# df_plot_cor_lowHighEntropy =
#   df_epa_entropy_1C %>%
#   ggplot(aes(team_abbr = defensiveTeam, x = epa_per_play_lowEntropy, y = epa_per_play_highEntropy)) +
#   # nflplotR::geom_mean_lines(aes(x0 = epa_per_play_lowEntropy , y0 = epa_per_play_highEntropy)) +
#   # ggplot2::geom_abline(slope = 1, intercept = 0, color="dodgerblue2", linetype="longdash") +
#   ggplot2::geom_abline(slope = 1, intercept = 0, color="red", linetype="longdash") +
#   geom_nfl_logos(width=0.05) +
#   annotate(
#     "segment", x =  0.32, xend = 0.08, y = -0.6, yend = -0.6,
#     arrow = arrow(type = "closed", length = unit(0.4, "cm")),
#     color = "firebrick"
#   ) +
#   annotate(
#     "text", x =  0.1, y = -0.6, hjust=0, vjust = -0.5, size = 4,
#     label = "better defensive outcomes", color = "firebrick"
#   ) +
#   annotate(
#     "segment", x =  -0.3, xend = -0.3, y = -.05, yend = -0.6,
#     arrow = arrow(type = "closed", length = unit(0.4, "cm")),
#     color = "firebrick"
#   ) +
#   annotate(
#     "text", x = -0.3, y = -0.55, hjust=0, vjust = -0.5, size = 4,
#     label = "better defensive outcomes", color = "firebrick", angle = 90
#   ) +
#   labs(title="Defensive Performance on High- vs. Low-Entropy Plays") +
#   xlab(paste0("EPA/play on low entropy plays")) +
#   ylab(paste0("EPA/play on high entropy plays"))
# ggsave(paste0("results_plot_epa_lowHighEntropy_corr.png"), df_plot_cor_lowHighEntropy, width=8, height=5)


