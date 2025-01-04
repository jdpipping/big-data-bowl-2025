
### Code for MOFO Probability Modeling

* `1_data.R` - generate datasets `df_C_plays.csv`, `df_C_players.csv`, and `df_C_tracking.csv`, stored in the folder `../processed-data`, which includes tracking data from all players on all relevant dropback plays where MOFO/MOFC is defined with 1 or 2 safeties from the time the line is set through the end of the play.

* `2_modeling_A.R` - prediction contest & MOFO/MOFC probability modeling

* `3_results_entropy.R` - results & plots, e.g. mean safety entropy for each defensive team

