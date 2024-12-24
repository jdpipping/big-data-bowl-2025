# If needed, can read in Dropbacks_Merged file that was defined earlier in vectorized distances file
Dropbacks_Merged <- read_csv("Dropbacks_Merged.csv")

# Likewise, here's how to read in the data that resulted from the neutral network model's code (see 2_modeling file within code_2 GitHub folder)
NN_model_results_DF <- fread("results_df_preds_outOfSample.csv")
# Recall that "p" means probability of MOFO coverage

# For the sake of formality, get the column names here to align with those of Dropbacks_Merged
colnames(NN_model_results_DF)
colnames(Dropbacks_Merged)
NN_model_results_DF <- NN_model_results_DF %>% rename(num_safeties_pre_snap = `num_safeties`)
NN_model_results_DF <- NN_model_results_DF %>% rename(defteam = `defensiveTeam`)
NN_model_results_DF <- NN_model_results_DF %>% rename(posteam = `possessionTeam`)
NN_model_results_DF <- NN_model_results_DF %>% rename(wp = `winProbability`)
NN_model_results_DF <- NN_model_results_DF %>% rename(PostSnap_MOF_Num = `mofo_postsnap`)
NN_model_results_DF <- NN_model_results_DF %>% rename(pre_snap_safety_1 = `nflId_p1`)
NN_model_results_DF <- NN_model_results_DF %>% rename(pre_snap_safety_2 = `nflId_p2`)
NN_model_results_DF <- NN_model_results_DF %>% rename(pre_snap_safety_1_name = `displayName_p1`)
NN_model_results_DF <- NN_model_results_DF %>% rename(pre_snap_safety_2_name = `displayName_p2`)

# Now merge() this to Dropbacks_Merged ... to keep it simple, can just limit the NN_model_results_DF to gameId, playId, and "p"
NN_DF_abridged <- NN_model_results_DF %>% select(c("gameId", "playId", "p"))
Dropbacks_Merged <- merge(x = Dropbacks_Merged, y = NN_DF_abridged, 
                          by = c("gameId", "playId"))
Dropbacks_Merged <- Dropbacks_Merged %>% rename(MOFO_probability_FDA = `p`)
rm(NN_DF_abridged)

setDT(Dropbacks_Merged)
setkey(Dropbacks_Merged, gameId, playId, nflId, frameId)
Dropbacks_Merged <- Dropbacks_Merged %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")
rm(Dropbacks_Merged)

# The goal is to explore why the model says what it says, and to do so, clustering will be important
# Practically, to do so, we will need to join the modeling CSV with df_safety_movement_1 and df_safety_movement_2 CSVs
df_safety_movement_1 <- read_csv("df_safety_movement_1.csv")
df_safety_movement_2 <- read_csv("df_safety_movement_2.csv")

# Get the column names here to align with those of Dropbacks_Merged
# Note: for 2-high plays, i.e. df_safety_movement_2, minSafetyDistToMOF refers to the minimum across both safeties
colnames(df_safety_movement_1)
colnames(df_safety_movement_2)
colnames(Dropbacks_Merged)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(num_safeties_pre_snap = `num_safeties`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(num_safeties_pre_snap = `num_safeties`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(defteam = `defensiveTeam`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(defteam = `defensiveTeam`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(posteam = `possessionTeam`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(posteam = `possessionTeam`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(PostSnap_MOF_Num = `mofo_postsnap`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(PostSnap_MOF_Num = `mofo_postsnap`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(Ball_X_Snap = `los`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Ball_X_Snap = `los`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(pre_snap_safety_1 = `nflId`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_1 = `nflId_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_2 = `nflId_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(pre_snap_safety_1_name = `displayName`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_1_name = `displayName_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_2_name = `displayName_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(Safety1_Initial_X = `x_first`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety1_Initial_X = `x_first_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety2_Initial_X = `x_first_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(Safety1_Initial_Y = `y_first`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety1_Initial_Y = `y_first_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(Safety2_Initial_Y = `y_first_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(pre_snap_safety_1_X_AtSnap = `x_last`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_1_X_AtSnap = `x_last_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_2_X_AtSnap = `x_last_p2`)
df_safety_movement_1 <- df_safety_movement_1 %>% rename(pre_snap_safety_1_Y_AtSnap = `y_last`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_1_Y_AtSnap = `y_last_p1`)
df_safety_movement_2 <- df_safety_movement_2 %>% rename(pre_snap_safety_2_Y_AtSnap = `y_last_p2`)

# Now merge() these to Dropbacks_Merged, but probably have to separate to 1-high and 2-high plays
# To keep it simple, take out some unnecessary columns from df_safety_movement DFs
df_safety_movement_1 <- df_safety_movement_1 %>% 
  select(-c("pre_snap_safety_1", "pre_snap_safety_1_name", "num_safeties_pre_snap", "PostSnap_MOF_Num",
            "defteam", "posteam", "Ball_X_Snap", "pre_snap_safety_1_X_AtSnap", "pre_snap_safety_1_Y_AtSnap",
            "Safety1_Initial_X", "Safety1_Initial_Y"))
final_dropbacks_1High <- merge(x = Dropbacks_Merged, y = df_safety_movement_1,
                               by = c("gameId", "playId"))

df_safety_movement_2 <- df_safety_movement_2 %>% 
  select(-c("pre_snap_safety_1", "pre_snap_safety_1_name", "pre_snap_safety_2", "pre_snap_safety_2_name", 
            "num_safeties_pre_snap", "PostSnap_MOF_Num",
            "defteam", "posteam", "Ball_X_Snap", "pre_snap_safety_1_X_AtSnap", "pre_snap_safety_1_Y_AtSnap",
            "pre_snap_safety_2_X_AtSnap", "pre_snap_safety_2_Y_AtSnap",
            "Safety1_Initial_X", "Safety1_Initial_Y", "Safety2_Initial_X", "Safety2_Initial_Y"))
final_dropbacks_2High <- merge(x = Dropbacks_Merged, y = df_safety_movement_2,
                               by = c("gameId", "playId"))

rm(df_safety_movement_1, df_safety_movement_2)

# Adjust 1-high column names to match 2-high, then rbind()
final_dropbacks_1High <- final_dropbacks_1High %>% rename(x_spline_basis1_p1 = `x_spline_basis1`,
                                                          x_spline_basis2_p1 = `x_spline_basis2`,
                                                          x_spline_basis3_p1 = `x_spline_basis3`,
                                                          x_spline_basis4_p1 = `x_spline_basis4`,
                                                          x_spline_basis5_p1 = `x_spline_basis5`,
                                                          x_spline_basis6_p1 = `x_spline_basis6`,
                                                          x_spline_basis7_p1 = `x_spline_basis7`,
                                                          x_spline_basis8_p1 = `x_spline_basis8`,
                                                          x_spline_basis9_p1 = `x_spline_basis9`,
                                                          x_spline_basis10_p1 = `x_spline_basis10`,
                                                          y_spline_basis1_p1 = `y_spline_basis1`,
                                                          y_spline_basis2_p1 = `y_spline_basis2`,
                                                          y_spline_basis3_p1 = `y_spline_basis3`,
                                                          y_spline_basis4_p1 = `y_spline_basis4`,
                                                          y_spline_basis5_p1 = `y_spline_basis5`,
                                                          y_spline_basis6_p1 = `y_spline_basis6`,
                                                          y_spline_basis7_p1 = `y_spline_basis7`,
                                                          y_spline_basis8_p1 = `y_spline_basis8`,
                                                          y_spline_basis9_p1 = `y_spline_basis9`,
                                                          y_spline_basis10_p1 = `y_spline_basis10`)

# We can't rbind() while they have a different number of columns, so let's add "NA" columns to final_dropbacks_1High
# Identify the columns that are in final_dropbacks_2High but not in final_dropbacks_1High
missing_cols <- setdiff(names(final_dropbacks_2High), names(final_dropbacks_1High))

# Add these columns to final_dropbacks_1High with NA values
for (col in missing_cols) {
  final_dropbacks_1High[[col]] <- NA
}

# Now rbind() will work since both data frames have the same columns
final_dropbacks_merged <- rbind(final_dropbacks_1High, final_dropbacks_2High)
rm(Dropbacks_Merged, missing_cols, final_dropbacks_1High, final_dropbacks_2High)

# Some good ones to check out where the model correctly guessed a disguised coverage
# And recall that PostSnap_MOF_Num in final_dropbacks_merged is equivalent to mofo_postsnap in the original NN_model_results_DF
# View(NN_model_results_DF %>% filter((num_safeties_pre_snap == 2 & p < 0.3 & PostSnap_MOF_Num == 0) | (num_safeties_pre_snap == 1 & p > 0.7 & PostSnap_MOF_Num == 1)))
# View(final_dropbacks_merged %>% filter(gameId == 2022091101, playId == 2298)) ... 2-high turns to Cover 3 Sky
# View(final_dropbacks_merged %>% filter(gameId == 2022092507, playId == 1836)) ... 1-high turns to Cover 2
# View(final_dropbacks_merged %>% filter(gameId == 2022090800, playId == 2623)) ... 2-high turns to Cover 1 Hole
# View(final_dropbacks_merged %>% filter(gameId == 2022101606, playId == 3531)) ... 1-high turns to Cover 0
# View(final_dropbacks_merged %>% filter(gameId == 2022090800, playId == 1504)) ... 2-high turns to Cover 3 Sky
# View(final_dropbacks_merged %>% filter(gameId == 2022090800, playId == 167)) ... 2-high turns to Cover 3 Sky
# View(final_dropbacks_merged %>% filter(gameId == 2022090800, playId == 2288)) ... 2-high turns to Cover 3 Sky
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 1672)) ... 2-high turns to Cover 3 Sky
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 3475)) ... 2-high turns to Cover 1 Hole
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 2533)) ... 2-high turns to Cover 1 Hole
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 3177)) ... 2-high turns to Cover 3 Sky
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 1769)) ... 2-high turns to Cover 3 Sky
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 2159)) ... some type of miscommunication, but looks like intent was 2-high to Cover 3 zone blitz
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 2226)) ... 2-high to Cover 3 Sky

# Here are "conventional" snaps, where model's guess of the post-snap coverage was right, and matched pre-snap alignment
# View(NN_model_results_DF %>% filter((num_safeties_pre_snap == 2 & p > 0.7 & PostSnap_MOF_Num == 1) | (num_safeties_pre_snap == 1 & p < 0.3 & PostSnap_MOF_Num == 0)))
# View(final_dropbacks_merged %>% filter(gameId == 2022090800, playId == 1967)) ... Cover 2 out of 2-high
# View(final_dropbacks_merged %>% filter(gameId == 2022090800, playId == 2336)) ... Tampa 2 out of 2-high
# View(final_dropbacks_merged %>% filter(gameId == 2022090800, playId == 954)) ... Cover 2 out of 2-high
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 783)) ... Cover 1 Hole out of 1-high
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 2303)) ... Cover 1 Hole out of 1-high
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 3119)) ... Cover 3 out of 1-high
# View(final_dropbacks_merged %>% filter(gameId == 2022091100, playId == 1587)) ... Cover 3 out of 1-high
# View(final_dropbacks_merged %>% filter(gameId == 2022091101, playId == 599)) ... hard to tell but looks like Cover 4 out of 2-high


# Write final_dropbacks_merged into a CSV
write.csv(final_dropbacks_merged, "final_dropbacks_merged.csv")
