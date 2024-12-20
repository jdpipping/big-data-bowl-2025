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
final_dropbacks_merged <- merge(x = Dropbacks_Merged, y = NN_DF_abridged, 
                          by = c("gameId", "playId"))
final_dropbacks_merged <- final_dropbacks_merged %>% rename(MOFO_probability_FDA = `p`)
rm(NN_DF_abridged)

setDT(final_dropbacks_merged)
setkey(final_dropbacks_merged, gameId, playId, nflId, frameId)
final_dropbacks_merged <- final_dropbacks_merged %>% relocate("gameId", "playId", "nflId", "displayName", "frameId")
rm(Dropbacks_Merged)

# Some good ones to check out where the model correctly guessed a disguised coverage:
# View(NN_model_results_DF %>% filter((num_safeties_pre_snap == 2 & p < 0.3) | (num_safeties_pre_snap == 1 & p > 0.7)))
# View(final_dropbacks_merged %>% filter(gameId == 2022091101, playId == 2298)) ... 2-high turns to Cover 3
# View(final_dropbacks_merged %>% filter(gameId == 2022092507, playId == 1836)) ... 1-high turns to Cover 2

# On the flip side, here are "conventional" snaps, where model's guess of the post-snap coverage matched pre-snap alignment
# View(NN_model_results_DF %>% filter((num_safeties_pre_snap == 2 & p > 0.7) | (num_safeties_pre_snap == 1 & p < 0.3)))
