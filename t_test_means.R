# Set up libraries and working directory
library(tidyverse)
library(ggplot2)
setwd("C:/users/Meso/Documents/Regis/MSDS 692/R code")

# Read mearged file created previously and remove the X column
merged <- read.csv('TRAITS_W_MARKERS.csv', header = TRUE, stringsAsFactors = FALSE)
merged$X <- NULL

# Create a data frame of all trials
coop_loc <- data.frame(COOP_LOC_NM = unique(merged$COOP_LOC_NM))
coop_loc$COOP_LOC_NM <- as.character(coop_loc$COOP_LOC_NM)

# Set up a new data frame
loc_means <- data.frame(COOP_LOC_NM = as.character(), TRAIT_NM = as.character(), MARKER_NM = as.character(), HAP_CALL = as.character(), MEAN_OBS = as.numeric(), MEAN_NORM = as.numeric(), stringsAsFactors = FALSE)

# Loop through to create a mean for each marker sub-population for each trial and triat
for (t in 1:nrow(coop_loc)) {
  loc_filter <- filter(merged, COOP_LOC_NM == coop_loc[t,])
  loc_markers <- data.frame(MARKER_NM = unique(loc_filter$MARKER_NM))
  loc_markers$MARKER_NM <- as.character(loc_markers$MARKER_NM)
  for (s in 1:nrow(loc_markers)) {
    marker_filter <- filter(loc_filter, MARKER_NM == loc_markers[s,])
    traits_marker <- data.frame(TRAIT_NM = unique(marker_filter$TRAIT_NM))
    traits_marker$TRAIT_NM <- as.character(traits_marker$TRAIT_NM)
    for (i in 1:nrow(traits_marker)) {
      trait_filter <- filter(marker_filter, TRAIT_NM == traits_marker[i,])
      line_AA <- filter(trait_filter, HAP_CALL == 'AA')
      line_BB <- filter(trait_filter, HAP_CALL == 'BB')
      if (is.data.frame(line_AA) && nrow(line_AA) == 0) next
      if (is.data.frame(line_BB) && nrow(line_BB) == 0) next
      LineA_mean <- mean(line_AA$OBSRV_NT_NBR)
      LineA_n_mean <- mean(line_AA$norm_obs)
      LineB_mean <- mean(line_BB$OBSRV_NT_NBR)
      LineB_n_mean <- mean(line_BB$norm_obs)
      loc_means[nrow(loc_means)+1,] = list(coop_loc[t,], traits_marker[i,], loc_markers[s,], 'AA', LineA_mean, LineA_n_mean)
      loc_means[nrow(loc_means)+1,] = list(coop_loc[t,], traits_marker[i,], loc_markers[s,], 'BB', LineB_mean, LineB_n_mean)
    }
  }
}

# Create table for p-values
t_test_loc_means <- data.frame(MARKER_NM = as.character(), TRAIT_NM = as.character(), p.value = as.numeric(), stringsAsFactors = FALSE)
marker_means <- data.frame(MARKER_NM = unique(loc_means$MARKER_NM), stringsAsFactors = FALSE)

# calculate p-values
for (s in 1:nrow(marker_means)) {
  marker_filter <- filter(loc_means, MARKER_NM == marker_means[s,])
  traits_marker <- data.frame(TRAIT_NM = unique(marker_filter$TRAIT_NM), stringsAsFactors = FALSE)
  for (i in 1:nrow(traits_marker)) {
    trait_filter <- filter(marker_filter, TRAIT_NM == traits_marker[i,])
    line_AA <- filter(trait_filter, HAP_CALL == 'AA')
    line_BB <- filter(trait_filter, HAP_CALL == 'BB')
    if (is.data.frame(line_AA) && nrow(line_AA) <= 1) next
    if (is.data.frame(line_BB) && nrow(line_BB) <= 1) next
    test <- t.test(line_AA$MEAN_NORM, line_BB$MEAN_NORM)
    t_test_loc_means[nrow(t_test_loc_means)+1,] = list(marker_means[s,], traits_marker[i,], test[3])
  }
}
t_test_loc_means$p.value <- as.numeric(t_test_loc_means$p.value)

# Find significant traits and markers
significant_means <- filter(t_test_loc_means, p.value <= 0.01)
sig_markers_means <- data.frame(MARKER_NM = unique(significant_means$MARKER_NM), stringsAsFactors = FALSE)
non_sig_markers_means <- anti_join(marker_means, sig_markers_means, by = "MARKER_NM")

# Count significant markers
marker_sig_count_means <- data.frame(MARKER_NM = character(), COUNT = integer(), stringsAsFactors = FALSE)
for (i in 1:nrow(sig_markers_means)) {
  sig_traits <- filter(significant_means, MARKER_NM == sig_markers_means[i,])
  st_count <- nrow(sig_traits)
  marker_sig_count_means[nrow(marker_sig_count_means)+1,] = list(sig_markers_means[i,], st_count)
}

# Significant markers are still high so I broke it down into trials again
loc_t_test <- data.frame(COOP_LOC_NM = character(), TRAIT_NM = character(), MARKER_NM = character(), p.value = numeric(), stringsAsFactors = FALSE)
for (t in 1:nrow(coop_loc)) {
  loc_filter <- filter(merged, COOP_LOC_NM == coop_loc[t,])
  loc_markers <- data.frame(MARKER_NM = unique(loc_filter$MARKER_NM), stringsAsFactors = FALSE)
  loc_markers$MARKER_NM <- as.character(loc_markers$MARKER_NM)
  for (s in 1:nrow(loc_markers)) {
    marker_filter <- filter(loc_filter, MARKER_NM == loc_markers[s,])
    traits_marker <- data.frame(TRAIT_NM = unique(marker_filter$TRAIT_NM), stringsAsFactors = FALSE)
    traits_marker$TRAIT_NM <- as.character(traits_marker$TRAIT_NM)
    for (i in 1:nrow(traits_marker)) {
      trait_filter <- filter(marker_filter, TRAIT_NM == traits_marker[i,])
      line_AA <- filter(trait_filter, HAP_CALL == 'AA')
      line_BB <- filter(trait_filter, HAP_CALL == 'BB')
      if (is.data.frame(line_AA) && nrow(line_AA) <= 1) next
      if (is.data.frame(line_BB) && nrow(line_BB) <= 1) next
      test <- t.test(line_AA$OBSRV_NT_NBR, line_BB$OBSRV_NT_NBR)
      loc_t_test[nrow(loc_t_test)+1,] = list(coop_loc[t,], traits_marker[i,], loc_markers[s,], test[3])
    }
  }
}
loc_t_test$p.value <- as.numeric(loc_t_test$p.value)
marker_loc <- data.frame(MARKER_NM = unique(loc_t_test$MARKER_NM), stringsAsFactors = FALSE)

# Find significant traits and trials for each marker
significant_loc <- filter(loc_t_test, p.value <= 0.01)
sig_marker_loc <- data.frame(MARKER_NM = unique(significant_loc$MARKER_NM), stringsAsFactors = FALSE)
non_sig_marker_loc <- anti_join(marker_loc, sig_marker_loc, by = "MARKER_NM")

# count significant markers and traits
marker_sig_count_loc <- data.frame(MARKER_NM = character(), TRAIT_NM = character(), COUNT = integer(), stringsAsFactors = FALSE)
for (i in 1:nrow(sig_marker_loc)) {
  sig_marker <- filter(significant_loc, MARKER_NM == sig_marker_loc[i,])
  sig_traits <- data.frame(TRAIT_NM = as.character(unique(sig_marker$TRAIT_NM)), stringsAsFactors = FALSE)
  for (s in 1:nrow(sig_traits)) {
    sig_trait_iso <- filter(sig_marker, TRAIT_NM == sig_traits[s,])
    st_count <- nrow(sig_trait_iso)
    marker_sig_count_loc[nrow(marker_sig_count_loc)+1,] = list(sig_marker_loc[i,], sig_traits[s,], st_count)
  }
}