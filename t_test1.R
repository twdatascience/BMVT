# Set up libraries and working directory
library(tidyverse)
library(ggplot2)
setwd("C:/users/Meso/Documents/Regis/MSDS 692/R code")

# Read the merged file created previously
merged <- read.csv('TRAITS_W_MARKERS.csv')

# Create list of marker names
markers_used <- data.frame(MARKER_NM = unique(merged$MARKER_NM), stringsAsFactors = FALSE)

# Create a loop to run t-tests and write p-values to a new data frame
final <- list()
for (i in 1:nrow(markers_used)) {
  marker_filter <- filter(merged, MARKER_NM == markers_used[i,])
  traits_marker <- data.frame(TRAIT_NM = unique(marker_filter$TRAIT_NM))
  traits_marker$TRAIT_NM <- as.character(traits_marker$TRAIT_NM)
  out <- list()
  for (s in 1:nrow(traits_marker)) {
    trait_filter <- filter(marker_filter, TRAIT_NM == traits_marker[s,])
    line_AA <- filter(trait_filter, HAP_CALL == 'AA')
    line_BB <- filter(trait_filter, HAP_CALL == 'BB')
    if (is.data.frame(line_AA) && nrow(line_AA) <= 1) next
    if (is.data.frame(line_BB) && nrow(line_BB) <= 1) next
    test <- t.test(line_AA['norm_obs'], line_BB['norm_obs'])
    test_df <- as.data.frame(c(traits_marker[s,], test[3]), col.names = c('TRAIT_NM', 'p-value'))
    test_df$MARKER_NM <- markers_used[i,]
    test_df$TRAIT_NM <- as.character(test_df$TRAIT_NM)
    test_df$p.value <- as.double(test_df$p.value)
    test_df$MARKER_NM <- as.character(test_df$MARKER_NM)
    out[[s]] <- test_df
  }
  final[[i]] <- as.data.frame(bind_rows(out))
}
total_p_values <- bind_rows(final)

# find which p-values were significant. I used 0.01 here.
significant <- filter(total_p_values, p.value <= 0.01)

# Create a table with each marker used and a count of how many traits had significant p-values
colnames(markers_used) <- "MARKER_NM"
markers_used$MARKER_NM <- as.character(markers_used$MARKER_NM)
sig_markers <- as.data.frame(unique(significant$MARKER_NM))
colnames(sig_markers) <- "MARKER_NM"
sig_markers$MARKER_NM <- as.character(sig_markers$MARKER_NM)
non_sig_markers <- anti_join(markers_used, sig_markers, by = "MARKER_NM")
marker_sig_count <- data.frame(MARKER_NM = character(), COUNT = integer(), stringsAsFactors = FALSE)
for (i in 1:nrow(sig_markers)) {
  sig_traits <- filter(significant, MARKER_NM == sig_markers[i,])
  st_count <- nrow(sig_traits)
  marker_sig_count[nrow(marker_sig_count)+1,] = list(sig_markers[i,], st_count)
}

# Too many p-values were signficant so I tried a slightly different kind of t-test: the wilcox test
final_w <- list()
for (i in 1:nrow(markers_used)) {
  marker_filter_w <- filter(merged, MARKER_NM == markers_used[i,])
  traits_marker_w <- data.frame(TRAIT_NM = unique(marker_filter$TRAIT_NM))
  traits_marker_w$TRAIT_NM <- as.character(traits_marker_w$TRAIT_NM)
  out <- list()
  for (s in 1:nrow(traits_marker_w)) {
    trait_filter <- filter(marker_filter_w, TRAIT_NM == traits_marker_w[s,])
    line_AA <- filter(trait_filter, HAP_CALL == 'AA')
    line_BB <- filter(trait_filter, HAP_CALL == 'BB')
    if (is.data.frame(line_AA) && nrow(line_AA) <= 1) next
    if (is.data.frame(line_BB) && nrow(line_BB) <= 1) next
    line_AA$norm_obs <- as.numeric(line_AA$norm_obs)
    line_BB$norm_obs <- as.numeric(line_BB$norm_obs)
    test <- wilcox.test(line_AA$norm_obs, line_BB$norm_obs, alternative = "two.sided")
    test_df <- as.data.frame(c(traits_marker[s,], test[3]), col.names = c('TRAIT_NM', 'p-value'))
    test_df$MARKER_NM <- markers_used[i,]
    test_df$TRAIT_NM <- as.character(test_df$TRAIT_NM)
    test_df$p.value <- as.double(test_df$p.value)
    test_df$MARKER_NM <- as.character(test_df$MARKER_NM)
    out[[s]] <- test_df
  }
  final_w[[i]] <- as.data.frame(bind_rows(out))
}
total_p_values_w <- bind_rows(final_w)

# Once again I found the traits with significant p-values
significant_w <- filter(total_p_values_w, p.value <= 0.01)

# Created another table to count significant p-values
sig_markers_w <- as.data.frame(unique(significant_w$MARKER_NM))
colnames(sig_markers_w) <- "MARKER_NM"
sig_markers_w$MARKER_NM <- as.character(sig_markers_w$MARKER_NM)
non_sig_markers_w <- anti_join(markers_used, sig_markers_w, by = "MARKER_NM")
marker_sig_count_w <- data.frame(MARKER_NM = character(), COUNT = integer(), stringsAsFactors = FALSE)
for (i in 1:nrow(sig_markers_w)) {
  sig_traits <- filter(significant_w, MARKER_NM == sig_markers_w[i,])
  st_count <- nrow(sig_traits)
  marker_sig_count_w[nrow(marker_sig_count_w)+1,] = list(sig_markers_w[i,], st_count)
}

# I decided to try to run a t-test for every trial and then obtain a percent of trials that were significant for a trait.
coop_loc <- data.frame(COOP_LOC_NM = unique(merged$COOP_LOC_NM))
coop_loc$COOP_LOC_NM <- as.character(coop_loc$COOP_LOC_NM)
final <- list()
for (i in 1:nrow(coop_loc)) {
  loc_filter <- filter(merged, COOP_LOC_NM == coop_loc[i,])
  loc_markers <- data.frame(MARKER_NM = unique(loc_filter$MARKER_NM))
  loc_markers$MARKER_NM <- as.character(loc_markers$MARKER_NM)
  out_2 <- list()
  for (s in 1:nrow(loc_markers)) {
    marker_filter <- filter(loc_filter, MARKER_NM == loc_markers[s,])
    traits_marker <- data.frame(TRAIT_NM = unique(marker_filter$TRAIT_NM))
    traits_marker$TRAIT_NM <- as.character(traits_marker$TRAIT_NM)
    out <- list()
    for (t in 1:nrow(traits_marker)) {
      trait_filter <- filter(marker_filter, TRAIT_NM == traits_marker[t,])
      line_AA <- filter(trait_filter, HAP_CALL == 'AA')
      line_BB <- filter(trait_filter, HAP_CALL == 'BB')
      if (is.data.frame(line_AA) && nrow(line_AA) <= 1) next
      if (is.data.frame(line_BB) && nrow(line_BB) <= 1) next
      test <- t.test(line_AA$norm_obs, line_BB$norm_obs)
      test_df <- as.data.frame(c(traits_marker[t,], test[3]), col.names = c('TRAIT_NM', 'p-value'))
      test_df$MARKER_NM <- loc_markers[s,]
      test_df$COOP_LOC_NM <- coop_loc[i,]
      test_df$TRAIT_NM <- as.character(test_df$TRAIT_NM)
      test_df$p.value <- as.double(test_df$p.value)
      test_df$MARKER_NM <- as.character(test_df$MARKER_NM)
      test_df$COOP_LOC_NM <- as.character(test_df$COOP_LOC_NM)
      out[[t]] <- test_df
    }
    out_2[[s]] <- as.data.frame(bind_rows(out))
  }
  final[[i]] <- as.data.frame(bind_rows(out_2))
}
coop_loc_p_values <- bind_rows(final)

# Found significant trials and traits
significant_loc <- filter(coop_loc_p_values, p.value <= 0.01)
sig_markers_loc <- as.data.frame(unique(significant_loc$MARKER_NM))
colnames(sig_markers_loc) <- "MARKER_NM"
sig_markers_loc$MARKER_NM <- as.character(sig_markers_loc$MARKER_NM)
non_sig_markers_loc <- anti_join(markers_used, sig_markers_loc, by = "MARKER_NM")

# Created a count for each marker
marker_sig_count_loc <- data.frame(MARKER_NM = character(), COUNT = integer(), stringsAsFactors = FALSE)
for (i in 1:nrow(sig_markers_loc)) {
  sig_traits <- filter(significant_loc, MARKER_NM == sig_markers_loc[i,])
  st_count <- nrow(sig_traits)
  marker_sig_count_loc[nrow(marker_sig_count_loc)+1,] = list(sig_markers_loc[i,], st_count)
}
sig_traits_loc <- data.frame(TRAIT_NM = unique(significant_loc$TRAIT_NM))
sig_traits_loc$TRAIT_NM <- as.character(sig_traits_loc$TRAIT_NM)

# Created a table with the percent of significant trials
marker_frac <- data.frame(MARKER_NM = character(), TRAIT_NM = character(), PERCENT = numeric(), stringsAsFactors = FALSE)
for (i in 1:nrow(sig_traits_loc)) {
  loc_filter <- filter(significant_loc, TRAIT_NM == sig_traits_loc[i,])
  loc_markers <- data.frame(MARKER_NM = unique(loc_filter$MARKER_NM))
  loc_markers$MARKER_NM <- as.character(loc_markers$MARKER_NM)
  for (s in 1:nrow(loc_markers)) {
    marker_filter <- filter(loc_filter, MARKER_NM == loc_markers[s,])
    loc_count <- nrow(marker_filter)
    loc_percent <- (loc_count/nrow(coop_loc))*100
    marker_frac[nrow(marker_frac)+1,] = list(loc_markers[s,], sig_traits_loc[i,], loc_percent)
  }
}