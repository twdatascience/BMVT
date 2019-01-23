# load libraries and set working directory
library(tidyverse)
library(ggplot2)
setwd("C:/users/Meso/Documents/Regis/MSDS 692/R code")

# read csv file for lab traits
LineTraits <- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\CSI2015_TRAITS_WITH_LOC.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, "NA", " NA"))

# create normalize function
normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) -  min(x)
  return(num/denom)
}

# clean up data frame to remove items that are not useful
exclude_line <- data.frame(Lines = c('9879', '12941'))
exclude_trait_nm <- data.frame(Trait_NM = c('UNIT#', 'FILTRATE ONE HOUR', 'MALT PROBLEMS', 'STEEP IN DATE', 'STEEP NUMBER'))
Line_Use <- LineTraits %>% filter(! dbo_LINE_LINE_ID %in% exclude_line$Lines, ! TRAIT_NM %in% exclude_trait_nm$Trait_NM)

# find all trials
trials <- data.frame(COOP_LOC_NM = unique(Line_Use$COOP_LOC_NM))

# set up an empty data frame with the columns wanted
LineNormal <- data.frame(dbo_LINE_LINE_ID = character(), TRAIT_NM = character(), OBSRV_NT_NBR = numeric(), OBSRV_EFF_TSP = character(), COOP_LOC_NM = character(), norm_obs = numeric(), stringsAsFactors = FALSE)

#loop through trials, and traits measured within trials to normalize data then write to the new table
for (i in 1:nrow(trials)) {
  loc_filter <- filter(Line_Use, COOP_LOC_NM == trials[i,])
  loc_traits <- data.frame(TRAIT_NM = unique(loc_filter$TRAIT_NM))
  for (s in 1:nrow(loc_traits)) {
    trait <- filter(loc_filter, TRAIT_NM == loc_traits[s,])
    trait <- na.omit(trait)
    if(is.na(trait[1,4])) next
    one_trait_norm <- as.data.frame(lapply(trait[4], normalize))
    trait$norm_obs <- unlist(one_trait_norm)
    trait$norm_obs <- as.numeric(trait$norm_obs)
    trait_factors <- sapply(trait, is.factor)
    trait[trait_factors] <- lapply(trait[trait_factors], as.character)
    for (t in 1:nrow(trait)) {
      new_entry <- trait[t,]
      LineNormal[nrow(LineNormal)+1,] = list(new_entry$dbo_LINE_LINE_ID, new_entry$TRAIT_NM, new_entry$OBSRV_NT_NBR, new_entry$OBSRV_EFF_TSP, new_entry$COOP_LOC_NM, new_entry$norm_obs)
    }
  }
}

#write the new table out as a csv
write.csv(LineNormal, 'LINE_NORMAL.csv')

# this was my first attempt to loop through. It was used for the preliminary EDA shown below
locations <- unique(Line_Use$COOP_LOC_NM)
out_list <- list()
for (i in seq_along(locations)) {
  loc_filter <- filter(Line_Use, COOP_LOC_NM == locations[i])
  loc_traits <- unique(loc_filter$TRAIT_NM)
  out <- list()
  for (s in seq_along(loc_traits)) {
    trait <- filter(loc_filter, TRAIT_NM == loc_traits[s])
    trait <- na.omit(trait)
    if(is.na(trait[1,4])) next
    one_trait_norm <- as.data.frame(lapply(trait[4], normalize))
    trait$norm_obs <- unlist(one_trait_norm)
    out[[s]] <- trait
  }
  out_list[[i]] <- as.data.frame(bind_rows(out))
}
norm_trait <- as.data.frame(bind_rows(out_list))

#preliminary EDA visualizations
ggplot(data = filter(norm_trait, TRAIT_NM == "YIELD")) + geom_boxplot(mapping = aes(REGN_NM, norm_obs, color = LOC_NM)) + labs(title = "Yield for Inter Mountain and Mid West", x = 'Region Name', y = 'Normalized Value') + scale_color_discrete(name = 'Location Name')

ggplot(data = filter(norm_trait, TRAIT_NM == "BETA GLUCAN")) + geom_boxplot(mapping = aes(REGN_NM, norm_obs, color = LOC_NM)) + labs(title = "Beta Glucan by Region", x = 'Region Name', y = 'Normalized Value') + scale_color_discrete(name = 'Location Name')

