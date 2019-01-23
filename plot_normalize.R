# load libraries and set working directory
library(tidyverse)
library(ggplot2)
setwd("C:/users/Meso/Documents/Regis/MSDS 692/R code")

# read csv file for lab traits
PlotTraits <- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\CSI2015_PLOT_TRAITS_LOC_NEW.csv', header = TRUE)

#When this table was pulled from the database many duplicate rows were created. This removes the duplicates
PlotTraits_noDup <- unique(PlotTraits)

# create normalize function
normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) -  min(x)
  return(num/denom)
}

# clean up data frame to remove items that are not useful
exclude_line <- data.frame(Lines = c('9879', '12941'))
exclude_trait <- data.frame(Trait = c("LODGING"))
Plot_Use <- PlotTraits_noDup %>% filter(! dbo_LINE_LINE_ID %in% exclude_line$Lines, ! TRAIT_NM %in% exclude_trait$Trait)

# find all trials
trials <- data.frame(COOP_LOC_NM = unique(Plot_Use$COOP_LOC_NM))

# set up an empty data frame with the columns wanted
PlotNormal <- data.frame(dbo_LINE_LINE_ID = character(), TRAIT_NM = character(), OBSRV_NT_NBR = numeric(), OBSRV_EFF_TSP = character(), COOP_LOC_NM = character(), norm_obs = numeric(), stringsAsFactors = FALSE)

#loop through trials, and traits measured within trials to normalize data then write to the new table
for (i in 1:nrow(trials)) {
  loc_filter <- filter(Plot_Use, COOP_LOC_NM == trials[i,])
  loc_traits <- data.frame(TRAIT_NM = unique(loc_filter$TRAIT_NM))
  for (s in 1:nrow(loc_traits)) {
    trait <- filter(loc_filter, TRAIT_NM == loc_traits[s,])
    trait <- na.omit(trait)
    if(is.na(trait[1,5])) next
    one_trait_norm <- as.data.frame(lapply(trait[5], normalize))
    trait$norm_obs <- unlist(one_trait_norm)
    trait$norm_obs <- as.numeric(trait$norm_obs)
    trait_factors <- sapply(trait, is.factor)
    trait[trait_factors] <- lapply(trait[trait_factors], as.character)
    for (t in 1:nrow(trait)) {
      new_entry <- trait[t,]
      PlotNormal[nrow(PlotNormal)+1,] = list(new_entry$dbo_LINE_LINE_ID, new_entry$TRAIT_NM, new_entry$OBSRV_NT_NBR, new_entry$OBSRV_EFF_TSP, new_entry$COOP_LOC_NM, new_entry$norm_obs)
    }
  }
}

#write the new table out as a csv
write.csv(PlotNormal, 'PLOT_NORMAL.csv')


