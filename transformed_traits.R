# Set up libraries and working directory
library(tidyverse)
library(ggplot2)
library(data.table)
setwd("C:/users/Meso/Documents/Regis/MSDS 692/R code")

# Load cleaned and uncleaned trait files
LineTraits <- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\CSI2015_TRAITS_WITH_LOC.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, "NA", " NA"))
LineTraits_norm <- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\R code\\LINE_NORMAL.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, 'NA', ' NA'))
PlotTraits <- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\CSI2015_PLOT_TRAITS_LOC_NEW.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, "NA", " NA"))
PlotTraits_norm <- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\R code\\PLOT_NORMAL.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, 'NA', ' NA'))

# Perform basic cleaning on uncleaned files (no na removal)
exclude_line <- data.frame(Lines = c('9879', '12941'))
exclude_trait_nm <- data.frame(Trait_NM = c('UNIT#', 'FILTRATE ONE HOUR', 'MALT PROBLEMS', 'STEEP IN DATE', 'STEEP NUMBER', 'MALTING PROGRAM', 'LODGING'))
Line_Use <- LineTraits %>% filter(! dbo_LINE_LINE_ID %in% exclude_line$Lines, ! TRAIT_NM %in% exclude_trait_nm$Trait_NM)
Plot_Use <- PlotTraits %>% filter(! dbo_LINE_LINE_ID %in% exclude_line$Lines, ! TRAIT_NM %in% exclude_trait_nm$Trait_NM)
LineTraits_norm$X <- NULL
LineTraits_norm$TRAIT_ID <- NULL
PlotTraits_norm$X <- NULL
LineTraits$TRAIT_ID <- NULL
Line_Use$TRAIT_ID <- NULL

# Combine LineTraits
common_atts <- c('dbo_LINE_LINE_ID', 'TRAIT_NM',    'OBSRV_NT_NBR', 'OBSRV_EFF_TSP', 'COOP_LOC_NM')
LineTraits_an <- merge(Line_Use, LineTraits_norm, by = common_atts, all.x = TRUE)

# Combine PlotTraits
PlotTraits_an <- merge(Plot_Use, PlotTraits_norm, by = common_atts, all.x = TRUE)

# Combine all traits
common_atts_an <- c('dbo_LINE_LINE_ID', 'TRAIT_NM', 'TRAIT_DSC', 'OBSRV_NT_NBR', 'OBSRV_EFF_TSP', 'COOP_LOC_NM', 'norm_obs', 'CNTRY_NM', 'REGN_NM', 'LOC_NM')
Traits_hold <- merge(LineTraits_an, PlotTraits_an, by = common_atts_an, all = TRUE)

# Remove any duplicates
Traits <- unique(Traits_hold)

# Create data frame of trials
coop_loc <- data.frame(COOP_LOC_NM = as.character(unique(Traits$COOP_LOC_NM)), stringsAsFactors = FALSE)

# Create column that combines trial with line id
Traits$LINEIDLOC <- paste(Traits$dbo_LINE_LINE_ID, Traits$COOP_LOC_NM)

# Transform selectively using norm_obs as the new value, lines at trials with more than one measurement were averaged
traits_by_line <- dcast(as.data.table(Traits), LINEIDLOC ~ TRAIT_NM, value.var = 'norm_obs', fun.aggregate = mean)
traits_by_line$dbo_LINE_LINE_ID <- character()
traits_by_line$COOP_LOC_NM <- character()

# Add identifying tags back 
for (i in 1:nrow(traits_by_line)) {
  lineid <- substr(traits_by_line[i,1], 0, 6)
  cooploc <- substr(traits_by_line[i, 1], 8, nchar(traits_by_line[i,1]))
  traits_by_line$dbo_LINE_LINE_ID[i] <- lineid
  traits_by_line$COOP_LOC_NM[i] <- cooploc
}

# Remove unwanted columns
Traits_hld <- Traits
Traits_hld$TRAIT_NM <- NULL
Traits_hld$TRAIT_DSC <- NULL
Traits_hld$OBSRV_NT_NBR <- NULL
Traits_hld$OBSRV_EFF_TSP <- NULL
Traits_hld$norm_obs <- NULL
Traits_hld$LINEIDLOC <- NULL
traits_by_line$LINEIDLOC <- NULL
Traits_lines <- unique(Traits_hld)
common_atts_ml <- c('dbo_LINE_LINE_ID', 'COOP_LOC_NM')

# Final combine
ML_traits <- merge(Traits_lines, traits_by_line, by = common_atts_ml)

# Write out to csv
write.csv(ML_traits,'ML_TRAITS.csv')

# Re-load to make sure NAs show up correctly
ML <- read.csv('ML_TRAITS.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, 'NA', ' NA', 'NaN', NaN))
