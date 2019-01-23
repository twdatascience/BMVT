# Set up libraries and set working directory
library(tidyverse)
library(ggplot2)
setwd("C:/users/Meso/Documents/Regis/MSDS 692/R code")

# Read in marker information and line and plot traits already cleaned
Markers<- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\CSI2015_MARKERS.csv', header = TRUE)
Line_Traits <- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\R code\\LINE_NORMAL.csv', header = TRUE, stringsAsFactors = FALSE)
Plot_Traits <- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\R code\\PLOT_NORMAL.csv', header = TRUE, stringsAsFactors = FALSE)
Line_Traits$X <- NULL
Plot_Traits$X <- NULL

# Combine the two types of traits
Traits <- bind_rows(Line_Traits, Plot_Traits)

# Remove plants from markers
Marks_noplant <- Markers[, !(names(Markers) %in% c("PLANT_ID")) ]
Markers <- unique(Marks_noplant)

# Remove unwanted info from marker data frame
exclude_line <- data.frame(Lines = c('CONRAD', 'MERIT 57'))
Marker_Use <- Markers %>% filter(! dbo_LINE_LINE_NM %in% exclude_line$Lines)
Marker_Use$dbo_LINE_LINE_NM <- NULL
Marker_Use$dbo_LINE_1_LINE_ID <- NULL
Marker_Use$dbo_LINE_1_LINE_NM <- NULL

# Make the initial merge
merged_1 <- merge(Marker_Use, Traits, by = 'dbo_LINE_LINE_ID')

# Clean out unwanted triats
exclude_trait_nm <- data.frame(Trait_NM = c('UNIT#', 'FILTRATE ONE HOUR', 'MALT PROBLEMS', 'STEEP IN DATE', 'STEEP NUMBER', 'FGGRAMS OVER', 'GM.MOIS', 'HEIGHT.CM', 'FGGRAMS.OVER', 'MALT.MIDS', 'MALT.PLUMPS', 'MIDS.BARLEY', 'PULMP.BARLEY', 'RDF', 'SOM.MOIS', 'VISC.CP', 'WIEGHT', 'Z_YIELD', 'THINS.BARLEY'))
merged <- merged_1 %>% filter(! TRAIT_NM %in% exclude_trait_nm$Trait_NM)

# Make a data frame of all markers used
markers_used <- data.frame(MARKER_NM = unique(merged$MARKER_NM), stringsAsFactors = FALSE)

# Write out the merged file to use later
write.csv(merged, "TRAITS_W_MARKERS.csv")