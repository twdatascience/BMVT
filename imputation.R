# Set up libraries and working directory
library(tidyverse)
library(ggplot2)
library(data.table)
library(DMwR)
setwd("C:/users/Meso/Documents/Regis/MSDS 692/R code")

# Read the prepared machine learning data frame
ML <- read.csv('ML_TRAITS.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, 'NA', ' NA', 'NaN', NaN))

# Filter the data frame
ML2 <- filter(ML, !is.na(ML$AA.CHART))
ML2$GM.MOIS <- NULL
ML2$HEIGHT.CM <- NULL
ML2$FGGRAMS.OVER <- NULL
ML2$MALT.MIDS <- NULL
ML2$MALT.PLUMPS <- NULL
ML2$MIDS.BARLEY <- NULL
ML2$PLUMP.BARLEY <- NULL
ML2$RDF <- NULL
ML2$SOM.MOIS <- NULL
ML2$VISC.CP <- NULL
ML2$WEIGHT <- NULL
ML2$Z_YIELD <- NULL
ML2$THINS.BARLEY <- NULL

# Impute the NA values
ML_Imputed <- knnImputation(ML2[, 10:ncol(ML2)], k = 10)

# Add in identifying tags
ML_Imputed$dbo_LINE_LINE_ID <- ML2$dbo_LINE_LINE_ID
ML_Imputed$COOP_LOC_NM <- ML2$COOP_LOC_NM

# Write out ML_Imputed as csv
write.csv(ML_Imputed, 'ML_IMPUTED.csv')

# Set up table to check where NAs were
nas <- data.frame(column = character(), row_found = character(), stringsAsFactors = FALSE)
for (i in 1:ncol(ML2)) {
  for (s in 1:nrow(ML2)) {
    if (is.na(ML2[s,i])) {
      nas[nrow(nas)+1,] = list(i,s)
    }
  }
}

# Create data frame with columns next to traits to tag where NAs were
ML_na_marked <- data.frame(dbo_LINE_LINE_ID = character(), COOP_LOC_NM = character(), AA.CHART = numeric(), AA.CHART.NA = character(), AA.DSB = numeric(), AA.DSB.NA = character(), B.MOISTURE.WGA = numeric(), B.MOISTURE.WGA.NA = character(),    B.PROTEIN.DSB.WGA = numeric(), B.PROTEIN.DSB.WGA.NA = character(),  B.STARCH.WGA = numeric(), B.STARCH.WGA.NA = character(),    BETA.GLUCAN = numeric(), BETA.GLUCAN.NA = character(),  DP.CHART = numeric(), DP.CHART.NA = character(),    DP.DSB = numeric(), DP.DSB.NA = character(),    FAN = numeric(), FAN.NA = character(),  FG.PLT = numeric(), FG.PLT.NA = character(),    FGW = numeric(), FGW.NA = character(),  FINE.EXT.DSB = numeric(), FINE.EXT.DSB.NA = character(),    M.MOISTURE.NIR = numeric(), M.MOISTURE.NIR.NA = character(),    M.PROTEIN.DSB.NIR = numeric(), M.PROTEIN.DSB.NIR.NA = character(),  M.PROTEIN.NIR = numeric(), M.PROTEIN.NIR.NA = character(),  M.LOSS = numeric(), M.LOSS.NA = character(),    OD = numeric(), OD.NA = character(),    S.T = numeric(), S.T.NA = character(),  SOLUBLE.PROTEIN = numeric(), SOLUBLE.PROTEIN.NA = character(), TURBIDITY = numeric(), TURBIDITY.NA = character(),   TW = numeric(), TW.NA = character(),    WN.= numeric(), WN.NA = character(),    WN.M = numeric(), WN.M.NA = character(),    WORT.COLOR = numeric(), WORT.COLOR.NA = character(),    YIELD = numeric(), YIELD.NA = character(), stringsAsFactors = FALSE)
for (i in 1:nrow(ML2)) {
  ML_na_marked[nrow(ML_na_marked)+1,] = list(ML2[i,2], ML2[i,3])
}

# Populate data frame
ML_na_marked$AA.CHART <- ML2$AA.CHART
ML_na_marked$AA.DSB <- ML2$AA.DSB
ML_na_marked$B.MOISTURE.WGA <- ML2$B.MOISTURE.WGA
ML_na_marked$B.PROTEIN.DSB.WGA <- ML2$B.PROTEIN.DSB.WGA
ML_na_marked$B.STARCH.WGA <- ML2$B.STARCH.WGA
ML_na_marked$BETA.GLUCAN <- ML2$BETA.GLUCAN
ML_na_marked$DP.CHART <- ML2$DP.CHART
ML_na_marked$DP.DSB <- ML2$DP.DSB
ML_na_marked$FAN <- ML2$FAN
ML_na_marked$FG.PLT <- ML2$FG.PLT
ML_na_marked$FGW <- ML2$FGW
ML_na_marked$FINE.EXT.DSB <- ML2$FINE.EXT.DSB
ML_na_marked$M.MOISTURE.NIR <- ML2$M.MOISTURE.NIR
ML_na_marked$M.PROTEIN.DSB.NIR <- ML2$M.PROTEIN.DSB.NIR
ML_na_marked$M.PROTEIN.NIR <- ML2$M.PROTEIN.NIR
ML_na_marked$M.LOSS <- ML2$M.LOSS
ML_na_marked$OD <- ML2$OD
ML_na_marked$S.T <- ML2$S.T
ML_na_marked$SOLUBLE.PROTEIN <- ML2$SOLUBLE.PROTEIN
ML_na_marked$TURBIDITY <- ML2$TURBIDITY
ML_na_marked$TW <- ML2$TW
ML_na_marked$WN. <- ML2$WN.
ML_na_marked$WN.M <- ML2$WN.M
ML_na_marked$WORT.COLOR <- ML2$WORT.COLOR
ML_na_marked$YIELD <- ML2$YIELD

# Create list of traits
traits <- c('AA.CHART', 'AA.DSB', 'B.MOISTURE.WGA', 'B.PROTEIN.DSB.WGA', 'B.STARCH.WGA', 'BETA.GLUCAN', 'DP.CHART', 'DP.DSB', 'FAN', 'FG.PLT',  'FGW',  'FINE.EXT.DSB', 'M.MOISTURE.NIR',   'M.PROTEIN.DSB.NIR',    'M.PROTEIN.NIR',    'M.LOSS',   'OD',   'S.T',  'SOLUBLE.PROTEIN',  'TURBIDITY',    'TW',   'WN.',  'WN.M', 'WORT.COLOR', 'YIELD')

# Mark which values were imputed

for (i in 1:ncol(ML_na_marked)) {
  for (s in 1:nrow(ML_na_marked)) {
    if (is.na(ML_na_marked[s,i])) {
      ML_na_marked[s,i+1] <- 'Y'
    }
  }
}
col.nums <- seq.int(4, 52, 2)
for (i in seq_along(col.nums)) {
  for (s in 1:nrow(ML_na_marked)) {
    if (ML_na_marked[s,col.nums[i]] != 'Y' ) {
      ML_na_marked[s,col.nums[i]] <- 'N'
    }
  }
}
ML_na_marked$AA.CHART <- ML_Imputed$AA.CHART
ML_na_marked$AA.DSB <- ML_Imputed$AA.DSB
ML_na_marked$B.MOISTURE.WGA <- ML_Imputed$B.MOISTURE.WGA
ML_na_marked$B.PROTEIN.DSB.WGA <- ML_Imputed$B.PROTEIN.DSB.WGA
ML_na_marked$B.STARCH.WGA <- ML_Imputed$B.STARCH.WGA
ML_na_marked$BETA.GLUCAN <- ML_Imputed$BETA.GLUCAN
ML_na_marked$DP.CHART <- ML_Imputed$DP.CHART
ML_na_marked$DP.DSB <- ML_Imputed$DP.DSB
ML_na_marked$FAN <- ML_Imputed$FAN
ML_na_marked$FG.PLT <- ML_Imputed$FG.PLT
ML_na_marked$FGW <- ML_Imputed$FGW
ML_na_marked$FINE.EXT.DSB <- ML_Imputed$FINE.EXT.DSB
ML_na_marked$M.MOISTURE.NIR <- ML_Imputed$M.MOISTURE.NIR
ML_na_marked$M.PROTEIN.DSB.NIR <- ML_Imputed$M.PROTEIN.DSB.NIR
ML_na_marked$M.PROTEIN.NIR <- ML_Imputed$M.PROTEIN.NIR
ML_na_marked$M.LOSS <- ML_Imputed$M.LOSS
ML_na_marked$OD <- ML_Imputed$OD
ML_na_marked$S.T <- ML_Imputed$S.T
ML_na_marked$SOLUBLE.PROTEIN <- ML_Imputed$SOLUBLE.PROTEIN
ML_na_marked$TURBIDITY <- ML_Imputed$TURBIDITY
ML_na_marked$TW <- ML_Imputed$TW
ML_na_marked$WN. <- ML_Imputed$WN.
ML_na_marked$WN.M <- ML_Imputed$WN.M
ML_na_marked$WORT.COLOR <- ML_Imputed$WORT.COLOR
ML_na_marked$YIELD <- ML_Imputed$YIELD

# visualize imputations
ggplot(ML_na_marked) + geom_point(aes(BETA.GLUCAN, B.STARCH.WGA, color = B.STARCH.WGA.NA))

ggplot(ML_na_marked) + geom_point(aes(OD, S.T, color = S.T.NA))

ggplot(ML_na_marked) + geom_point(aes(SOLUBLE.PROTEIN, S.T, color = SOLUBLE.PROTEIN.NA))

ggplot(ML_na_marked) + geom_point(aes(TURBIDITY, SOLUBLE.PROTEIN, color = TURBIDITY.NA))

ggplot(ML_na_marked) + geom_point(aes(TW, YIELD, color = TW.NA, shape = YIELD.NA))

ggplot(ML_na_marked) + geom_point(aes(TW, FGW, color = TW.NA))

ggplot(ML_na_marked) + geom_point(aes(TURBIDITY, WORT.COLOR, color = WORT.COLOR.NA))

ggplot(ML_na_marked) + geom_point(aes(YIELD, SOLUBLE.PROTEIN, color = YIELD.NA))
