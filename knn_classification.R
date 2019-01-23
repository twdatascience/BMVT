# Set up libraries and working directory
library(tidyverse)
library(class)
library(DMwR)
setwd("C:/users/Meso/Documents/Regis/MSDS 692/R code")

# Load files
Markers<- read.csv('C:\\Users\\Meso\\Documents\\Regis\\MSDS 692\\CSI2015_MARKERS.csv', header = TRUE)
merged <- read.csv('TRAITS_W_MARKERS.csv', header = TRUE, stringsAsFactors = FALSE)
markers_used <- data.frame(MARKER_NM = unique(merged$MARKER_NM), stringsAsFactors = FALSE)
ML <- read.csv('ML_TRAITS.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, 'NA', ' NA', 'NaN', NaN))
ML_Imputed <- read.csv('ML_IMPUTED.csv', header = TRUE, stringsAsFactors = FALSE, na.strings = c(NA, 'NA', ' NA', 'NaN', NaN))

# Find varieties
lines <- data.frame(dbo_LINE_LINE_ID = unique(ML$dbo_LINE_LINE_ID), stringsAsFactors = FALSE)

# Remove plants
Markers$PLANT_ID <- NULL

line_markers <- merge(Markers, lines, by = 'dbo_LINE_LINE_ID')
good_hap <- c('AA', 'BB', 'AB', 'Dupe')
imp_needed <- line_markers %>% filter(! HAP_CALL %in% good_hap)
no_imp_hap <- line_markers %>% filter(HAP_CALL %in% c('AA', 'BB'))
no_imp_hap$dbo_LINE_LINE_NM <- NULL
no_imp_hap$dbo_LINE_1_LINE_ID <- NULL
no_imp_hap$dbo_LINE_1_LINE_NM <- NULL

# Create training set
train_markers <- merge(ML_Imputed, no_imp_hap, by = 'dbo_LINE_LINE_ID')
imp_needed$dbo_LINE_LINE_NM <- NULL
imp_needed$dbo_LINE_1_LINE_ID <- NULL
imp_needed$dbo_LINE_1_LINE_NM <- NULL
imp_needed$MARKER_HAP <- NULL

# Create test set
test_markers <- merge(ML_Imputed, imp_needed, by = 'dbo_LINE_LINE_ID')

# Run knn k =10 for each marker
knn10 <- list()
for (i in 1:nrow(markers_used)) {
  train_set <- filter(train_markers, MARKER_NM == markers_used[i,])
  test_set <- filter(test_markers, MARKER_NM == markers_used[i,])
  if (is.data.frame(train_set) && nrow(train_set) <= 0) next
  if (is.data.frame(test_set) && nrow(test_set) <= 0) next
  k10 <- knn(train_set[,2:26], test_set[,2:26], train_set[,29], k=10)
  test_set$KNN_HAP <- k10
  knn10[[i]] <- as.data.frame(test_set)
}
imp_knn10 <- as.data.frame(bind_rows(knn10))
train_markers$KNN_HAP <- NA
markers_knn <- bind_rows(train_markers, imp_knn10)
ggplot(markers_knn) + geom_boxplot(aes(HAP_CALL, SOLUBLE.PROTEIN, color = KNN_HAP))
