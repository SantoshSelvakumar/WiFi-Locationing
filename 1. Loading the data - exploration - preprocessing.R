###############################################################################
#                                                                             #
#   WIFI | PREPROCESSING THE DATA | VERSION 7.0                               #
#                                                                             #
#   connected to results V7, preprocessed: values between -30-0, +100, 
#   zero var,          #                                                                             #
###############################################################################
# libraries ----
if (require(pacman) == FALSE) {
  install.packages('pacman')
}

pacman::p_load(readr, psych, ggplot2, dplyr, tidyverse)

# # set working directory
# setwd("~/Google Drive/BSSA - data analytics/Module 3/IoT - wifi/wifi")

# load training data file
trainingData <- read.csv("C:/Users/Santosh/Desktop/Ubiqum/RStudio/WIFI_Locationing/trainingData.csv")
validationData <- read.csv("C:/Users/Santosh/Desktop/Ubiqum/RStudio/WIFI_Locationing/validationData.csv")

#Move the info to the front and gather the data
trainingData_Gathered <- trainingData[ , c((ncol(trainingData)-8):(ncol(trainingData)), 1:(ncol(trainingData)-9))]
trainingData_Gathered <- gather(trainingData_Gathered, WAP, DBM, 10:ncol(trainingData_Gathered))

# create subsets for further investigation
SUBSET <- trainingData %>%
  filter(BUILDINGID == BUILDINGID,
         FLOOR == BUILDINGID)

# check for missing data
trainingData[!complete.cases(trainingData),] 

# change structure according to attribute information in the resources:
trainingData$LONGITUDE <- as.factor(trainingData$LONGITUDE)
trainingData$LATITUDE <- as.factor(trainingData$LATITUDE)
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
trainingData$SPACEID <- as.factor(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
trainingData$TIMESTAMP <- as.POSIXct(trainingData$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')

# do the same for the validationData set
validationData$LONGITUDE <- as.numeric(validationData$LONGITUDE)
validationData$LATITUDE <- as.numeric(validationData$LATITUDE)
validationData$FLOOR <- as.factor(validationData$FLOOR)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)
validationData$USERID <- as.factor(validationData$USERID)
validationData$PHONEID <- as.factor(validationData$PHONEID)
validationData$TIMESTAMP <- as.POSIXct(validationData$TIMESTAMP, origin = '1970-01-01', tz = 'GMT')

# remove measurements made by user 6 in building 2, floor 4 
trainingData <- trainingData %>%
  filter(!(USERID == 6 & FLOOR == 4 & BUILDINGID == 2))

# turn +100 values into -120 ----
trainingData[trainingData == 100] <- -120
validationData[validationData == 100] <- -120

# remove values of higher than -30 ---
list30 <- apply(trainingData %>% select(starts_with("WAP")),1, max ) > -30 # creates list of rows 
sum(list30) / length(list30) # to see how often they occur are in the trainingData
trainingData <- trainingData[!list30,] # remove

# save the rows for further investigation (script 1.c)
Outliers30 <- trainingData[list30,]
saveRDS(Outliers30, file = "C:/Users/Santosh/Desktop/Ubiqum/RStudio/WIFI_Locationing_2/Outliers.rds")
rm(Outliers30)

# dubbel check if they occur in validationData, if so: remove.
list30 <- apply(validationData %>% select(starts_with("WAP")),1, max ) > -30
sum(list30) / length(list30)

# split and bind datasets to identify WAPS with zero variance:
trainingDataWAPS <- trainingData %>% select(starts_with("WAP"))
trainingDataINFO <- trainingData[,c((ncol(trainingData)-8):(ncol(trainingData)))]
validationDataWAPS <- validationData%>% select(starts_with("WAP"))
validationDataINFO <- validationData[,c((ncol(trainingData)-8):(ncol(trainingData)))]

# remove rows (1) and columns (2) with zero variance in the trainingdata (V2) ----
listZVRows <- apply(trainingDataWAPS %>% select(starts_with("WAP")),1, var ) == 0
listZVCols <- apply(trainingDataWAPS %>% select(starts_with("WAP")),2, var ) == 0
sum(listZVRows) / length(listZVRows) # see how much % it is from the data
sum(listZVCols) / length(listZVCols) # see how much % it is from the data
trainingDataWAPS <- trainingDataWAPS[,!listZVCols] # remove
trainingDataWAPS <- trainingDataWAPS[!listZVRows,] # remove
trainingDataINFO <- trainingDataINFO[!listZVRows,] # remove

# identify rows (1) and columns (1) with zero variance in the validationdata (V2) ----
listZVRows2 <- apply(validationDataWAPS %>% select(starts_with("WAP")),1, var ) == 0
listZVCols2 <- apply(validationDataWAPS %>% select(starts_with("WAP")),2, var ) == 0
sum(listZVRows2) / length(listZVRows2) # see how much % it is from the data
sum(listZVCols2) / length(listZVCols2) # see how much % it is from the data
validationDataWAPS <- validationDataWAPS[!listZVRows2,] # remove
validationDataINFO <- validationDataINFO[!listZVRows2,] # remove

# select the columns of with ZERO VAR and save them for further investigation ----
trainingZVCols <- trainingData[,listZVCols]
validationZVCols <- validationData[listZVCols2] 
saveRDS(trainingZVCols, file = "C:/Users/Santosh/Desktop/Ubiqum/RStudio/WIFI_Locationing_2/train-zerovar-column")
saveRDS(validationZVCols, file = "C:/Users/Santosh/Desktop/Ubiqum/RStudio/WIFI_Locationing_2/validate-zerovar-column")
rm(trainingZVCols)
rm(validationZVCols)

# bind WAPS back to INFO for complete trainingData & validationData ----
trainingData <- cbind(trainingDataWAPS,trainingDataINFO)
validationData <- cbind(validationDataWAPS,validationDataINFO)
names(trainingData) # check if all columns are there
names(validationData) # check if all columns are there

rm(trainingDataWAPS)
rm(trainingDataINFO)
rm(validationDataWAPS)
rm(validationDataINFO)

# store the files in order to load them in another scirpt
saveRDS(trainingData, file = "C:/Users/Santosh/Desktop/Ubiqum/RStudio/WIFI_Locationing_2/trainingDataProc(V7).rds")
saveRDS(validationData, file = "C:/Users/Santosh/Desktop/Ubiqum/RStudio/WIFI_Locationing_2/validationDataProc(V7).rds")

# clean your global environment
rm(list = ls())

# future research:  the role of duplicates

