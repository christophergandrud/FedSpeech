############
# Clean Up Speeches Topic Modeling 
# Christopher Gandrud
# 30 July 2013
############

#### Packages
library(plyr)
library(lubridate)

####--------------- Speeches Base Load/Clean ------------- ####
# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw")

# Load data
OrgData <- read.csv("BaseSpeechCount.csv")
OrgData$full_date <- as.character(OrgData$full_date)

# Find month and quarter variables
OrgData$Date <- dmy(OrgData$full_date) 

OrgData$MonthYear <- floor_date(OrgData$Date, "month")
OrgData$QuarterYear <- quarter(OrgData$Date, with_year = TRUE)

#### -------------- Speech Topic Data Clean Up ---------- ######
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/TopicModeling")

MalletRaw <- read.csv("TopicsinDocs5.csv")

# Remove first two columns
MalletRaw <- MalletRaw[, c(-1, -2, -12)]

# Clean up variable names
names(MalletRaw) <- c("SpeechID", "top1", "top1Prop", "top2", "top2Prop",
                      "top3", "top3Prop", "top4", "top4Prop",
                      "top5Prop", "top5")

# Extract document ID and order correctly
MalletRaw$SpeechID <- gsub("\\\\", "", MalletRaw$SpeechID) # Remove Escapes, i.e. \'s
MalletRaw$SpeechID <- gsub("\\(1\\)May2013SpeechesFedSpeechIndvParsed_20Mayparsed.", "", MalletRaw$SpeechID)
MalletRaw$SpeechID <- gsub(".txt", "", MalletRaw$SpeechID) # Remove Escapes, i.e. \'s

MalletRaw$SpeechID <- as.numeric(MalletRaw$SpeechID)

MalletOrder <- MalletRaw[order(MalletRaw$SpeechID), ]

# Assign topic labels to data
TopicLabels <- c("Financial.Markets", "Macroeconomics", "Monetary.Policy", 
                 "International.Economy",	"Local.Housing.Dev", 
                 "Banking.Regulation")   

TopicLevels <- 0:5 

TopCols <- c("top1", "top2", "top3", "top4", "top5")

for (i in TopCols){
  MalletOrder[, i] <- factor(MalletOrder[, i], levels = TopicLevels,
                            labels = TopicLabels)
}

#### --------------- Merge Speeches Files ------------- ####
CombinedSpeeches <- cbind(MalletOrder, OrgData)

# Keep specific variables
ToKeep <- c("MonthYear", "QuarterYear", "name", "position_cat", "SpeechID",
            "top1", "top1Prop", "top2", "top2Prop", "top3", "top3Prop",
            "top4", "top4Prop", "top5", "top5Prop", "bankersfinance",
            "other_private", "otherregulators", "io", "community_organisations",
            "thinktank", "press_association", "prof_econ_assoc", "university",
            "hearing", "trade_assoc", "non_finance_gov", "nonbuinessadvocacy",
            "social_events", "economic_literacy", "other")

CombClean <- 

