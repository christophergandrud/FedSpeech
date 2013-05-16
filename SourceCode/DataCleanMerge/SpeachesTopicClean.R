############
# Clean Up Speaches Topic Modeling 
# Christopher Gandrud
# 16 May 2013
############

#### Packages
library(plyr)
library(lubridate)

####--------------- Speeches Base Load/Clean ------------- ####
# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/")

# Load data
OrgData <- read.csv("BaseSpeechCount.csv")
OrgData$full_date <- as.character(OrgData$full_date)

# Find month and quarter variables
OrgData$Date <- dmy(OrgData$full_date) 

OrgData$MonthYear <- floor_date(OrgData$Date, "month")
OrgData$QuarterYear <- quarter_year(OrgData$Date, with_year = TRUE)

#### -------------- Speech Topic Data Clean Up ---------- ######
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/TopicModeling")

MalletRaw <- read.csv("TopicsinDocs.csv")

## Remove mystery document
MalletRaw <- MalletRaw[-1, ]

## Extract speech ID from speech file name 
MalletRaw$filename <- gsub("\\\\", "", MalletRaw$filename) # Remove Escapes, i.e. \'s
MalletRaw$filename <- gsub("C:UsersKevinDesktopfed.text.parsedparsed.", "", MalletRaw$filename)
MalletRaw$filename <- gsub(".txt", "", MalletRaw$filename)

# Assign topic lables
TopicLabels <- c("Economic.Conditions", "Community.Lending", 
				"Financial.Markets", "Monetary.Policy", 
				"Banking.Regulations")   

TopicLevels <- seq(0, 4) 

TopCols <- c("top1", "top2", "top3", "top4", "top5")

MalletLabel <- MalletRaw

for (i in TopCols){
  TopicFact <- factor(MalletRaw[, i], levels = TopicLevels,
                            labels = TopicLabels)
  MalletLabel <- cbind(MalletLabel, Temp)
  MalletLabel
}







