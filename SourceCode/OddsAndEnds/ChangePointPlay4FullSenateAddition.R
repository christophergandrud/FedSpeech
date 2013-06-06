##############
# Fed Change Point Play 4: Senate
# Christopher Gandrud
# 6 June 2013
##############

library(lubridate)
library(DataCombine)
library(plyr)
library(digest)
library(devtools)

# Load e.divGG function
source_gist("5675688")


# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/")

MainData <- read.csv("TestFullPlay.csv", stringsAsFactors = FALSE)

MainData$name[MainData$name == "man, Roger W. Ferguson Jr."] <- "Roger W. Ferguson Jr."
# MainData <- subset(MainData, name != "Roger W. Ferguson Jr." | is.na(name))
# MainData <- subset(MainData, laughter < 25 | is.na(laughter))

# Drop incomplete data
SubMain <- MainData[, c("Date", "NonFedFinanceCom", "CleanFullCommitteeName1", 
                        "CleanFullCommitteeName2", "legislature", "laughter",
                        "attendance")]
SubMain$Date <- dmy(SubMain$Date)
SubMain <- SubMain[order(SubMain$Date),]

# Create Laughter variables for Full Finance and Fed Testimony
SubMain$MonthYear <- floor_date(SubMain$Date, "month")

SubMain <- DropNA(SubMain, "laughter")

# NAs for subcommittes that are missing
SubMain$CleanFullCommitteeName2[SubMain$CleanFullCommitteeName2 == ""] <- NA

# Keep only full HFSC
SubMain <- subset(SubMain, 
                       CleanFullCommitteeName1 == "Committee on Banking, Housing, and Urban Affairs")
SubMain <- subset(SubMain, is.na(CleanFullCommitteeName2))

# Hearings count filler
SubMain$Any <- 1

# Sub Counts
MonthLaughter <- function(NewSumName, NewMeanName, Legislature = NULL){
  if (!is.null(Legislature)){
    SubTemp <- subset(SubMain, legislature == Legislature)
  }
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempSum = sum(Any))
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempMean = mean(laughter))
##
  #SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempMeanAttend = mean(attendance, an.rm = TRUE))
##  
  SubTemp <- SubTemp[!duplicated(SubTemp[, "MonthYear"]), ]
  SubTemp <- SubTemp[, c("MonthYear", "TempSum", "TempMean")]
  names(SubTemp) <- c("MonthYear", NewSumName,  NewMeanName)
  SubTemp
}


## Fed 
SubFedSenate <- MonthLaughter(NewSumName = "SumFedSenate", 
                        NewMeanName = "FedLaughterSenate", Legislature = "Senate")


#### ---- Merge in Econ Vars ---- ####
EconData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/FREDEconData.csv")

# Clean
EconData <- EconData[, -1]
EconData <- rename(EconData, c("DateField" = "MonthYear"))
EconData$MonthYear <- ymd(as.character(EconData$MonthYear))
EconData <- EconData[year(EconData$MonthYear) > 2000, ]

# Merge
Combined <- merge(SubFedSenate, EconData, by = "MonthYear", all = TRUE)

attach(Combined)
Combined$FedLaughterSenate[is.na(FedLaughterSenate)] <- 0
Combined$SumFedSenate[is.na(SumFedSenate)] <- 0
detach(Combined)

#write.csv(Combined, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/ChangePointNote/SenateFullHearings.csv")

#### ------- Play Change Point ----- ####
ScrutVars <- c("SumFedSenate", "FedLaughterSenate")
e.divGG(data = Combined, Vars = ScrutVars, TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 1999, min.size = 24)
