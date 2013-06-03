##############
# Fed Change Point Play 3
# Christopher Gandrud
# 3 June 2013
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
#MainData <- subset(MainData, name != "Roger W. Ferguson Jr." | is.na(name))
MainData <- subset(MainData, laughter < 25 | is.na(laughter))

# Drop incomplete data
SubMain <- MainData[, c("Date", "NonFedFinanceCom", "CleanFullCommitteeName1", 
                        "legislature", "laughter", "Field")]
SubMain <- SubMain[year(SubMain$Date) > 2000, ]

# Create Laughter variables for Full Finance and Fed Testimony
SubMain$Date <- ymd(SubMain$Date)
SubMain$MonthYear <- floor_date(SubMain$Date, "month")

SubMain <- DropNA(SubMain, "laughter")

# Hearings count filler
SubMain$Any <- 1

# Sub Counts
MonthLaughter <- function(FedorNot, NewSumName, NewMeanName, Legislature = NULL){
  if (!is.null(Legislature)){
    SubMain <- subset(SubMain, legislature == Legislature)
  }
  SubTemp <- subset(SubMain, NonFedFinanceCom == FedorNot)
  if (FedorNot == 1){
    SubTemp <- subset(SubTemp, Field == 0)
  }
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempSum = sum(Any))
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempMean = mean(laughter))
  SubTemp <- SubTemp[!duplicated(SubTemp[, "MonthYear"]), ]
  SubTemp <- SubTemp[, c("MonthYear", "TempSum", "TempMean")]
  names(SubTemp) <- c("MonthYear", NewSumName,  NewMeanName)
  SubTemp
}

## Non Fed
SubNonFed <- MonthLaughter(1, NewSumName = "SumNonFed", NewMeanName = "NonFedLaughter") 

## Fed 
SubFedHouse <- MonthLaughter(FedorNot = 0, NewSumName = "SumFedHouse", 
                        NewMeanName = "FedLaughterHouse", Legislature = "House")

SubFedSenate <- MonthLaughter(FedorNot = 0, NewSumName = "SumFedSenate", 
                        NewMeanName = "FedLaughterSenate", Legislature = "Senate")


#### ---- Merge in Econ Vars ---- ####
EconData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/FREDEconData.csv")

# Clean
EconData <- EconData[, -1]
EconData <- rename(EconData, c("DateField" = "MonthYear"))
EconData$MonthYear <- ymd(as.character(EconData$MonthYear))
EconData <- EconData[year(EconData$MonthYear) > 2000,]

# Merge
CombNonFed <- merge(SubNonFed, EconData, by = "MonthYear", all = TRUE)
CombNonFed <- CombNonFed[, c("MonthYear", "SumNonFed", "NonFedLaughter")]
CombFedHouse <- merge(SubFedHouse, EconData, by = "MonthYear", all = TRUE)
CombFedHouse <- CombFedHouse[, c("MonthYear", "SumFedHouse", "FedLaughterHouse")]
CombFedSenate <- merge(SubFedSenate, EconData, by = "MonthYear", all = TRUE)

Combined <- merge(CombNonFed, CombFedSenate, by = "MonthYear")
Combined <- merge(Combined, CombFedHouse, by = "MonthYear")


attach(Combined)
Combined$NonFedLaughter[is.na(NonFedLaughter)] <- 0
Combined$FedLaughterHouse[is.na(FedLaughterHouse)] <- 0
Combined$FedLaughterSenate[is.na(FedLaughterSenate)] <- 0
Combined$SumNonFed[is.na(SumNonFed)] <- 0
Combined$SumFedHouse[is.na(SumFedHouse)] <- 0
Combined$SumFedSenate[is.na(SumFedSenate)] <- 0
detach(Combined)

# write.csv(Combined, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/ChangePointNote/MainHearings.csv")

#### ------- Play Change Point ----- ####
e.divGG(data = Combined, Vars = c("SumFedHouse", "SumFedSenate", "FedLaughterHouse", "FedLaughterSenate"), TimeVar = "MonthYear", 
        sig.lvl = 0.1, R = 999, min.size = 48)

e.divGG(data = Combined, Vars = c("NonFedLaughter", "SumNonFed"),
        TimeVar = "MonthYear", 
        sig.lvl = 0.1, R = 999, min.size = 6)
