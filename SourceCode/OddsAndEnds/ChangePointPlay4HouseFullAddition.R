##############
# Fed Change Point Play 4: House
# Christopher Gandrud
# 9 June 2013
##############

library(lubridate)
library(DataCombine)
library(plyr)
library(digest)
library(devtools)

# Load e.divGG function
#source_gist("5675688")
source("~/Desktop/e.divGG.R")

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/")

MainData <- read.csv("TestFullPlay.csv", stringsAsFactors = FALSE)

MainData$name[MainData$name == "man, Roger W. Ferguson Jr."] <- "Roger W. Ferguson Jr."

# Drop incomplete data
SubMain <- MainData[, c("Date", "NonFedFinanceCom", "CleanFullCommitteeName1", 
                        "CleanFullCommitteeName2", "legislature", "laughter",
                        "attendance", "Field", "FedLetterCorrespondence")]
SubMain$Date <- dmy(SubMain$Date)
SubMain <- SubMain[order(SubMain$Date),]

# Create Laughter variables for Full Finance and Fed Testimony
SubMain$MonthYear <- floor_date(SubMain$Date, "month")

SubMain <- DropNA(SubMain, "laughter")

SubMain$Field[is.na(SubMain$Field)] <- 0

# NAs for subcommittes that are missing
attach(SubMain)
  SubMain$CleanFullCommitteeName2[CleanFullCommitteeName2 == ""] <- NA
  SubMain$CleanFullCommitteeName2[CleanFullCommitteeName2 == "NA"] <- NA
detach(SubMain)

# Keep only full HFSC
SubMain <- subset(SubMain, 
                  CleanFullCommitteeName1 == "Committee on Financial Services" |
                  CleanFullCommitteeName1 == "Committee on Banking and Financial Services" |
                  CleanFullCommitteeName1 == "Committee on Banking and Financial Services")
SubMain <- subset(SubMain, is.na(CleanFullCommitteeName2))

# Numeric Attendance #### 
SubMain$attendance <- as.numeric(SubMain$attendance)
SubMain <- subset(SubMain, !is.na(attendance))

# Numeric FedLetterCorrespondence #### 
SubMain$FedLetterCorrespondence <- as.numeric(SubMain$FedLetterCorrespondence)
##### Assume NA is 0 ####
SubMain$FedLetterCorrespondence[is.na(SubMain$FedLetterCorrespondence)] <- 0

# Hearings count filler
SubMain$Any <- 1

# Sub Counts
MonthLaughter <- function(FedorNot, NewSumName, NewLaughName, NewAttendName, NewLetterName, Legislature = NULL){
  if (!is.null(Legislature)){
    SubMain <- subset(SubMain, legislature == Legislature)
  }
  SubTemp <- subset(SubMain, NonFedFinanceCom == FedorNot)
  if (FedorNot == 1){
    SubTemp <- subset(SubTemp, Field == 0)
  }
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempSum = sum(Any))
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempMean = mean(laughter, na.rm = TRUE))
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempMeanAttend = mean(attendance, na.rm = TRUE))
  if (FedorNot == 0){
    SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempMeanLetter = mean(FedLetterCorrespondence, na.rm = TRUE))
    SubTemp <- SubTemp[!duplicated(SubTemp[, "MonthYear"]), ]
    SubTemp <- SubTemp[, c("MonthYear", "TempSum", "TempMean", 
                           "TempMeanAttend", "TempMeanLetter")]
    names(SubTemp) <- c("MonthYear", NewSumName,  NewLaughName, 
                        NewAttendName, NewLetterName)
  }
  if (FedorNot == 1){
    SubTemp <- SubTemp[!duplicated(SubTemp[, "MonthYear"]), ]
    SubTemp <- SubTemp[, c("MonthYear", "TempSum", "TempMean", 
                           "TempMeanAttend")]
    names(SubTemp) <- c("MonthYear", NewSumName,  NewLaughName, 
                        NewAttendName)
  }
  SubTemp
}

## Non Fed
SubNonFed <- MonthLaughter(1, NewSumName = "SumNonFed", 
                           NewLaughNam = "NonFedLaughter",
                           NewAttendName = "NonFedAttend")

## Fed 
SubFedHouse <- MonthLaughter(FedorNot = 0, NewSumName = "SumFedHouse", 
                              NewLaughNam = "FedLaughterHouse",
                              NewAttendName = "FedAttend",
                              NewLetterName = "FedLetter", 
                              Legislature = "House")


#### ---- Merge in Econ Vars ---- ####
EconData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/FREDEconData.csv")

# Clean
EconData <- EconData[, -1]
EconData <- rename(EconData, c("DateField" = "MonthYear"))
EconData$MonthYear <- ymd(as.character(EconData$MonthYear))
EconData <- EconData[year(EconData$MonthYear) >= 1997, ]
EconData <- EconData[-1:-4, ]

# Merge
CombNonFed <- merge(SubNonFed, EconData, by = "MonthYear", all = TRUE)
CombNonFed <- CombNonFed[, c("MonthYear", "SumNonFed", "NonFedLaughter",
                             "NonFedAttend")]
CombFedHouse <- merge(SubFedHouse, EconData, by = "MonthYear", all = TRUE)

Combined <- merge(CombNonFed, CombFedHouse, by = "MonthYear")

attach(Combined)
  Combined$NonFedLaughter[is.na(NonFedLaughter)] <- 0
  Combined$FedLaughterHouse[is.na(FedLaughterHouse)] <- 0
  Combined$SumNonFed[is.na(SumNonFed)] <- 0
  Combined$SumFedHouse[is.na(SumFedHouse)] <- 0
  Combined$FedAttend[is.na(FedAttend)] <- 0
  Combined$NonFedAttend[is.na(NonFedAttend)] <- 0
  Combined$NonFedLetter[is.na(NonFedLetter)] <- 0
  Combined$FedLetter[is.na(FedAttend)] <- 0
detach(Combined)

# write.csv(Combined, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/ChangePointNote/HouseFullHearings.csv")

#### ------- Play Change Point ----- ####
ScrutVars <- c("SumFedHouse", "FedAttend", "FedLaughterHouse", "FedLetter")
e.divGG(data = Combined, Vars = ScrutVars, TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 1999, min.size = 24)

NonFedVars <- c("SumNonFed", "NonFedAttend", "NonFedLaughter")
e.divGG(data = Combined, Vars = NonFedVars,
        TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 999, min.size = 24)
