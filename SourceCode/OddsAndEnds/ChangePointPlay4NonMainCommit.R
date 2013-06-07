##############
# Fed Change Point Play 4: Non-Main Committees
# Christopher Gandrud
# 7 June 2013
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

# Drop incomplete data
SubMain <- MainData[, c("Date", "NonFedFinanceCom", "CleanFullCommitteeName1", 
                        "CleanFullCommitteeName2", "legislature", "laughter",
                        "attendance")]
SubMain$Date <- dmy(SubMain$Date)
SubMain <- SubMain[order(SubMain$Date),]

# Create Laughter variables for Full Finance and Fed Testimony
SubMain$MonthYear <- floor_date(SubMain$Date, "month")

SubMain <- DropNA(SubMain, "laughter")
SubMain <- DropNA(SubMain, "attendance")


attach(SubMain)
# NAs for subcommittes that are missing
SubMain$CleanFullCommitteeName2[CleanFullCommitteeName2 == ""] <- NA
SubMain$CleanFullCommitteeName2[CleanFullCommitteeName2 == "NA"] <- NA
# Var for non full main committees
SubMain$Main <- 0
SubMain$Main[CleanFullCommitteeName1 == "Committee on Banking, Housing, and Urban Affairs" & is.na(CleanFullCommitteeName2)] <- 1
SubMain$Main[CleanFullCommitteeName1 == "Committee on Financial Services" & is.na(CleanFullCommitteeName2)] <- 1
SubMain$Main[CleanFullCommitteeName1 == "Committee on Banking and Financial Services" & is.na(CleanFullCommitteeName2)] <- 1
SubMain$Main[CleanFullCommitteeName1 == "Committee on Banking and Financial Services" & is.na(CleanFullCommitteeName2)] <- 1
detach(SubMain)

# Drop if a main committee hearing
SubMain <- subset(SubMain, Main == 0)

# Hearings count filler
SubMain$Any <- 1

# Sub Counts
MonthLaughter <- function(NewSumName, NewMeanName, Legislature = NULL){
  SubTemp <- SubMain
  if (!is.null(Legislature)){
    SubTemp <- subset(SubMain, legislature == Legislature)
  }
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempSum = sum(Any))
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempMean = mean(laughter)) 
  SubTemp <- ddply(SubTemp, .(MonthYear), transform, TempMeanAttend = mean(attendance, na.rm = TRUE))
  SubTemp <- SubTemp[!duplicated(SubTemp[, "MonthYear"]), ]
  SubTemp <- SubTemp[, c("MonthYear", "TempSum", "TempMean", "TempMeanAttend")]
  names(SubTemp) <- c("MonthYear", NewSumName,  NewMeanName, "Attend")
  SubTemp
}

## Fed 
SubFedNonMain <- MonthLaughter(NewSumName = "SumFedNonMain", 
                        NewMeanName = "FedLaughterNonMain")

#### ---- Merge in Econ Vars ---- ####
EconData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/FREDEconData.csv")

# Clean
EconData <- EconData[, -1]
EconData <- rename(EconData, c("DateField" = "MonthYear"))
EconData$MonthYear <- ymd(as.character(EconData$MonthYear))
EconData <- EconData[year(EconData$MonthYear) > 2000, ]

# Merge
Combined <- merge(SubFedNonMain, EconData, by = "MonthYear", all = TRUE)

attach(Combined)
Combined$FedLaughterNonMain[is.na(FedLaughterNonMain)] <- 0
Combined$SumFedNonMain[is.na(Attend)] <- 0
Combined$SumFedNonMain[is.na(SumFedNonMain)] <- 0
detach(Combined)

#write.csv(Combined, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/ChangePointNote/NonMainFullHearings.csv")

#### ------- Play Change Point ----- ####
ScrutVars <- c("SumFedNonMain", "Attend", "FedLaughterNonMain")
e.divGG(data = Combined, Vars = ScrutVars, TimeVar = "MonthYear", 
        sig.lvl = 0.1, R = 1999, min.size = 30)
