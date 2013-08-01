#############
# Merge Non-Rotating Board/Org Connectivity data in to create main connectivity
# Christopher Gandrud
# 1 August 2013
#############

# Load data
SpeechesData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/FedSpeechesCorrected-NEW.csv")

# Clean data
VarsKeep <- c("date_of_speech", "year_of_speech", "BX_spelling", "HFSC_donate_chair",
              "HFSC_rankmem", "X")

SpeechSmall <- SpeechesData[, VarsKeep]

names(SpeechSmall) <- c("date_of_speech", "year", "Organisation", 
                        "HFSC_ChairConnect", "HFSC_RankMembConnect", 
                        "SpeakerConnect")

SpeechSmall$SpeakerConnect[is.na(SpeechSmall$SpeakerConnect)] <- 0

HFSCClean <- function(x){
  for (i in x){
    SpeechSmall[, i] <- as.character(SpeechSmall[, i])
    SpeechSmall[, i][SpeechSmall[, i] == "#VALUE!"] <- NA
    SpeechSmall[, i] <- gsub("%", "", x = SpeechSmall[, i])
    SpeechSmall[, i] <- as.numeric(SpeechSmall[, i])
  }
  # Combined donation connectivity variable
  SpeechSmall$HFSC_CombConnect <- SpeechSmall[, x[1]] + SpeechSmall[, x[2]]
  SpeechSmall
}

x <- c("HFSC_ChairConnect", "HFSC_RankMembConnect")
SpeechSmall <- HFSCClean(x)

#### Merge with non-rotating Board connectivity
# Created by  FedSpeech/SourceCode/NetworkCentrality/FedConnectionsGraph.R
setwd("~/Dropbox/Fed Hearings/EVScores31July2013/")

MergeNonRotate <- function(year){
  SpeechOut <- data.frame()
  for (i in year){
    Temp <- read.csv(paste0(i, ".csv"))
    TempSpeech <- subset(SpeechSmall, year == i)
    TempSpeech <- merge(TempSpeech, Temp, by = c("Organisation", "year"),
                         all.x = TRUE, all.y = FALSE)
    TempSpeech$FedBoardCentrality[is.na(TempSpeech$FedBoardCentrality)] <- 0
    SpeechOut <- rbind(SpeechOut, TempSpeech)
  }
  SpeechOut
}

yearList <- 1997:2013
SpeechComb <- MergeNonRotate(yearList)

#### Change Point 1st Go
# Load e.divGG function
library(devtools)
source_gist("5675688")

library(lubridate)

SpeechComb$date <- dmy(SpeechComb$date_of_speech)

ConVars <- c("SpeakerConnect", "HFSC_CombConnect", "FedBoardCentrality")
e.divGG(data = SpeechComb, Vars = "FedBoardCentrality", TimeVar = "date", 
        Titles = ConVars, sig.lvl = 0.1, R = 999, min.size = 40)