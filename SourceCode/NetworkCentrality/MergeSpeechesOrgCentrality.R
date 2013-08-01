#############
# Merge Non-Rotating Board/Org Connectivity data in to create main connectivity
# Christopher Gandrud
# 1 August 2013
#############

# Load package
library(lubridate)

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

SpeechComb$date_of_speech <- dmy(SpeechComb$date_of_speech) 
SpeechComb <- SpeechComb[order(SpeechComb$date_of_speech), ]


#### Total number of speeches per year
library(plyr)

SpeechComb$Dumb <- 1
SpeechTemp <- ddply(SpeechComb, .(year), transform, YearTotal = sum(Dumb))

SpeechNoDups <- SpeechTemp[!duplicated(SpeechTemp$year), ]
SpeechNoDups <- subset(SpeechNoDups, year != 2013)
plot(SpeechNoDups$year, SpeechNoDups$YearTotal)

#### Change Point 1st Go
# Load e.divGG function
library(devtools)
# source_gist("5675688")

ConVars <- c("SpeakerConnect", "HFSC_CombConnect", "FedBoardCentrality")

e.divGG(data = SpeechComb, Vars = ConVars, TimeVar = "date_of_speech", 
        Titles = ConVars, sig.lvl = 0.1, R = 999, min.size = 60)

e.divGG(data = SpeechComb, Vars = "FedBoardCentrality", TimeVar = "date_of_speech", 
        Titles = "Fed. Board Centrality", sig.lvl = 0.05, R = 999, min.size = 40)