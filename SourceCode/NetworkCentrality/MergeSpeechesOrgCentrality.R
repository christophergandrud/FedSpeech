#############
# Merge Non-Rotating Board/Org Connectivity data in to create main connectivity
# Christopher Gandrud
# 21 August 2013
#############

# Load package
library(lubridate)

# Load data
SpeechesData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/FedSpeechesCorrected-NEW.csv")

# Clean data
VarsKeep <- c("date_of_speech", "year_of_speech", "name", "BX_spelling", 
              "HFSC_donate_chair", "HFSC_rankmem", "X")

SpeechSmall <- SpeechesData[, VarsKeep]

names(SpeechSmall) <- c("full_date", "year", "name", "Organisation", 
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

SpeechComb$full_date <- dmy(SpeechComb$full_date) 
SpeechComb <- SpeechComb[order(SpeechComb$full_date), ]

# write.csv(SpeechComb, 
#          file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/ConnectivityClean.csv",
#          row.names = FALSE)

#### Total number of speeches per year
library(plyr)
library(ggplot2)

SpeechComb$Dumb <- 1
SpeechTemp <- ddply(SpeechComb, .(year), transform, YearTotal = sum(Dumb))

SpeechNoDups <- SpeechTemp[!duplicated(SpeechTemp$year), ]
SpeechNoDups <- subset(SpeechNoDups, year != 2013)
plot(SpeechNoDups$year, SpeechNoDups$YearTotal)

#### Change Point 1st Go
# Load e.divGG function
library(devtools)
source_gist("5675688")

# All connectivity vars
ConVars <- c("HFSC_CombConnect", "SpeakerConnect", "FedBoardCentrality")
ConTitles <- c("HCFS Connected", "Speaker Connected", "Fed. Board Centrality")

e.divGG(data = SpeechComb, Vars = ConVars, TimeVar = "full_date", 
        Titles = ConTitles, sig.lvl = 0.05, R = 999, min.size = 60, JustGraph = FALSE)

e.divGG(data = SpeechComb, Vars = ConVars, TimeVar = "full_date", 
        Titles = ConTitles, sig.lvl = 0.05, R = 999, min.size = 60, JustGraph = TRUE)

# Only board centrality
e.divGG(data = SpeechComb, Vars = "FedBoardCentrality", TimeVar = "full_date", 
        Titles = "Fed. Board Centrality", sig.lvl = 0.05, R = 999, min.size = 40)

# Board centrality, dropping direct speaker connection
SpeechInDirect <- subset(SpeechComb, SpeakerConnect == 0)
e.divGG(data = SpeechInDirect, Vars = "FedBoardCentrality", TimeVar = "full_date", 
        Titles = "Fed. Board Centrality", sig.lvl = 0.05, R = 999, min.size = 40)


#### Create graphs for APSA version of the paper
SubConnect <- SpeechComb[, c("SpeakerConnect", "HFSC_CombConnect", "FedBoardCentrality")]