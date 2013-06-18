#################
# Merge Fed Speeches with Fed Orgs
# Christopher Gandrud
# 18 June 2013
#################

library(lubridate)
library(stringr)

setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/")

# Load and clean SpeakersDonors data
SpeakersDonors <- read.csv("SpeakersDonors.csv", stringsAsFactors = FALSE) 

# Subset key variables
SpeakersDonors$Date <- dmy(SpeakersDonors$date_of_speech)
SpeakersDonors$Year <- year(SpeakersDonors$Date)
SubDonors <- SpeakersDonors[, c("Year", "org_spoken_at",
                           "chair_hfsc_contribution_from_org",
                           "rnkmem_hfsc_contribution_from_org")]

names(SubDonors) <- c("Year", "Organisation", "ChairCont", "RankMembCont")

# Sub No Data
SubDonors <- subset(SubDonors, ChairCont != "NO DATA AVAILABLE")

# Set 0 if no donor
SubDonors$ChairCont[SubDonors$ChairCont == "NONE"] <- 0
SubDonors$RankMembCont[SubDonors$RankMembCont == "NONE"] <- 0
SubDonors$ChairCont[SubDonors$ChairCont == ""] <- 0
SubDonors$RankMembCont[SubDonors$RankMembCont == ""] <- 0

SubDonors$ChairCont[SubDonors$ChairCont != "0"] <- 1
SubDonors$RankMembCont[SubDonors$RankMembCont != "0"] <- 1

SubDonors$ChairCont <- as.numeric(SubDonors$ChairCont)
SubDonors$RankMembCont <- as.numeric(SubDonors$RankMembCont)

# Standardise Organisation Names
NameChange <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/StandardisedOrgNames.csv", stringsAsFactors = FALSE)

SubDonors$Organisation <- as.character(SubDonors$Organisation)
NameRows <- 1:nrow(NameChange)

for (i in NameRows){
SubDonors$Organisation <- str_replace(string = SubDonors$Organisation, 
                           pattern = gsub("\\\\", "\\", NameChange$From[i], 
                                          fixed = TRUE), 
                           replacement = NameChange$To[i])
}

# Create Congress Connection
HCFSChair <- subset(SubDonors, ChairCont == 1)
HCFSChair$Connect <- "HCFS Chair"
HCFSChair <- HCFSChair[, c("Year", "Organisation", "Connect")]

HCFSRanking <- subset(SubDonors, RankMembCont == 1)
HCFSRanking$Connect <- "HCFS Ranking"
HCFSRanking <- HCFSRanking[, c("Year", "Organisation", "Connect")]

CongressDonors <- rbind(HCFSRanking, HCFSChair)

CongressDonors <- CongressDonors[!duplicated(CongressDonors[, 1:3]),]

NonDonorSpeech <- SubDonors[, c("Year", "Organisation")]
NonDonorSpeech$Speech <- 1

#### ------------- Load Fed Connected Orgs ------------------ ####
ConnectedOrgs <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/BasicFedNetwork.csv")

FedConnected <- ConnectedOrgs[, c("Year", "Organisation")]
FedConnected <- FedConnected[!duplicated(FedConnected[, 1:2]),]

FedSpeech <- merge(FedConnected, NonDonorSpeech, by = union("Year", "Organisation"))

FedSpeech <- subset(FedSpeech, Year >= 1997)
FedSpeech <- FedSpeech[, -3]
FedSpeech$Connect <- "Federal Reserve"

#### ---------- Combine Data Sets & Graph ------------- ####
# Orgs Fed spoke to with connections either to Congress or the Fed
Combined <- rbind(CongressDonors, FedSpeech)

Combined <- Combined[order(Combined$Year, Combined$Organisation), ]

Combined <- Combined[!duplicated(Combined[, 1:3]),]

# write.csv(Combined, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/CongressFedSpeeches.csv")

library(d3Network)

year <- c(1997:2012)

for (i in year){
  FileName <- paste0("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Figures/CongressFedOrgs/FedCongress", i, ".html")
  Sub <- subset(Combined, Year == i)

  d3Network(Sub, file = FileName, 
                Source = "Connect", Target = "Organisation",
                linkDistance = 100, charge = -300, fontsize = 12,
                width = 1000, height = 500)
}



