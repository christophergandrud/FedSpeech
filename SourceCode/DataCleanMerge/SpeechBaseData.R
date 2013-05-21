################
# Clean Base Speech Data 
# Christopher Gandrud
# 21 May 2013
################

# Load libraries
library(lubridate)
library(DataCombine)
library(gdata)

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/")

# Load data
OrgData <- read.csv("FedSpeechesVersionMay2013.csv")
OrgData$full_date <- as.character(OrgData$full_date)

# Create DateField variable
# Find month and quarter variables
OrgData$Date <- dmy(OrgData$full_date) 

OrgData$DateField <- floor_date(OrgData$Date, "month")

FrontVars <- c("Date", "DateField")

for (i in FrontVars){
	OrgData <- MoveFront(OrgData, i)
}

# Remove unwanted variables
RemoveVars <- c("full_date", "vars.link")

OrgData <- remove.vars(OrgData, RemoveVars)

# Save
write.csv(OrgData, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/SpeechBaseData.csv")