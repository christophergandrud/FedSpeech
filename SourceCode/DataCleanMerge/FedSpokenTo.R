#############
# Create Federal Reserve Spoke At Dummy
# Christopher Gandrud
# 8 August 2013
#############

# Load package
library(lubridate)

Full <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/FedSpeechesCorrected-NEW.csv", stringsAsFactors = FALSE)

Sub <- Full[, c("date_of_speech", "name", "org_spoken_at")]

rm(Full)

Sub$Fed <- grepl("Federal Reserve", x = Sub$org_spoken_at)
Sub$FedSpoketoFed[Sub$Fed == TRUE] <- 1
Sub$FedSpoketoFed[Sub$Fed == FALSE] <- 0

Sub <- Sub[, -4]

names(Sub) <- c("full_date", "name", "org_spoken_at", "FedSpoketoFed")

Sub$full_date <- dmy(Sub$full_date)