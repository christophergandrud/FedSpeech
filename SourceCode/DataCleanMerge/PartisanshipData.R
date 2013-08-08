#############
# Create Partisanship Data
# Christopher Gandrud
# 8 August 2013
############

# Load package
library(foreign)

# Load data from Gandrud and Grafstrom (2013)
AllCongress <- read.dta("/General_Data/Partisan_Congress/us_congress_1965_2013.dta")

AllPres <- read.dta("/Users/christophergandrud/Dropbox/GreenBook/Base_Data/PresidentBase.dta")
AllPres <- AllPres[-190:-191,]

AllPartComb <- cbind(AllPres, AllCongress)

# Clean up
VarsKeep <- c("Quarter", "pres_party", "house_dem_rep", "senate_dem_rep")
CombPartisan <- AllPartComb[, VarsKeep]

write.csv(CombPartisan, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/PartisanData.csv", row.names = FALSE)