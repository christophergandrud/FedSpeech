############
# Econ Change Point with Case-Shiller 
# Christopher Gandrud
# 31 July 2013
###########

# Load e.divGG function
library(devtools)
source_gist("5675688")

# Load econ data
EconData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/FREDEconData.csv")

# Drop if missing
EconData <- subset(EconData, !is.na(U6RATE))
EconData <- subset(EconData, !is.na(GDPDEF))

## Change Point Analysis
EconVars <- c("PCEPIPercent", "UnemploymentRateChange", "GDPC96Percent", "CaseShillerChange")
EconTitles <- c("Inflation", "Unemployment (% Change)", "Growth", "Case-Shiller Index (% Change)")
e.divGG(data = EconData, Vars = EconVars, TimeVar = "DateField", 
        Titles = EconTitles, sig.lvl = 0.05, R = 999, min.size = 24)

e.divGG(data = EconData, Vars = "CaseShillerChange", TimeVar = "DateField", 
        Titles = "Case-Shiller Index % Change", sig.lvl = 0.05, R = 999, min.size = 24)