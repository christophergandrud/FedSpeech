############
# Merge together data for regressions
# Christopher Gandrud
# 7 August 2013
############

##### Econ Data ####
# Load e.divGG function
library(devtools)
source_gist("5675688")

# Load econ data
EconData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/FREDEconData.csv")

# Drop if missing
EconData <- subset(EconData, !is.na(U6RATE))
EconData <- subset(EconData, !is.na(GDPDEF))

#### Speeches Data ####

#### Topics Data ####

#### Congressional Scrutiny States ####

#### Final Clean #### 