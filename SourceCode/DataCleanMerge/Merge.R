############
# Merge Data by quarter
# Christopher Gandrud
# 21 May 2013
############

# Packages 
library(plyr)
library(DataCombine)
library(lubridate)

setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/")

####-------- Testimony & Speeches Count ------------####
# Load testimony per month
TestCount <- read.csv("TestimonyPerMonth.csv")

# Clean
TestCount <- rename(TestCount, c("MonthYear" = "DateField"))
TestCount <- TestCount[, -1]

# Add lag variables
for (i in -1:-3){
	TestCount <- slide(TestCount, Var = "MonthTestTotal", slideBy = i)
}

# Add lead variables
## Note: Currently assume that all testimony for the following two months is known.
for (i in 1:2){
	TestCount <- slide(TestCount, Var = "MonthTestTotal", slideBy = i)
}

####-------- EconData -----------------------------####
EconData <- read.csv("FREDEconData.csv")

# Add lag variables
EconVars <- names(EconData[, -1])

for (u in EconVars){
	for (i in -1:-2){
	  EconData <- slide(EconData, Var = u, slideBy = i)
	}
}

####-------- Speeches Orgs -----------------------####
SpeechBase <- read.csv("SpeechBaseData.csv")
SpeechBase <- SpeechBase[, -1]


####-------- Merge -------------------------------####
Combined <- merge(TestCount, EconData, by = "DateField")

Combined <- merge(SpeechBase, Combined, by = "DateField")

