############
# Merge Data by quarter
# Christopher Gandrud
# 18 May 2013
############

# Packages 
library(reshape2)
library(lubridate)

setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/")

####-------- Testimony & Speeches Count ------------####
# Load testimony per month
TestCount <- read.csv("TestimonyPerMonth.csv")

# Clean
TestCount <- rename(TestCount, c("MonthYear" = "DateField"))
TestCount <- TestCount[, -1]

####-------- EconData -----------------------------####
EconData <- read.csv("FREDEconData.csv")
EconData <- EconData[, -1]

####-------- Merge -------------------------------####
Combined <- merge(TestCount, EconData, by = "DateField")