###############
# Changepoint Play
# Christopher Gandrud
# 29 May 2013
############### 

# Load packages
library(changepoint)
library(digest)
library(devtools)

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data")

MainMonth <- read.csv("MainMonth.csv")
MainQt <- read.csv("MainQuarter.csv")

# Drop if no hearings
MainMonth <- MainMonth[-1:-2, ]
MainMonth <- MainMonth[-193:-197, ]


#### -------------- Change Point Analysis Monthly ----------------- ####

tsMonth <- function(x){
	ts(x, start = c(1997, 3), frequency = 12)
}

tsQt <- function(x){
	ts(x, start = c(1997, 2), frequency = 4)
}

## Testimony Month Count
TestPerMonthTS <- tsMonth(MainMonth$TestCountMonthNoF)
ChangeCount <- cpt.meanvar(TestPerMonthTS, method = "BinSeg")
plot(ChangeCount, ylab = "Number of Hearings per Month", xlab = "")

## Members Present Median
PresTS <- tsMonth(MainMonth$MembPresMeanNoF)
ChangePres <- cpt.mean(PresTS, method = "BinSeg")
plot(ChangePres, ylab = "Monthly Mean Attendance", xlab = "")

## Laughter Median
LaughTS <- tsMonth(MainMonth$LaughMeanNoF)
ChangeLaugh <- cpt.meanvar(LaughTS)
plot(ChangeLaugh, ylab = "Monthly Mean Laughter", xlab = "")


#### Median Members Present/Number of Hearings
attach(MainMonth)
	MembersHearings <- MembPresMedianNoF/TestCountMonth
detach(MainMonth)

MembersHearings[is.nan(MembersHearings)] <- 0

MembersTS <- ts(MembersHearings, start = 1997)

ChangePresTest <- cpt.mean(MembersHearings, method = "BinSeg")
plot(ChangePresTest)

#### Monthly Econ Data ####
UnRateTS <- tsMonth(MainMonth$U6RATE)
UnRateChange <- cpt.mean(UnRateTS)
plot(UnRateChange, ylab = "Monthly Unemployment Rate", xlab = "")

#### ---------------- Change Point Analysis Quarterly -------------- ####
# Testimony Quarterly Count
TestPerQuarterTS <- tsQt(MainQt[, 5])
ChangeCountQt <- cpt.meanvar(TestPerQuarterTS, method = "BinSeg")
plot(ChangeCountQt, ylab = "Number of Hearings per Quarter", xlab = "")


# Members Present Quarterly
#PresQuarter$AverageMedianPres[is.nan(PresQuarter$AverageMedianPres)] <- 0

PresPerQuarterTS <- tsQt(MainQt[, 3])
ChangePresQt <- cpt.meanvar(PresPerQuarterTS, method = "BinSeg")
plot(ChangePresQt, ylab = "Quarterly Median Attendance", xlab = "")

# Members Present/Hearing Count Quarterly
MainQt$PresCount <- MainQt$MembPresMedian/MainQt$TestCountQuarter
PresCountTS <- tsQt(MainQt[, 6])
ChangePresTestQt <- cpt.meanvar(PresCountTS, method = "BinSeg")
plot(ChangePresTestQt, ylab = "Quarterly Median Attendance/No. of Hearings", xlab = "")

# Laughter Quarterly
LaughPerQuarterTS <- tsQt(MainQt[, 4])
ChangeLaughQt <- cpt.meanvar(LaughPerQuarterTS, method = "BinSeg")
plot(ChangeLaughQt, ylab = "Quarterly Median Laughter", xlab = "")

# Field Hearings Quarterly 
FieldQt <- tsQt(MainQt$FieldCountQuarter)
ChangeQuarterQt <- cpt.meanvar(FieldQt, method = "BinSeg")
plot(ChangeQuarterQt, ylab = "Quarterly Field Hearings Count", xlab = "")
