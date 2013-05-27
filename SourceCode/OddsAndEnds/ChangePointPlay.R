###############
# Changepoint Play
# Christopher Gandrud
# 27 May 2013
############### 

library(changepoint)

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data")

Main <- read.csv("Main.csv")

# Drop if no hearings
Main <- Main[-1:-2, ]
Main <- Main[-193:-197, ]


#### -------------- Change Point Analysis ----------------- ####

## Testimony Month Count
TestPerMonthTS <- ts(Main$TestCountMonth, start = c(1997, 3), frequency = 12)
ChangeCount <- cpt.meanvar(TestPerMonth, method = "BinSeg")
plot(ChangeCount, ylab = "Number of Hearings per Month")

## Members Present Median
PresTS <- ts(Main$MembPresMedian, start = c(1997, 3), frequency = 12)
ChangePres <- cpt.mean(PresTS, method = "BinSeg")
plot(ChangePres, ylab = "Monthly Median Attendance")

## Laughter Median
LaughTS <- ts(Main$LaughMedian, start = c(1997, 3), frequency = 12)
ChangeLaugh <- cpt.meanvar(Main$LaughMedian)
plot(ChangeLaugh, ylab = "Monthly Median Laughter")


#### Median Members Present/Number of Hearings
attach(Main)
	MembersHearings <- MembPresMedian/TestCountMonth
detach(Main)

MembersHearings[is.nan(MembersHearings)] <- 0

MembersTS <- ts(MembersHearings, start = 1997)

ChangePresTest <- cpt.mean(MembersHearings, method = "BinSeg")
plot(ChangePresTest)