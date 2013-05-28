###############
# Changepoint Play
# Christopher Gandrud
# 28 May 2013
############### 

# Load packages
library(changepoint)
library(digest)
library(devtools)

# Load quarter_year and quarter_sum functions
source_gist("5500733")
source_gist("5660246")

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data")

Main <- read.csv("Main.csv")

# Drop if no hearings
Main <- Main[-1:-2, ]
Main <- Main[-193:-197, ]


#### -------------- Change Point Analysis Monthly ----------------- ####

## Testimony Month Count
TestPerMonthTS <- ts(Main$TestCountMonth, start = c(1997, 3), frequency = 12)
ChangeCount <- cpt.meanvar(TestPerMonthTS, method = "BinSeg")
plot(ChangeCount, ylab = "Number of Hearings per Month", xlab = "")

## Members Present Median
PresTS <- ts(Main$MembPresMedian, start = c(1997, 3), frequency = 12)
ChangePres <- cpt.mean(PresTS, method = "BinSeg")
plot(ChangePres, ylab = "Monthly Median Attendance", xlab = "")

## Laughter Median
LaughTS <- ts(Main$LaughMedian, start = c(1997, 3), frequency = 12)
ChangeLaugh <- cpt.meanvar(LaughTS)
plot(ChangeLaugh, ylab = "Monthly Median Laughter", xlab = "")


#### Median Members Present/Number of Hearings
attach(Main)
	MembersHearings <- MembPresMedian/TestCountMonth
detach(Main)

MembersHearings[is.nan(MembersHearings)] <- 0

MembersTS <- ts(MembersHearings, start = 1997)

ChangePresTest <- cpt.mean(MembersHearings, method = "BinSeg")
plot(ChangePresTest)

#### ---------------- Change Point Analysis Quarterly -------------- ####
# Drop non-fully observed quarters
MainQt <- Main[c(-1, -191, -192), ]

# Testimony Quarterly Count
TestQuarter <- quarter_sum(data = MainQt, Var = "TestCountMonth",
							TimeVar = "MonthYear")
TestPerQuarterTS <- ts(TestQuarter[, 2], start = c(1997, 2), frequency = 4)
ChangeCountQt <- cpt.meanvar(TestPerQuarterTS, method = "BinSeg")
plot(ChangeCountQt, ylab = "Number of Hearings per Quarter", xlab = "")


# Members Present Quarterly
PresQuarter <- quarter_sum(data = MainQt, Var = "MembPresMedian",
							TimeVar = "MonthYear", NewVar = "PresSum")
PresQuarter <- merge(TestQuarter, PresQuarter, by = "Quarter")
PresQuarter$AverageMedianPres <- PresQuarter[, 3]/PresQuarter[, 2]
PresQuarter$AverageMedianPres[is.nan(PresQuarter$AverageMedianPres)] <- 0

PresPerQuarterTS <- ts(PresQuarter[, 4], start = c(1997, 2), frequency = 4)
ChangeCountQt <- cpt.meanvar(PresPerQuarterTS, method = "BinSeg")
plot(ChangeCountQt, ylab = "Monthly Median Attendance Averaged by Quarterly Hearing Count", xlab = "")

# Laughter Quarterly
LaughterQuarter <- quarter_sum(data = MainQt, Var = "LaughMedian",
							TimeVar = "MonthYear")
LaughPerQuarterTS <- ts(LaughQuarter[, 2], start = c(1997, 2), frequency = 4)
ChangeCountQt <- cpt.meanvar(LaughPerQuarterTS, method = "BinSeg")
plot(ChangeCountQt, ylab = "Monthly Sum of the Median Members Present per Quarter", xlab = "")

