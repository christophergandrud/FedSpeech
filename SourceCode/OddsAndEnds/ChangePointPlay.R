###############
# Changepoint Play
# Christopher Gandrud
# 27 May 2013
############### 

library(changepoint)

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data")

Main <- read.csv("Main.csv")


#### -------------- Change Point Analysis ----------------- ####

## Testimony Month Count
ChangeCount <- cpt.meanvar(Combined$TestCountMonth, method = "BinSeg")
plot(ChangeCount, ylab = "Number of Hearings per Month")


## Laughter Median
ChangeLaugh <- cpt.meanvar(Combined$LaughMedian)
plot(ChangeLaugh, ylab = "Monthly Median Laughter")

## Members Present Median
ChangePres <- cpt.mean(Combined$MembPresMedian, method = "BinSeg")
plot(ChangePres, ylab = "Monthly Median Attendance")
