

# Load libraries
library(lubridate)
library(DataCombine)
library(plyr)
library(ggplot2)

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/")

# Import testimony data
TestimonyData <- read.csv("TestimonyCount.csv")

# Declare full_data varaiable to be a dat variable
TestimonyData$Date <- dmy(TestimonyData$full_date)

TestimonyData <- MoveFront(TestimonyData, "Date")

# Month-Year (rounded to closest month) Variable
TestimonyData$MonthYear <- round_date(TestimonyData$Date, "month")

TestimonyData <- MoveFront(TestimonyData, "MonthYear")

TestimonyData <- TestimonyData[order(TestimonyData$Date), ]

#### Make testimony per month graph ####
# Create month sums
TestimonyData$Any <- 1
TestimonyData <- ddply(TestimonyData, .(MonthYear), transform, MonthTotal = sum(Any)) 

ggplot(TestimonyData, aes(x = MonthYear, y = MonthTotal)) +
         geom_line() +
         xlab("") + ylab("Monthly Testimony Totals \n") +
         theme_bw()
 