

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
TestUnique <- TestimonyData[!duplicated(TestimonyData[, 1]), ]

pdf(file = "~/Dropbox/Fed_Speeches_Paper/figures/TestimonyMonthCound.pdf")
ggplot(TestUnique, aes(x = MonthYear, y = MonthTotal)) +
         geom_line() +
         xlab("") + ylab("Monthly Testimony Count \n") +
         theme_bw(base_size = 15)
dev.off()

ggplot(TestimonyData, aes(MonthTotal)) + geom_density() + theme_bw()

