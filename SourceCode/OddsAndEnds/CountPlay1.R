################
# Create a graph of testimony counts.
# Christopher Gandrud
# 3 May 2013
################

# Load libraries
library(lubridate)
library(devtools)
library(digest)
library(DataCombine)
library(plyr)
library(ggplot2)

# Load quarter_year function
source_gist("5500733")

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/")

# Import testimony data
TestimonyData <- read.csv("TestimonyCount.csv")
TestimonyData$full_date <- as.character(TestimonyData$full_date)

# Declare full_data varaiable to be a dat variable
TestimonyData$Date <- dmy(TestimonyData$full_date)

TestimonyData <- MoveFront(TestimonyData, "Date")

# Month-Year (rounded to closest month) Variable
TestimonyData$MonthYear <- floor_date(TestimonyData$Date, "month")
TestimonyData$Quarter <- quarter_year(TestimonyData$Date, with_year = TRUE)

TestimonyData <- MoveFront(TestimonyData, "Quarter")

TestimonyData <- TestimonyData[order(TestimonyData$Quarter), ]

#### Make testimony per month graph ####
# Create month sums
TestimonyData$Any <- 1
TestimonyData <- ddply(TestimonyData, .(Quarter), transform, QuarterTotal = sum(Any)) 
TestUnique <- TestimonyData[!duplicated(TestimonyData[, 1]), ]

pdf(file = "~/Dropbox/Fed_Speeches_Paper/figures/TestimonyMonthCound.pdf")
ggplot(TestUnique, aes(x = Quarter, y = QuarterTotal)) +
         geom_line() +
         xlab("") + ylab("Monthly Testimony Count \n") +
         theme_bw(base_size = 15)
dev.off()

ggplot(TestimonyData, aes(MonthTotal)) + geom_density() + theme_bw()

