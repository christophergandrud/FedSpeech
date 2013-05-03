################
# Merge Testimony and Base Speech Count Data
# Christopher Gandrud
# 3 May 2013
################

# Load libraries
library(lubridate)
library(DataCombine)
library(plyr)
library(devtools)

# Load quarter_year function
source_gist("5500733")

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/")

#### ---------------Testimony------------------ #######
# Import testimony data
TestimonyData <- read.csv("TestimonyCount.csv")
TestimonyData$full_date <- as.character(TestimonyData$full_date)

# Declare full_data varaiable to be a dat variable
TestimonyData$Date <- dmy(TestimonyData$full_date)

TestimonyData <- MoveFront(TestimonyData, "Date")

TestimonyData <- TestimonyData[order(TestimonyData$Date), ]

#### Month-Year (rounded to closest month) Variable
TestimonyMonth <- TestimonyData
TestimonyMonth$MonthYear <- floor_date(TestimonyMonth$Date, "month")
TestimonyMonth <- MoveFront(TestimonyMonth, "MonthYear")

#### Make testimony per month Data ####
# Create month sums
TestimonyMonth$Any <- 1
TestimonyMonth <- ddply(TestimonyMonth, .(MonthYear), transform, MonthTestTotal = sum(Any)) 
TestUnique <- TestimonyMonth[!duplicated(TestimonyMonth[, 1]), ]

# Keep MonthYear & MonthTotal
TestimonyMonthTotals <- TestUnique[, c("MonthYear", "MonthTestTotal")]

#### Make testimony per quarter Data ####
#### Quarter
TestimonyQuarter <- TestimonyData
TestimonyQuarter$QuarterYear <- quarter_year(TestimonyQuarter$Date, with_year = TRUE)
TestimonyQuarter <- MoveFront(TestimonyQuarter, "QuarterYear")

TestimonyQuarter$Any <- 1
TestimonyQuarter <- ddply(TestimonyQuarter, .(QuarterYear), transform, QuarterTestTotal = sum(Any)) 
TestUnique <- TestimonyQuarter[!duplicated(TestimonyQuarter[, 1]), ]

# Keep QuarterYear & QuarterTotal
TestimonyQuarterTotals <- TestUnique[, c("QuarterYear", "QuarterTestTotal")]

#### ---------------------- Speech Orgs ------------------- ####


