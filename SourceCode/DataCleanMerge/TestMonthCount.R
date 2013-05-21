################
# Testimony Sum by Month
# Christopher Gandrud
# 21 May 2013
################

# Load libraries
library(lubridate)
library(plyr)
library(DataCombine)

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/")

#### ---------------Testimony------------------ #######
# Import testimony data
TestimonyData <- read.csv("TestimonyCount.csv")
TestimonyData$full_date <- as.character(TestimonyData$full_date)

# Declare full_data varaiable to be a dat variable
TestimonyData$Date <- dmy(TestimonyData$full_date)

TestimonyData <- TestimonyData[order(TestimonyData$Date), ]

#### Month-Year (rounded to closest month) Variable
TestimonyMonth <- TestimonyData
TestimonyMonth$MonthYear <- floor_date(TestimonyMonth$Date, "month")
TestimonyMonth <- MoveFront(TestimonyMonth, "MonthYear")

#### Make testimony per month Data ####
# Create month sums
TestimonyMonth$Any <- 1
TestimonyMonth <- ddply(TestimonyMonth, 
						.(MonthYear), 
						transform, MonthTestTotal = sum(Any)) 
TestUnique <- TestimonyMonth[!duplicated(TestimonyMonth[, 1]), ]

# Keep MonthYear & MonthTotal
TestimonyMonthTotals <- TestUnique[, c("MonthYear", "MonthTestTotal")]

# Save
write.csv(TestimonyMonthTotals, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/TestimonyPerMonth.csv")