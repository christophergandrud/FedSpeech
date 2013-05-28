#################
# Merge Testimony and Base Speech Count Data
# Christopher Gandrud
# 16 May 2013
################

# Load libraries
library(lubridate)
library(plyr)
library(devtools)
library(DataCombine)
library(reshape2)
library(digest)

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

write.csv(TestimonyMonthTotals, file = "TestimonyPerMonth.csv")





#### Make testimony per quarter Data ####
#### Quarter
TestimonyQuarter <- TestimonyData
TestimonyQuarter$QuarterYear <- quarter_year(TestimonyQuarter$Date, with_year = TRUE)

TestimonyQuarter <- MoveFront(TestimonyQuarter, "QuarterYear")

TestimonyQuarter$Any <- 1
TestimonyQuarter <- ddply(TestimonyQuarter, 
						.(QuarterYear), 
						transform, QuarterTestTotal = sum(Any)) 
TestUnique <- TestimonyQuarter[!duplicated(TestimonyQuarter[, 1]), ]

# Keep QuarterYear & QuarterTotal
TestimonyQuarterTotals <- TestUnique[, c("QuarterYear", "QuarterTestTotal")]








#### ---------------------- Speech Orgs ------------------- ####
# Load data
OrgData <- read.csv("BaseSpeechCount.csv")
OrgData$full_date <- as.character(OrgData$full_date)

# Find month and quarter variables
OrgData$Date <- dmy(OrgData$full_date) 

OrgData$MonthYear <- floor_date(OrgData$Date, "month")
OrgData$QuarterYear <- quarter_year(OrgData$Date, with_year = TRUE)

#### Find Monthly Percentages ####
# Monthly totals
OrgData <- ddply(OrgData, 
					.(MonthYear), 
					transform, SpeechMonth = sum(any_speech))

# bankersfinance total
#### Find Monthly Percentages ####
# Monthly totals
OrgData <- ddply(OrgData, 
					.(MonthYear), 
					transform, BF_Month = sum(bankersfinance))

# Percentages data
OrgData$BF_PerMonth <- OrgData$BF_Month/OrgData$SpeechMonth

BF_MonthUnique <- OrgData[!duplicated(OrgData[, "MonthYear"]), ]
BF_MonthUnique <- BF_MonthUnique[, c("MonthYear", "BF_PerMonth")]

# community_organisations total
#### Find Monthly Percentages ####
# Monthly totals
OrgData <- ddply(OrgData, 
					.(MonthYear), 
					transform, CO_Month = sum(community_organisations))

# Percentages data
OrgData$CO_PerMonth <- OrgData$CO_Month/OrgData$SpeechMonth

CO_MonthUnique <- OrgData[!duplicated(OrgData[, "MonthYear"]), ]
CO_MonthUnique <- CO_MonthUnique[, c("MonthYear", "CO_PerMonth")]

#### Combine Monthly OrgPercents
OrgPercents <- merge(BF_MonthUnique, CO_MonthUnique, by = "MonthYear")


#### Merge with testimony month counts
Monthly <- merge(TestimonyMonthTotals, OrgPercents, 
				 by = "MonthYear")

#### Find Quarterly Percentages ####
# Quarterly totals
OrgData <- ddply(OrgData, 
					.(QuarterYear), 
					transform, SpeechQuarter = sum(any_speech))

# bankersfinance total
#### Find Quarterly Percentages ####
# Quarterly totals
OrgData <- ddply(OrgData, 
					.(QuarterYear), 
					transform, BF_Quarter = sum(bankersfinance))

# Percentages data
OrgData$BF_PerQuarter <- OrgData$BF_Quarter/OrgData$SpeechQuarter

BF_QuarterUnique <- OrgData[!duplicated(OrgData[, "QuarterYear"]), ]
BF_QuarterUnique <- BF_QuarterUnique[, c("QuarterYear", "BF_PerQuarter", "BF_Quarter")]

# community_organisations total
#### Find Quarterly Percentages ####
# Quarterly totals
OrgData <- ddply(OrgData, 
					.(QuarterYear), 
					transform, CO_Quarter = sum(community_organisations))

# Percentages data.
OrgData$CO_PerQuarter <- OrgData$CO_Quarter/OrgData$SpeechQuarter

CO_QuarterUnique <- OrgData[!duplicated(OrgData[, "QuarterYear"]), ]
CO_QuarterUnique <- CO_QuarterUnique[, c("QuarterYear", "CO_PerQuarter", "CO_Quarter")]

#### Combine Quarterly OrgPercents
OrgPercents <- merge(BF_QuarterUnique, CO_QuarterUnique, by = "QuarterYear")

#### Create BF as a ratio of CO
OrgPercents$CO_BF <- OrgPercents$CO_Quarter/OrgPercents$BF_Quarter


#### Merge with testimony month counts
Quarterly <- merge(TestimonyQuarterTotals, OrgPercents, 
				 by = "QuarterYear")

require(ggplot2)
ggplot(Quarterly, aes(QuarterTestTotal, BF_PerQuarter)) + geom_point() + theme_bw()

