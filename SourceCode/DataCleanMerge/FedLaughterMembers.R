###############
# Clean Congressional Testimony Laughter Data
# Christopher Gandrud
# 31 May 2013
############### 

library(lubridate)
library(DataCombine)
library(plyr)
library(digest)
library(devtools)

# Load quarter_year
source_gist("5500733")

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data")

# Import data 
LData <- read.csv("Raw/MonetaryPolicyChron.csv")

#### --------- Clean ----------------------------------------- #### 
LDataSub <- LData[, c("Date", "Laughter.Count", "Members.Present", 
                      "Field.hearing", "Fed.SemiAnnual.Oversight.Hearing")]
names(LDataSub) <- c("Date", "LaughCount", "MembersPres", "Field", "Oversight")

# Convert NA to 0 for Oversight/Field
LDataSub$Oversight[is.na(LDataSub$Oversight)] <- 0
LDataSub$Field[is.na(LDataSub$Field)] <- 0

# Clean date 
LDataSub$DateStandard <- dmy(as.character(LDataSub$Date))
LDataSub <- LDataSub[order(LDataSub$DateStandard), ]

LDataSub$LaughCount <- as.numeric(LDataSub$LaughCount)
LDataSub$MembersPres <- gsub("[a-zA-Z]", NA, LDataSub$MembersPres)
LDataSub$MembersPres <- as.numeric(LDataSub$MembersPres)

LDataSub <- DropNA(LDataSub, "MembersPres")
LDataSub <- DropNA(LDataSub, "LaughCount")


#### ------------ Per Month ------------------------------------ ####
# Median members per month

LDM <- LDataSub

# Create MonthYear variable
LDM$MonthYear <- floor_date(LDM$DateStandard, "month")

# Medians
LDM <- ddply(LDM, .(MonthYear), transform, MembPresMedian = median(MembersPres)) 
LDM <- ddply(LDM, .(MonthYear), transform, LaughMedian = median(LaughCount)) 

# Means
LDM <- ddply(LDM, .(MonthYear), transform, MembPresMean = mean(MembersPres)) 
LDM <- ddply(LDM, .(MonthYear), transform, LaughMean = mean(LaughCount)) 

LDM$Dummy <- 1
LDM <- ddply(LDM, .(MonthYear), transform, HearingCountMonth = sum(Dummy)) 

# Create Month Only data
LDMonth <- LDM[!duplicated(LDM[, "MonthYear"]), ]

# Clean up
LDMonth <- LDMonth[, c("MonthYear", "MembPresMedian", 
			"LaughMedian", "MembPresMean", 
			"LaughMean", "HearingCountMonth")]

#### Excluding Field hearings ####
LDNoF <- subset(LDM, Field == 0)

# Median
LDNoF <- ddply(LDNoF, .(MonthYear), transform, MembPresMedianNoF = median(MembersPres)) 
LDNoF <- ddply(LDNoF, .(MonthYear), transform, LaughMedianNoF = median(LaughCount)) 

# Mean
LDNoF <- ddply(LDNoF, .(MonthYear), transform, MembPresMeanNoF = mean(MembersPres)) 
LDNoF <- ddply(LDNoF, .(MonthYear), transform, LaughMeanNoF = mean(LaughCount)) 

LDNoF$Dummy <- 1
LDNoF <- ddply(LDNoF, .(MonthYear), transform, HearingCountMonthNoF = sum(Dummy)) 

# Create Month Only data
LDNoF <- LDNoF[!duplicated(LDNoF[, "MonthYear"]), ]

# Clean up
LDNoF <- LDNoF[, c("MonthYear", "MembPresMedianNoF", 
			"LaughMedianNoF", "MembPresMeanNoF", 
			"LaughMeanNoF", "HearingCountMonthNoF")]

# Merge no Field with full Field
LDMonth <- merge(LDMonth, LDNoF, by = "MonthYear")

#### ------------ Merge with Testimony Count ------------- ####
TestCount <- read.csv("TestimonyPerMonth.csv", stringsAsFactors = FALSE) 
TestCount <- TestCount[, -1]
TestCount$MonthYear <- ymd(TestCount$MonthYear)

# Merge with Main Data
LDMonth <- merge(LDMonth, TestCount, by = "MonthYear", all = TRUE)

#### ------------ Per Quarter ---------------------------------- ####
# Drop non-fully observed quarters
LDQt <- LDataSub[c(-1, -2), ]

# Create Quarter variable 
LDQt$Quarter <- quarter_year(LDQt[, "DateStandard"], with_year = TRUE)

# Median
LDQt <- ddply(LDQt, .(Quarter), transform, MembPresMedian = median(MembersPres)) 
LDQt <- ddply(LDQt, .(Quarter), transform, LaughMedian = median(LaughCount)) 

LDQt$Dummy <- 1
LDQt <- ddply(LDQt, .(Quarter), transform, HearingCountQuarter = sum(Dummy)) 

LDQt <- ddply(LDQt, .(Quarter), transform, FieldCountQuarter = sum(Field))

# Create Month Only data
LDQt <- LDQt[!duplicated(LDQt[, "Quarter"]), ]

# Clean up
LDQt <- LDQt[, c("Quarter", "MembPresMedian", 
                  "LaughMedian", "HearingCountQuarter", "FieldCountQuarter")]

#### ------------ Merge Month Data with economic data --------------------- ####
EconData <- read.csv("FREDEconData.csv")

# Clean
EconData <- EconData[, -1]
EconData <- rename(EconData, c("DateField" = "MonthYear"))
EconData$MonthYear <- ymd(as.character(EconData$MonthYear))
EconData <- EconData[year(EconData$MonthYear) >= 1997,]

# Merge
CombinedMonth <- merge(LDMonth, EconData, by = "MonthYear", all = TRUE)

# Drop if outside of Full Hearing and Testimony data
CombinedMonth <- CombinedMonth[c(-1:-5, -196:-200), ]

# Clean combined 
attach(CombinedMonth)
CombinedMonth$HearingCountMonth[is.na(HearingCountMonth)] <- 0
CombinedMonth$LaughMedian[is.na(LaughMedian)] <- 0
CombinedMonth$MembPresMedian[is.na(MembPresMedian)] <- 0
CombinedMonth$LaughMean[is.na(LaughMean)] <- 0
CombinedMonth$MembPresMean[is.na(MembPresMean)] <- 0
CombinedMonth$HearingCountMonthNoF[is.na(HearingCountMonthNoF)] <- 0
CombinedMonth$LaughMedianNoF[is.na(LaughMedianNoF)] <- 0
CombinedMonth$MembPresMedianNoF[is.na(MembPresMedianNoF)] <- 0
CombinedMonth$LaughMeanNoF[is.na(LaughMeanNoF)] <- 0
CombinedMonth$MembPresMeanNoF[is.na(MembPresMeanNoF)] <- 0
CombinedMonth$MonthTestTotal[is.na(MonthTestTotal)] <- 0
detach(CombinedMonth)

#### ---- Save ---- ####
write.csv(CombinedMonth, file = "MainMonth.csv")
write.csv(LDQt, file = "MainQuarter.csv")