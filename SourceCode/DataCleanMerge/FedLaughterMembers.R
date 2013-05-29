###############
# Clean Congressional Testimony Laughter Data
# Christopher Gandrud
# 29 May 2013
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
LDM <- ddply(LDM, .(MonthYear), transform, TestCountMonth = sum(Dummy)) 

# Create Month Only data
LDMonth <- LDM[!duplicated(LDM[, "MonthYear"]), ]

# Clean up
LDMonth <- LDMonth[, c("MonthYear", "MembPresMedian", 
			"LaughMedian", "MembPresMean", 
			"LaughMean", "TestCountMonth")]

#### Excluding Field hearings ####
LDNoF <- subset(LDM, Field == 0)

# Median
LDNoF <- ddply(LDNoF, .(MonthYear), transform, MembPresMedianNoF = median(MembersPres)) 
LDNoF <- ddply(LDNoF, .(MonthYear), transform, LaughMedianNoF = median(LaughCount)) 

# Mean
LDNoF <- ddply(LDNoF, .(MonthYear), transform, MembPresMeanNoF = mean(MembersPres)) 
LDNoF <- ddply(LDNoF, .(MonthYear), transform, LaughMeanNoF = mean(LaughCount)) 

LDNoF$Dummy <- 1
LDNoF <- ddply(LDNoF, .(MonthYear), transform, TestCountMonthNoF = sum(Dummy)) 

# Create Month Only data
LDNoF <- LDNoF[!duplicated(LDNoF[, "MonthYear"]), ]

# Clean up
LDNoF <- LDNoF[, c("MonthYear", "MembPresMedianNoF", 
			"LaughMedianNoF", "MembPresMeanNoF", 
			"LaughMeanNoF", "TestCountMonthNoF")]

# Merge no Field with full Field
LDMonth <- merge(LDMonth, LDNoF, by = "MonthYear")

#### ------------ Per Quarter ---------------------------------- ####
# Drop non-fully observed quarters
LDQt <- LDataSub[c(-1, -2), ]

# Create Quarter variable 
LDQt$Quarter <- quarter_year(LDQt[, "DateStandard"], with_year = TRUE)

# Median
LDQt <- ddply(LDQt, .(Quarter), transform, MembPresMedian = median(MembersPres)) 
LDQt <- ddply(LDQt, .(Quarter), transform, LaughMedian = median(LaughCount)) 

LDQt$Dummy <- 1
LDQt <- ddply(LDQt, .(Quarter), transform, TestCountQuarter = sum(Dummy)) 

LDQt <- ddply(LDQt, .(Quarter), transform, FieldCountQuarter = sum(Field))

# Create Month Only data
LDQt <- LDQt[!duplicated(LDQt[, "Quarter"]), ]

# Clean up
LDQt <- LDQt[, c("Quarter", "MembPresMedian", 
                  "LaughMedian", "TestCountQuarter", "FieldCountQuarter")]

#### ------------ Merge Month Data with economic data --------------------- ####
EconData <- read.csv("FREDEconData.csv")

# Clean
EconData <- EconData[, -1]
EconData <- rename(EconData, c("DateField" = "MonthYear"))
EconData$MonthYear <- ymd(as.character(EconData$MonthYear))
EconData <- EconData[year(EconData$MonthYear) >= 1997,]

# Merge
CombinedMonth <- merge(LDMonth, EconData, by = "MonthYear", all = TRUE)

# Clean combined 
CombinedMonth$TestCountMonth[is.na(CombinedMonth$TestCountMonth)] <- 0
CombinedMonth$LaughMedian[is.na(CombinedMonth$LaughMedian)] <- 0
CombinedMonth$MembPresMedian[is.na(CombinedMonth$MembPresMedian)] <- 0
CombinedMonth$LaughMean[is.na(CombinedMonth$LaughMean)] <- 0
CombinedMonth$MembPresMean[is.na(CombinedMonth$MembPresMean)] <- 0
CombinedMonth$TestCountMonthNoF[is.na(CombinedMonth$TestCountMonthNoF)] <- 0
CombinedMonth$LaughMedianNoF[is.na(CombinedMonth$LaughMedianNoF)] <- 0
CombinedMonth$MembPresMedianNoF[is.na(CombinedMonth$MembPresMedianNoF)] <- 0
CombinedMonth$LaughMeanNoF[is.na(CombinedMonth$LaughMeanNoF)] <- 0
CombinedMonth$MembPresMeanNoF[is.na(CombinedMonth$MembPresMeanNoF)] <- 0

#### ---- Save ---- ####
write.csv(CombinedMonth, file = "MainMonth.csv")
write.csv(LDQt, file = "MainQuarter.csv")