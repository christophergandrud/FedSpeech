###############
# Clean Congressional Testimony Laughter Data
# Christopher Gandrud
# 27 May 2013
############### 

library(lubridate)
library(DataCombine)
library(plyr)

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data")

# Import data 
LData <- read.csv("Raw/MonetaryPolicyChron.csv")

#### Clean
LDataSub <- LData[, c("Date", "Laughter.Count", "Members.Present", 
                      "Fed.SemiAnnual.Oversight.Hearing")]
names(LDataSub) <- c("Date", "LaughCount", "MembersPres", "Oversight")

# Convert NA to 0 for Oversight
LDataSub$Oversight[is.na(LDataSub$Oversight)] <- 0

# Clean date 
LDataSub$DateStandard <- dmy(as.character(LDataSub$Date))
LDataSub <- LDataSub[order(LDataSub[, 5]), ]

LDataSub$LaughCount <- as.numeric(LDataSub$LaughCount)
LDataSub$MembersPres <- gsub("[a-zA-Z]", NA, LDataSub$MembersPres)
LDataSub$MembersPres <- as.numeric(LDataSub$MembersPres)

LDataSub <- DropNA(LDataSub, "MembersPres")
LDataSub <- DropNA(LDataSub, "LaughCount")


#### Per Month ####
# Median members per month
library(plyr)

# Create MonthYear variable
LD$MonthYear <- floor_date(LD$DateStandard, "month")

LD <- ddply(LD, .(MonthYear), transform, MembPresMedian = median(MembersPres)) 
LD <- ddply(LD, .(MonthYear), transform, LaughMedian = median(LaughCount)) 

LD$Dummy <- 1
LD <- ddply(LD, .(MonthYear), transform, TestCountMonth = sum(Dummy)) 

# Create Month Only data
LDMonth <- LD[!duplicated(LD[, "MonthYear"]), ]

# Clean up
LDMonth <- LDMonth[, c("MonthYear", "MembPresMedian", 
			"LaughMedian", "TestCountMonth")]

#### Merge with economic data ####
EconData <- read.csv("FREDEconData.csv")

# Clean
EconData <- EconData[, -1]
EconData <- rename(EconData, c("DateField" = "MonthYear"))
EconData$MonthYear <- ymd(as.character(EconData$MonthYear))
EconData <- EconData[year(EconData$MonthYear) >= 1997,]

# Merge
Combined <- merge(LDMonth, EconData, by = "MonthYear", all = TRUE)

# Clean combined 
Combined$TestCountMonth[is.na(Combined$TestCountMonth)] <- 0
Combined$LaughMedian[is.na(Combined$LaughMedian)] <- 0
Combined$MembPresMedian[is.na(Combined$MembPresMedian)] <- 0

#### ---- Save ---- ####
write.csv(Combined, file = "Main.csv")