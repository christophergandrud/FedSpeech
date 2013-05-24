###############
# Clean Congressional Testimony Laughter Data
# Christopher Gandrud
# 24 May 2013
############### 

library(lubridate)
library(DataCombine)

# Import data 
LData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/MonetaryPolicyChron.csv")

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


LD <- LDataSub

#### Graphing Fun ####
# Graph median members per month
library(plyr)

# Create MonthYear variable
LD$MonthYear <- floor_date(LD$DateStandard, "month")

LD <- ddply(LD, .(MonthYear), transform, MembPresMedian = median(MembersPres)) 
LD <- ddply(LD, .(MonthYear), transform, LaughMedian = median(LaughCount)) 

# Create Month Only data
LDMonth <- LD[!duplicated(LD[, "MonthYear"]), ]


#### Change Point Analysis ####
library(changepoint)
ChangeLaugth <- cpt.meanvar(LDMonth$LaughMedian)
plot(ChangeLaugth, ylab = "Monthly Median Laughter")

ChangePres <- cpt.mean(LDMonth$MembPresMedian, method = "BinSeg")
plot(ChangePres, ylab = "Monthly Median Attendance")

library(ggplot2)
ggplot(LD, aes(DateStandard, LaughMedian)) + 
  geom_point() + 
  stat_smooth() +
  xlab("") +
  theme_bw()

ggplot(LD, aes(MembPresMedian, LaughMedian)) + 
  geom_point() + 
  stat_smooth() +
  xlab("") +
  theme_bw()