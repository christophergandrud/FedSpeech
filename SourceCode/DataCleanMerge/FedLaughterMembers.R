###############
# Clean Congressional Testimony Laughter Data
# Christopher Gandrud
# 22 May 2013
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


#### Graphing Fun ####
library(changepoint)
Test <- cpt.var(LDataSub$MembersPres, method = "PELT")
plot(Test, type = "l")

ggplot(LDataSub, aes(DateStandard, MembersPres)) + 
  geom_point() + 
  stat_smooth() +
  xlab("") +
  theme_bw()