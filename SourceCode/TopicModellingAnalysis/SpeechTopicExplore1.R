##############
# Fed Speeches Topics Change Point Explore
# Christopher Gandrud
# 6 August 2013
##############

# Depends on data created by SpeechesTopicClean.R
source("~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/DataCleanMerge/SpeechesTopicClean.R")

# Load e.divGG function
library(devtools)
source_gist("5675688")

LongDataMake <- function(obj, TopicLabels){
  #### Convert to format for change point analysis
  TopCols <- c("top1", "top2", "top3", "top4", "top5")
  TopProp <- c("top1Prop", "top2Prop", "top3Prop", "top4Prop", "top5Prop")

  TempDFi <- data.frame()
  for (i in TopCols){
      Temp <- obj[, c("MonthYear", "QuarterYear", "SpeechID", i)]
      names(Temp) <- c("MonthYear", "QuarterYear", "SpeechID", "Topic")
      TempDFi <- rbind(TempDFi, Temp)
  }

  TempDFu <- data.frame()
  for (u in TopProp){
    Temp <- data.frame(obj[, c(u)])
    names(Temp) <- c("TopicProp")
    TempDFu <- rbind(TempDFu, Temp)
  }

  LongTopic <- cbind(TempDFi, TempDFu)

  TopicLabels <- c("Financial.Markets", "Macroeconomics", "Monetary.Policy", 
                   "International.Economy",  "Local.Housing.Dev", 
                   "Banking.Regulation") 

  TopicWide <- LongTopic[, 1:3]
  for (v in TopicLabels){
    Temp <- subset(LongTopic, Topic == v)
    Temp <- Temp[, -4]
    names(Temp) <- c("MonthYear", "QuarterYear", "SpeechID", v)
    TopicWide <- merge(TopicWide, Temp, 
                       by = c("MonthYear", "QuarterYear", "SpeechID"), all = TRUE )
  }

  TopicWide <- TopicWide[!duplicated(TopicWide[, 1:3]), ]

  ## Code NA as 0
  TopicWide[is.na(TopicWide)] <- 0
  TopicWide
}

TopicLabels5 <- c("Financial.Markets", "Macroeconomics", "Monetary.Policy", 
                 "International.Economy",  "Local.Housing.Dev", 
                 "Banking.Regulation") 

TopicLabels10 <- c("Local.Housing.Dev", "Financial.Markets", "Monetary.Policy", 
                 "Risk.Regulation", "Housing", "International.Economy", "Banking",
                 "Prices", "Technology", "Macroeconomics")  

TopicWide5 <- LongDataMake(obj = CombClean5, TopicLabels = TopicLabels5)
TopicWide10 <- LongDataMake(obj = CombClean10, TopicLabels = TopicLabels10)

write.csv(TopicWide5, "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/TopicsSpoken.csv", 
          row.names = FALSE)

###### ------- Preliminary Change Point ---------- ####

#### 5
CleanNames5 <- c("Financial Markets", "Macroeconomics", "Monetary Policy", 
                "International Economy",  "Local Housing and Dev", 
                "Banking Regulation")

e.divGG(data = TopicWide5, Vars = TopicLabels5, TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 999, min.size = 6, Titles = CleanNames5, 
        JustGraph = TRUE)

#### 10 
# TopicLabels10 <- c("Financial.Markets", "Macroeconomics", "Monetary.Policy",      
#                   "International.Economy", "Local.Housing.Dev",
#                   "Banking.Regulation")  

# CleanNames10 <- c("Financial Markets", "Macroeconomics", "Monetary Policy",      
#                  "International Economy", "Local Housing Dev",
#                  "Banking Regulation")  

# e.divGG(data = TopicWide10, Vars = TopicLabels10, TimeVar = "MonthYear", 
#        sig.lvl = 0.05, R = 999, min.size = 6, Titles = CleanNames10)