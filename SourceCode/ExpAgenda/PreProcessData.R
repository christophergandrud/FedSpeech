#############
# Prepare speeches for text analysis
# Christopher Gandrud
# 30 September 2013
#############

library(ExpAgenda)
library(stringr)
library(lubridate)
library(ggplot2)

RemoveTitle <- function(data){
  data[, "name"] <- gsub(pattern = "Governor†", "", data[, "name"])
  data[, "name"] <- gsub(pattern = "Governor", "", data[, "name"])
  data[, "name"] <- gsub(pattern = "Chairman†", "", data[, "name"])
  data[, "name"] <- gsub(pattern = "Chairman", "", data[, "name"])
  data[, "name"] <- gsub(pattern = "Vice Chair†", "", data[, "name"])
  data[, "name"] <- gsub(pattern = "Vice Chair", "", data[, "name"])
  data[, "name"] <- str_trim(data[, "name"])
  data
}

setwd("~/Dropbox/Fed_Speeches_Data/May2013Speeches/FedSpeechIndvParsed_20May/")

MetaData <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/FedSpeechesVersionMay2013.csv",
                     stringsAsFactors = FALSE)

MetaData <- RemoveTitle(MetaData)

DocTerm <- PreProcess(textsPattern = "*.txt", authorsDF = MetaData,
                      AuthorCol = "name", removeAuthors = "Brian F. Madigan", 
                      sparse = 0.8)

 ## Run text analysis
Est1 <- ExpAgendaVonmon(obj = DocTerm, n.cats = 6)

## Find main stems associated with each topic and main topic of each speech
TopicsStems <- TopicSummary(Est1, NStems = 5)

Labels <- c("Topic1", "Topic2", "Topic3", "Topic4")
TopicDoc <- DocTopics(Est1)

## Graph by speaker over time
MetaDataSub <- subset(MetaData, name != "Brian F. Madigan")
TopicDocComb <- cbind(MetaDataSub[, "full_date"], TopicDoc[,2:3])
TopicDocComb$year <- year(dmy(TopicDocComb[,1]))

# Create counts
Years <- unique(TopicDocComb$year)
Speakers <- unique(TopicDocComb$Names)
OutDF <- data.frame()
for (i in Years){
  YearSub <- subset(TopicDocComb, year == i)
  for (u in Speakers){
    SpeakSub <- subset(YearSub, Names == u)
    temp <- data.frame(table(SpeakSub$Topic))
    temp$Year <- i
    temp$Names <- u
    OutDF <- rbind(OutDF, temp)
  }
}

OutDF <- subset(OutDF, Freq != 0)

ggplot(OutDF, aes(Year, Freq, colour = Names)) +
  geom_line() +
  facet_grid(Var1~.) +
  theme_bw()
