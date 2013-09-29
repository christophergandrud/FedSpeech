#############
# Prepare speeches for text analysis
# Christopher Gandrud
# 29 September 2013
#############

library(ExpAgenda)
library(stringr)

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
TestEA <- ExpAgendaVonmon(obj = DocTerm)

