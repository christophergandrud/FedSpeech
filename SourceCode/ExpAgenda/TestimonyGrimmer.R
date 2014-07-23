#############
# Prepare testimony for text analysis
# Christopher Gandrud
# 1 October 2013
#############

library(foreign)
library(ExpAgenda)
library(stringr)
library(lubridate)
library(plyr)
library(ggplot2)

setwd("~/Dropbox/Fed_Speeches_Data/fed.testimony.parsed/")

MetaData <- read.dta("~/Dropbox/Fed_Speeches_Data/fed_testimony.dta")

SingleDrop <- c("Board of s of the Federal Reserve System",	"David W. Wilcox","J. Nellie Liang","Janet L. Yellen","Jeffrey Marquardt","Kenneth Buckley","Kevin Warsh","Lawrence B. Lindsey","man, Donald L. Kohn","Mark E. Van Der Weide","Maryann F. Hunter","Michael R. Foley","Michael S. Gibson","Oliver Ireland","Patricia White","Patrick Parkinson","Roger W. Ferguson Jr.","Sandra Braunstein","Sarah Bloom Raskin","Statement for the Record","Stephen R. Malphrus","Suzanne G. Killian","Theodore E. Allison","William R. Nelson")

DocTerm <- PreProcess(textsPattern = "*.txt", authorsDF = MetaData,
                      AuthorCol = "name", sparse = 0.8, 
                      removeAuthors = SingleDrop)

## Run text analysis
Est1 <- ExpAgendaVonmon(obj = DocTerm, n.cats = 5)

## Find main stems associated with each topic and main topic of each speech
# TopicsStems <- TopicSummary(Est1, NStems = 5)

TopicDoc <- DocTopics(Est1)
