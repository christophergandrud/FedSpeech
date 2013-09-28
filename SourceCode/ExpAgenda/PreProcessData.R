#############
# Prepare speeches for text analysis
# Christopher Gandrud
# 28 September 2013
#############

library(tm)
library(SnowballC)
library(ExpAgenda)

# Import speeches
setwd("~/Dropbox/Fed_Speeches_Data/May2013Speeches/FedSpeechIndvParsed_20May/")
FileList <- list.files(pattern = "*.txt")
Texts <- lapply(FileList, readLines)
TextsV <- as.vector(Texts)

# Basic clean
TextsV <- tolower(TextsV)
TextsV <- removeNumbers(TextsV)
TextsV <- removePunctuation(TextsV)

# Remove stop words
StopWords <- stopwords(kind = "en")
TextsV <- removeWords(TextsV, StopWords)

TextsV <- stripWhitespace(TextsV)

# Import speaker/date meta data
Meta <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/FedSpeechesVersionMay2013.csv",
                 stringsAsFactors = FALSE)
MetaSub <- Meta[, c("full_date", "name")]

# Bind into one data frame
Full <- cbind(MetaSub, TextsV)

#### Create author matrix ####
# Order by author
FullOrd <- Full[order(Full$name), ]

# Create author matrix
AuthorsRaw <- as.data.frame(FullOrd$name)
AuthorsRaw$ID <- row.names(AuthorsRaw)

First <- by(AuthorsRaw, AuthorsRaw[, 1], head, n = 1)
Last <- by(AuthorsRaw, AuthorsRaw[, 1], tail, n = 1)

First <- do.call("rbind", as.list(First))
Last <- do.call("rbind", as.list(Last))

Authors <- cbind(First[, 1:2], Last[, 2])

names(Authors) <- c("name", "first", "last")
Authors <- as.matrix(Authors)

#### Create document term matrix ####
# Convert to corpus
# FullCorp <- Corpus(DataframeSource(Full))
FullCorp <- Corpus(VectorSource(FullOrd$TextsV))

# Stemming
Stems <- tm_map(FullCorp, stemDocument)

# Create term document matrix
TermDoc <- DocumentTermMatrix(Stems)

# Remove sparse terms
TermDocS <- removeSparseTerms(TermDoc, 0.9)






