##############
# Ties cumulative sum creation
# Christopher Gandrud & Kevin Young
# 1 August 2013
##############

# Load libraries
library(stringr)
library(plyr)
library(data.table)
library(DataCombine)
library(digest)
library(devtools)

# Set working directory
setwd("~/Dropbox/Fed Hearings")

#### -------Completed in an earlier version the manually added Speech Connectino Data
# Load csv version of the data
Ties <- read.csv("Fed_BX_Full_DATES.csv")
SubTies <- Ties[, c("Organisation", "Connected.Org.Duration", 
  "Connected.Org.Role.Start.Date","Connected.Org.Role.End.Date", 
  "Individual", "Board.of.Governors.of.the.Federal.Reserve.System.Role.Start.Date", 
  "Board.of.Governors.of.the.Federal.Reserve.System.Role.End.Date")]

names(SubTies) <- c("Organisation", "YearsExp", "StartDate", "EndDate", "Indv", "FedStart", "FedEnd")

# Keep only complete data for the 
SubTies <- subset(SubTies, !is.na(StartDate))
SubTies <- subset(SubTies, !is.na(FedStart))

# Ad Hoc Fixes for miscoding (in original)
attach(SubTies)
  SubTies$FedStart[Indv == "Thomas P FitzGibbon Jr"] <- 2004
  SubTies$FedEnd[Indv == "Thomas P FitzGibbon Jr"] <- 2013
  SubTies$StartDate[Indv == "Doctor Morris A Davis" & 
                    Organisation == "University of Wisconsin-Madison School of Business"] <- 2008
  SubTies$EndDate[Indv == "Doctor Morris A Davis" & 
                    Organisation == "University of Wisconsin-Madison School of Business"] <- 2013
detach(SubTies)


# Standardise Organisation Names
NameChange <- read.csv("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/StandardisedOrgNames.csv", stringsAsFactors = FALSE)

SubTies$Organisation <- as.character(SubTies$Organisation)
NameRows <- 1:nrow(NameChange)

for (i in NameRows){
SubTies$Organisation <- str_replace(string = SubTies$Organisation, 
                           pattern = gsub("\\\\", "\\", NameChange$From[i], 
                                          fixed = TRUE), 
                           replacement = NameChange$To[i])
}

#### -------

# Add in Connected Data 
## This data contains a variable (Match.FedSpeech) that records if the Fed gave a speech to the organization
#Connected <- read.csv("SubTiesToCode.csv", 
#                      stringsAsFactors = FALSE)

# Minor clearn
# Connected <- Connected[, -1]
# Connected$Match.FedSpeech[is.na(Connected$Match.FedSpeech)] <- 0

# Keep only when the person started working for the Fed after 1997, the year our data begins
#SubTies$EndDate[is.na(SubTies$EndDate)] <- 2013
SubTies$FedEnd[is.na(SubTies$FedEnd)] <- 2013
SubTies$EndDate[is.na(SubTies$EndDate)] <- 2013
# SubTies <- subset(SubTies, !is.na(EndDate))

# Create data frame with full years
Year <- c(1997:2013) 
DummyID <- rep(1, length(Year))
FullYears <- data.frame(DummyID, Year)

# Merge in organisation-indv names 
OrgIndvNames <- SubTies[, c("Organisation", "Indv", 
                          "StartDate", "EndDate", "FedStart", "FedEnd")]
OrgIndvNames <- OrgIndvNames[!duplicated(OrgIndvNames), ]
DummyID <- rep(1, nrow(OrgIndvNames))
OrgIndvNamesDF <- data.frame(DummyID, OrgIndvNames)

FullYears <- merge(OrgIndvNamesDF, FullYears, by = "DummyID", all = TRUE)
FullYears <- FullYears[, -1]

# Create Fed Connections
FedConnect <- FullYears[, c("Indv", "FedStart", "FedEnd", "Year")]
FedConnect <- FedConnect[!duplicated(FedConnect[, c("Indv", "FedStart", "FedEnd", "Year")]),]

FedConnect$StartDate <- FedConnect$FedStart
FedConnect$EndDate <- FedConnect$FedEnd

FedConnect$Organisation <- "Federal Reserve"
FedConnect <- MoveFront(FedConnect, "Organisation")

FullYears <- rbind.fill(FullYears, FedConnect)

FullYears <- subset(FullYears, FedEnd >= 1997)

# Calculate years experience variable
YearsExpCalc <- function(x){
  Temp <- x
  Rows <- 1:nrow(Temp)
  for (i in Rows){
    if (Temp[i, "EndDate"] <= Temp[i, "Year"]){
      Temp[i, "YearsExp"] <- Temp[i, "EndDate"] - Temp[i, "StartDate"]
    } else if (Temp[i, "EndDate"] > Temp[i, "Year"]){
      if (Temp[i, "StartDate"] > Temp[i, "Year"]){
        Temp[i, "YearsExp"] <- 0 
      } else{
        Temp[i, "YearsExp"] <- Temp[i, "Year"] - Temp[i, "StartDate"]
      }
    } 
  }
  Temp
}

FullYearExp <- YearsExpCalc(FullYears)

FullTies <- subset(FullYearExp, Year >= FedStart)
FullTies <- subset(FullTies, Year <= FedEnd)

# write.csv(FullTies, file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/BasicFedNetwork.csv")

# Clean workspace
CleanOut <- setdiff(ls(), "FullTies")
rm(list = CleanOut)

#### ----------- --------------------------- ----------- 
#### ----------- Constrict to non-rotating Board Members ---- ####

MembFile <- "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/FRB_Members_and_Spellings.csv"
MembNames <- read.csv(MembFile)
MembNames <- MembNames[, c("BoardStart", "BoardEnd", "BX_spelling")]
names(MembNames) <- c("BoardStart", "BoardEnd", "Indv")

# Merge with Fill Ties
BoardTies <- merge(FullTies, MembNames, by = "Indv")
BoardTies <- subset(BoardTies, FedStart >= BoardStart & FedEnd <= BoardEnd)


#### ----------- --------------------------- ----------- 
#### ----------- Network Centrality Analysis ----------- ####
#### ----------- --------------------------- ----------- 

library(igraph)

YearsList <- 1997:2013
DataToUse <- BoardTies
for (i in YearsList){
  # Subset the Data by year
  YearlyTies <- subset(DataToUse, Year == i)

  Var <- c("Organisation", "Indv", "YearsExp")
  foredgelist <- YearlyTies[, Var]
  
  # renaming the variables so they can be used in igraph

  names(foredgelist) <- c("sender", "receiver", "width")

  # transforming the edgelist into useable format for igraph

  g1<- graph.data.frame(foredgelist, directed = TRUE)
  #Edges <- V(g1)$name
  #colnames(Edges) <- c('sender','receiver', 'width')
  E(g1)$width
  
  # this is a package that sets the default colour and default transparency...can come in handy later
  # colvec <- rep(rgb(2,200,1, 80, names = NULL, 
  #              maxColorValue = 255), length(V(g1)$name))  
  
  # colvec[V(g1)$name=="Princeton"]<-rgb(200,25,15, 155, names=NULL, maxColorValue=255)
  
  # this plots the network graphic
  #plot(g1, layout=layout.kamada.kawai, vertex.size=20,  
  #  edge.width=E(g1)$width*.5, edge.arrow.size=0, 
  #  edge.color="blue", vertex.color = colvec, vertex.label =g1$Name, 
  #  vertex.label.color="black", vertex.label.family="sans", 
  #  vertex.label.cex=.5, vertex.label.degree = 1,
  #  main = i)
  
  # Save yearly network centrality scores
  evcentstore<-evcent(g1, scale=FALSE)
  NamesValue <- data.frame(evcentstore$vector)
  NamesValue$names <- row.names(NamesValue)
   NamesValue <- NamesValue[order(-NamesValue$evcentstore.vector),] 
  names(NamesValue) <- c("FedBoardCentrality", "Organisation")
  NamesValue$year <- i
  FileName <- paste0("~/Dropbox/Fed Hearings/EVScores31July2013/", 
                      i, ".csv")
  write.csv(NamesValue, file = FileName, row.names = FALSE)
}


#### ------------ Create JavaScript Network Graphs ------------- ####
library(d3Network)

year <- c(1997, 1999, 2000, 2001, 2003, 2006, 2009, 2013)

for (i in year){
  FileName <- paste0("~/Desktop/FedBoard/", i, ".html")
  Sub <- subset(BoardTies, Year == i)

  d3SimpleNetwork(Sub, file = FileName, 
                Source = "Indv", Target = "Organisation",
                linkDistance = 40, charge = -100,
                width = 1000, height = 500)
}



#### ------------ Kevin, I haven't touched anything after this point ------- ###
# this TRIES TO limit the number of isolates
#####But I still have a problem with getting rid of isolate vertices....
#g3 <- delete.vertices(g1, which(degree(g1) < 5))

#jpeg(file = "myplot.jpg")
#plot(g3, layout=layout.kamada.kawai, vertex.size=4,  edge.width=E(g3)$width*.1, edge.arrow.size=0, edge.color="red", vertex.color=colvec,vertex.label=NA, vertex.label.color="black", vertex.label.family="sans", vertex.label.cex=1, vertex.label.degree=0)
#dev.off()


# 
write.csv(foredgelist, file = "edgelist.csv")

# this generates the eigenvector centrality score for all actors in the network, then stores it, then writes to file

evcentstore<-evcent(g1)
NamesValue <- data.frame(evcentstore$vector)
NamesValue$names <- row.names(NamesValue)
View(NamesValue)
NamesValue <- NamesValue[order(-NamesValue$evcentstore.vector),] 
View(NamesValue)
FileName <- paste0("CentralityScores/EVCentralityBLAH", i, ".csv")
write.csv(NamesValue, file = FileName)





edgetrial <- read.csv("edgelist.csv")


names(edgetrial) <- c("sender", "receiver", "width")

# transforming the edgelist into useable format for igraph

g1<- graph.data.frame(edgetrial, directed = TRUE)
#Edges <- V(g1)$name
#colnames(Edges) <- c('sender','receiver', 'width')
E(g1)$width

# this is a package that sets the default colour and default transparency...can come in handy later
colvec <- rep(rgb(2,200,1, 80, names = NULL, 
                  maxColorValue = 255), length(V(g1)$name))  

# this plots the network graphic
plot(g1, layout=layout.kamada.kawai, vertex.size=20,  
     edge.width=E(g1)$width*.5, edge.arrow.size=0, 
     edge.color="blue", vertex.color = colvec, vertex.label =g1$Name, 
     vertex.label.color="black", vertex.label.family="sans", 
     vertex.label.cex=.5, vertex.label.degree = 1)
 

evcentstore<-evcent(g1, weight=g1$width)
NamesValue <- data.frame(evcentstore$vector)
NamesValue$names <- row.names(NamesValue)
View(NamesValue)
NamesValue <- NamesValue[order(-NamesValue$evcentstore.vector),] 
View(NamesValue)
write.csv(NamesValue, file ="trial2.csv")


#### -------------- Not used but maybe useful in the future ---------- ####
# Keep min (max) StartDate, EndDate, FedStart and FedEnd 
#MinMaxTies <- merge(SubTies, SubTies, by = c("Organisation", "Indv"))

#MinMaxTies$Copied <- 0
#attach(MinMaxTies)
#  MinMaxTies$Copied[StartDate.x == StartDate.y &
#                    EndDate.x == EndDate.y &
#                    FedStart.x == FedStart.x &
#                    FedEnd.x == FedEnd.y] <- 1
#detach(MinMaxTies)

#MinMaxTies <- subset(MinMaxTies, Copied == 0)

# Function to find minimum or maximum value
#MinMaxDates <- function(x, MM){
#  Rows <- 1:nrow(MinMaxTies)
#  VarX <- paste0(x, ".x")
#  VarY <- paste0(x, ".y")
#  for (i in Rows){
#    if (MM == "min"){
#      MinMaxTies[i, x] <- min(MinMaxTies[i, VarX], MinMaxTies[i, VarY])
#    }
#    if (MM == "max"){
#      MinMaxTies[i, x] <- max(MinMaxTies[i, VarX], MinMaxTies[i, VarY])
#    }
#  }
#  MinMaxTies
#}

#MinMaxTies <- MinMaxDates("StartDate", MM = "min")
#MinMaxTies <- MinMaxDates("EndDate", MM = "max")

#MinMaxTies <- MinMaxDates("FedStart", MM = "min")
#MinMaxTies <- MinMaxDates("FedEnd", MM = "max")

# Standardise all FedStart and FedEnd to min and max
#IndvNames <- unique(MinMaxTies$Indv)

#MinMaxTies2 <- data.frame()
#for (i in IndvNames){
#  Temp <- subset(MinMaxTies, Indv == i)
#  Temp$FedStart <- min(Temp$FedStart)
#  Temp$FedEnd <- max(Temp$FedEnd)
#  Temp$StartDate <- min(Temp$StartDate)
#  Temp$EndDate <- max(Temp$EndDate)
#  MinMaxTies2 <- rbind.fill(MinMaxTies2, Temp)
#}

# Drop duplicated and redundant
#MinMaxTies2 <- MinMaxTies2[order(MinMaxTies$Indv,
#                               MinMaxTies$Organisation,
#                               MinMaxTies$FedStart),]

#MinMaxNoDups <- MinMaxTies2[!duplicated(MinMaxTies2[, c("Organisation",
#                                       "Indv",
#                                       "StartDate",
#                                       "EndDate",
#                                       "FedStart",
#                                       "FedEnd")]), ]

#TiesUnique <- MinMaxNoDups[, c("Organisation", "Indv", 
#                              "StartDate", "EndDate", "FedStart", 
#                              "FedEnd")]

