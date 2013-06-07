##############
# Ties cumulative sum creation
# Christopher Gandrud & Kevin Young
# 7 June 2013
##############

# Kevin
####I also converted all 'current' dates to 2013.
####I also changed all 'date' obs to 'year' obs in excel.


# Load libraries
library(stringr)
library(plyr)
library(data.table)

# Set working directory
setwd("~/Dropbox/Fed Hearings")

#### -------Completed in an earlier version the manually added Speech Connectino Data
# Load csv version of the data
#Ties <- read.csv("Fed_BX_Full_DATES.csv")
# SubTies <- Ties[, c("Organisation", "Connected.Org.Duration", "Connected.Org.Role.Start.Date", "Connected.Org.Role.End.Date", "Individual", "Board.of.Governors.of.the.Federal.Reserve.System.Role.Start.Date", "Board.of.Governors.of.the.Federal.Reserve.System.Role.End.Date")]


# Keep Organisation, years.of.experience, Connected.Org.Role.End.Date, Individual, FedStart, FedEnd
# SubTies <- Ties[, c("Organisation", "Connected.Org.Duration", "Connected.Org.Role.Start.Date","Connected.Org.Role.End.Date", "Individual", "Board.of.Governors.of.the.Federal.Reserve.System.Role.Start.Date", "Board.of.Governors.of.the.Federal.Reserve.System.Role.End.Date")]

# names(SubTies) <- c("Organisation", "YearsExp", "StartDate", "EndDate", "Indv", "FedStart", "FedEnd")


# Keep only start years FOR THE FED
# SubTies$Year <- str_extract(SubTies$FedStart, "[1-2][0-9][0-9][0-9]")
# SubTies$Year <- as.numeric(SubTies$Year)
#### -------

# Add in Connected Data 
## This data contains a variable (Match.FedSpeech) that records if the Fed gave a speech to the organization
Connected <- read.csv("~/Dropbox/Fed Hearings/SubTiesToCode.csv", 
                      stringsAsFactors = FALSE)

# Minor clearn
Connected <- Connected[, -1]
Connected$Match.FedSpeech[is.na(Connected$Match.FedSpeech)] <- 0

# Keep only when the person started working for the Fed after 1997, the year our data begins
Connected$EndDate[is.na(Connected$EndDate)] <- 2013
Connected$FedEnd[is.na(Connected$FedEnd)] <- 2013
Connected <- subset(Connected, FedStart >= 1997)

# Keep min (max) StartDate, EndDate, FedStart and FedEnd 
MinMaxTies <- merge(Connected, Connected, by = c("Organisation", "Indv"))

MinMaxTies$Copied <- 0
attach(MinMaxTies)
  MinMaxTies$Copied[StartDate.x == StartDate.y &
                    EndDate.x == EndDate.y &
                    FedStart.x == FedStart.x &
                    FedEnd.x == FedEnd.y] <- 1
detach(MinMaxTies)

MinMaxTies <- subset(MinMaxTies, Copied == 0)

# Function to find minimum or maximum value
MinMaxDates <- function(x, MM){
  Rows <- 1:nrow(MinMaxTies)
  VarX <- paste0(x, ".x")
  VarY <- paste0(x, ".y")
  for (i in Rows){
    if (MM == "min"){
      MinMaxTies[i, x] <- min(MinMaxTies[i, VarX], MinMaxTies[i, VarY])
    }
    if (MM == "max"){
      MinMaxTies[i, x] <- max(MinMaxTies[i, VarX], MinMaxTies[i, VarY])
    }
  }
  MinMaxTies
}

MinMaxTies <- MinMaxDates("StartDate", MM = "min")
MinMaxTies <- MinMaxDates("EndDate", MM = "max")

MinMaxTies <- MinMaxDates("FedStart", MM = "min")
MinMaxTies <- MinMaxDates("FedEnd", MM = "max")

# Drop duplicated and redundant
MinMaxNoDups <- MinMaxTies[!duplicated(MinMaxTies$Organisation,
                                       MinMaxTies$Indv,
                                       MinMaxTies$StartDate,
                                       MinMaxTies$EndDate,
                                       MinMaxTies$FedStart,
                                       MinMaxTies$FedEnd,),]
MinMaxNoDups <- MinMaxNoDups[order(MinMaxNoDups$Indv,
                                  MinMaxNoDups$Organisation),]

TiesUnique <- MinMaxNoDups[, c("Match.FedSpeech.x", "Organisation", "Indv", 
                               "StartDate", "EndDate", "FedStart", "FedEnd",
                               "YearsExp.x")]
TiesUnique <- rename(TiesUnique, c("YearsExp.x" = "YearsExp",
                                   "Match.FedSpeech.x" = "Match.FedSpeech"))

# Clean up individual FedStart and StopYears
attach(TiesUnique)
  TiesUnique$FedStart[Indv == "Doctor Ben S Bernanke"] <- 2002
  TiesUnique$FedStart[Indv == "Doctor Karen Dynan"] <- 2004
  TiesUnique$FedEnd[Indv == "Doctor Karen Dynan"] <- 2013
  TiesUnique$FedStart[Indv == "Steven B Kamin"] <- 1999
  TiesUnique$FedEnd[Indv == "Steven B Kamin"] <- 2013
  TiesUnique$FedStart[Indv == "Thomas P FitzGibbon Jr"] <- 2004
  TiesUnique$FedEnd[Indv == "Thomas P FitzGibbon Jr"] <- 2013
detach(TiesUnique)

# Create data frame with full years
Year <- 1996:2013 
DummyID <- rep(1, length(Year))
FullYears <- data.frame(DummyID, Year)

# Merge in organisation-indv names 
OrgIndvNames <- TiesUnique[, c("Match.FedSpeech", "Organisation", "Indv", 
                          "StartDate", "EndDate", "FedStart", "FedEnd")]
OrgIndvNames <- OrgIndvNames[!duplicated(OrgIndvNames), ]
DummyID <- rep(1, nrow(OrgIndvNames))
OrgIndvNamesDF <- data.frame(DummyID, OrgIndvNames)

FullYears <- merge(OrgIndvNamesDF, FullYears, by = "DummyID", all = TRUE)
FullYears <- FullYears[, -1]

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

# Clean workspace
CleanOut <- setdiff(ls(), "FullTies")
rm(list = CleanOut)

#### ----------- --------------------------- ----------- 
#### ----------- Network Centrality Analysis ----------- ####
#### ----------- --------------------------- ----------- 

library(igraph)

YearsList <- 1997:2012
for (i in YearsList){
  # Subset the Data by year
  YearlyTies <- subset(FullTies, Year == i)

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
  colvec <- rep(rgb(60,1,1, 20, names = NULL, 
                maxColorValue = 255), length(V(g1)$name))  

  # this plots the network graphic
  plot(g1, layout=layout.kamada.kawai, vertex.size=4,  
    edge.width=E(g1)$width*.1, edge.arrow.size=0, 
    edge.color="red", vertex.color = colvec, vertex.label = NA, 
    vertex.label.color="black", vertex.label.family="sans", 
    vertex.label.cex=1, vertex.label.degree = 0,
    main = i)
}


#### ------------ Kevin, I haven't touched anything after this point ------- ###
# this TRIES TO limit the number of isolates
#####But I still have a problem with getting rid of isolate vertices....
#g3 <- delete.vertices(g1, which(degree(g1) < 5))

#jpeg(file = "myplot.jpg")
#plot(g3, layout=layout.kamada.kawai, vertex.size=4,  edge.width=E(g3)$width*.1, edge.arrow.size=0, edge.color="red", vertex.color=colvec,vertex.label=NA, vertex.label.color="black", vertex.label.family="sans", vertex.label.cex=1, vertex.label.degree=0)
#dev.off()


# this generates the eigenvector centrality score for all actors in the network, then stores it, then writes to file

evcentstore<-evcent(g1)
NamesValue <- data.frame(evcentstore$vector)
NamesValue$names <- row.names(NamesValue)
View(NamesValue)
NamesValue <- NamesValue[order(-NamesValue$evcentstore.vector),] 
View(NamesValue)
write.csv(NamesValue, file = "EVcentrality.csv")
