##############
# Ties cumulative sum creation
# Christopher Gandrud
# 28 May 2013
##############

# Load libraries
library(stringr)
library(plyr)
library(data.table)

# Set working directory
setwd("~/Dropbox/Fed Hearings")

# Load csv version of the data
Ties <- read.csv("InstitutionalCumSum.csv")

# Keep Organisation, years.of.experience, Connected.Org.Role.Start.Date, Individual
SubTies <- Ties[, c("Organisation", "years.of.experience", "Connected.Org.Role.Start.Date", "Individual")]
names(SubTies) <- c("Organisation", "YearsExp", "StartDate", "Indv")

# Keep only start years
SubTies$Year <- str_extract(SubTies$StartDate, "[1-2][0-9][0-9][0-9]")
SubTies$Year <- as.numeric(SubTies$Year)

# Drop if year is missing--Kevin what are these?
SubTies <- SubTies[, -3]
SubTies <- subset(SubTies, !is.na(Year))

# Reclassify YearsExp NA as 0.5 (i.e. less than one year of experience)
SubTies$YearsExp[is.na(SubTies$YearsExp)] <- 0.5

# Make sure that the Year is ordered correctly by organisation-Indv
SubTies <- ddply(SubTies, .(Organisation), function(x) x[order(x$Year, x$Indv),])

# Drop mystery duplicates
SubTies <- SubTies[!duplicated(SubTies[, 1:4]), ]

# Create cumulative sum by Organisation
## Sum by organisation-year
# SubTies <- ddply(SubTies, .(Organisation, Year), 
#                  transform, SumYear = sum(YearsExp))

# Create data frame with full years
Year <- 1996:2013 
DummyID <- rep(1, length(Year))
FullYears <- data.frame(DummyID, Year)

# Merge in organisation-indv names 
OrgIndvNames <- SubTies[, c("Organisation", "Indv")]
OrgIndvNames <- OrgIndvNames[!duplicated(OrgIndvNames), ]
DummyID <- rep(1, nrow(OrgIndvNames))
OrgIndvNamesDF <- data.frame(DummyID, OrgIndvNames)

FullYears <- merge(OrgIndvNamesDF, FullYears, by = "DummyID", all = TRUE)
FullYears <- FullYears[, -1]
names(FullYears) <- c("Organisation", "Indv", "Year")

# Merge SubTies and FullYears
KeyVars <- c("Organisation", "Indv", "Year")

D1Temp <- data.table(FullYears, key = KeyVars)
D2Temp <- data.table(SubTies, key = KeyVars)

FullTies <- merge(D2Temp, D1Temp, all = TRUE, allow.cartesian = TRUE)

# Create cumulative sum
FullTies$YearsExp[is.na(FullTies$YearsExp)] <- 0

FullTies <- FullTies[order(FullTies$Organisation, FullTies$Indv, FullTies$Year),]

FullTies <- ddply(FullTies, .(Organisation, Indv), transform, CumSum = cumsum(YearsExp))

# Clean up and save
FullTies <- subset(FullTies, Year >= 1996)
FullTies <- FullTies[, c("Organisation", "Indv", "Year", "CumSum")]
write.csv(FullTies, file = "OrgTiesCumSum.csv")