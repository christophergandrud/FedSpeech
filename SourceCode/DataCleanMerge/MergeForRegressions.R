############
# Merge together data for regressions
# Christopher Gandrud
# 24 July 2014
############

# Load packages
library(lubridate)
library(DataCombine)
library(plyr)

# Set working directory
setwd('~/Dropbox/Fed_Speeches_Paper/FedSpeech/')

##### Econ Data ####
# Load econ data
EconData <- read.csv("Data/FREDEconData.csv")
EconData <- rename(EconData, c("DateField" = "month_year"))
EconData$month_year <- ymd(EconData$month_year)

# Drop if missing
EconData <- subset(EconData, !is.na(U6RATE))
EconData <- subset(EconData, !is.na(GDPDEF))

#### Partisan Data ####
Partisan <- read.csv("Data/Raw/PartisanData.csv")

## Merge with Econ data on a quarterly basis
EconData$Quarter <- quarter(EconData$month_year, with_year = TRUE)

EconPart <- merge(Partisan, EconData, by = "Quarter", all = TRUE)

#### Topics Data ####
Topics <- read.csv("Data/TopicsSpoken.csv")
Topics$full_date <- dmy(Topics$full_date)

#### Level of Quantification in Speeches ####
Quantification <- read.csv('Data/Raw/LIWC_for_1116.csv', stringsAsFactors = FALSE)
Quantification$quanty <- Quantification$quant + Quantification$number
Quantification <- Quantification[, c('ID', 'quanty')]
names(Quantification) <- c('SpeechID', 'quanty')

# Merge with Topics Data
Topics <- merge(Topics, Quantification, by = 'SpeechID', all.x = TRUE)

#### Speeches Connectivity Data ####
Connect <- read.csv("Data/ConnectivityClean.csv", stringsAsFactors = FALSE)
Connect$full_date <- ymd(Connect$full_date)

OrgClass <- read.csv("Data/Raw/BaseSpeechCount.csv", stringsAsFactors = FALSE)
OrgClass$full_date <- dmy(OrgClass$full_date)

# Add FedSpeakToFed variable
source("SourceCode/DataCleanMerge/FedSpokenTo.R")

OrgClass <- merge(Sub, OrgClass, by = c("full_date", "name"))

# Keep if 1997 or later
OrgClass <- subset(OrgClass, year > 1996)

RemoveTitle <- function(data){
    data[, "name"] <- gsub(pattern = "Governor†", "", data[, "name"])
    data[, "name"] <- gsub(pattern = "Governor", "", data[, "name"])
    data[, "name"] <- gsub(pattern = "Chairman†", "", data[, "name"])
    data[, "name"] <- gsub(pattern = "Chairman", "", data[, "name"])
    data[, "name"] <- gsub(pattern = "Vice Chair†", "", data[, "name"])
    data[, "name"] <- gsub(pattern = "Vice Chair", "", data[, "name"])
    data
}
trim.leading <- function (x)  sub("^\\s+", "", x)

Connect <- RemoveTitle(data = Connect)
Connect$name <- trim.leading(Connect$name)
OrgClass <- RemoveTitle(data = OrgClass)

# Merge
Speeches <- merge(Connect, OrgClass, by = c("full_date", "name"), all = TRUE)
Speeches <- subset(Speeches, !is.na(full_date))
Speeches <- MoveFront(Speeches, c("month_year"))

# Drop duplicates
Speeches <- Speeches[!duplicated(Speeches[, c("full_date", "name")]), ]

# Create new month_year variable
Speeches$month_year <- floor_date(Speeches$full_date, "month")

#### Congressional Scrutiny States ####
### See ChangePointCongFed.Rnw
Speeches$Scrutiny[Speeches$month_year < as.POSIXct("2007-04-01")] <- "1"
Speeches$Scrutiny[Speeches$month_year >= as.POSIXct("2007-04-01")] <- "3"
Speeches$Scrutiny[Speeches$month_year >= as.POSIXct("2010-06-01")] <- "2"
Speeches$Scrutiny <- ordered(Speeches$Scrutiny,
                            labels = c("Low", "Medium", "High"))

#### Final Merge and Clean ####
Combined <- merge(Speeches, Topics, by = c("full_date", "name"))
Combined <- merge(Combined, EconPart, by = "month_year")

KeepVars <- c("month_year", "full_date", "name", "position_cat", "Organisation",
              "HFSC_ChairConnect", "HFSC_RankMembConnect", "SpeakerConnect",
              "HFSC_CombConnect", "FedBoardCentrality", "FedSpoketoFed",
              "bankersfinance", "other_private", "otherregulators",
              "io", "community_organisations",
              "thinktank",               "press_association",
              "prof_econ_assoc",         "university",
              "hearing",                 "trade_assoc",
              "non_finance_gov",         "nonbuinessadvocacy",
              "social_events",           "economic_literacy",
              "other",                   "org",
              "Scrutiny",                "SpeechID",
              "Financial.Markets",       "Macroeconomics",
              "Monetary.Policy",         "International.Economy",
              "Local.Housing.Dev",       "Banking.Regulation",
              "CPIAUCNS",                "PCEPI",
              "INTDSRUSM193N",           "DFF",
              "FEDFUNDS",                "GDPDEF",
              "GDPC96",                  "U6RATE",
              "SPCS10RSA",               "CPIAUCNSPercent",
              "PCEPIPercent",            "GDPC96Percent",
              "CaseShillerChange",       "UnemploymentRateChange",
              "pres_party", "house_dem_rep", "senate_dem_rep")

Combined <- Combined[, KeepVars]

DeleteObj <- setdiff(ls(), c("Combined"))
rm(list = DeleteObj)
rm(DeleteObj)
