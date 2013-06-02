###############
# Merge House Full with FedTestimony
# Christopher Gandrud
# 1 June 2013
###############

# Load packages
library(lubridate)
library(DataCombine)
library(plyr)

setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/Raw/")

# Load the data sets
HouseFull <- read.csv("MonetaryPolicyChron.csv", stringsAsFactors = FALSE)

TestimonyHalfComp <- read.csv("TestimonyHearingsIncomplete.csv", 
                              stringsAsFactors = FALSE)

# Create dates and sort
HouseFull$Date <- dmy(HouseFull$Date)
TestimonyHalfComp$Date <- dmy(TestimonyHalfComp$Date)

TestimonyHalfComp <- TestimonyHalfComp[order(TestimonyHalfComp$Date), ]
# Remove Mystery
TestimonyHalfComp <- TestimonyHalfComp[, -length(TestimonyHalfComp)]

# Add CleanFullCommitteeName1, SubCommittee, and legislature variables
HouseFull$CleanFullCommitteeName1 <- "Committee on Banking and Financial Services"
HouseFull$CleanFullCommitteeName1[year(HouseFull$Date) > 2000] <- "Committee on Financial Services"
# Add NAs to TrascriptHyperlink
TestimonyHalfComp$TranscriptHyperlink[TestimonyHalfComp$TranscriptHyperlink == ""] <- NA
TestimonyHalfComp$attendance[TestimonyHalfComp$attendance == ""] <- NA

# Add 0s to TestimonyHalfComp SubCommittee
TestimonyHalfComp$SubCommittee[is.na(TestimonyHalfComp$SubCommittee)] <- 0

# Fill In
FillInSpec <- function(data, x, y) {
FillIn(D1 = data, D2 = HouseFull,
         Var1 = x, Var2 = y, KeyVar = c("Date", "CleanFullCommitteeName1"), 
         allow.cartesian = TRUE)
}

Combined <- FillInSpec(data = TestimonyHalfComp, x = "TranscriptHyperlink", y = "URL")
Combined <- FillInSpec(data = Combined, x = "laughter", y = "Laughter.Count")
Combined <- FillInSpec(data = Combined, x = "attendance", y = "Members.Present")

# Append Full Committte
HouseFull <- HouseFull[, c("Date", "CleanFullCommitteeName1", "URL", 
                           "Members.Present", "Laughter.Count", 
                           "Fed.SemiAnnual.Oversight.Hearing", "Field.hearing")]
HouseFull$Field.hearing[is.na(HouseFull$Field.hearing)] <- 0
HouseFull$legislature <- "House"
HouseFull$SubCommittee <- 0
names(HouseFull) <- c("Date", "CleanFullCommitteeName1", "TranscriptHyperlink", "attendance", "laughter", "SemiAnnualOversight", "Field", "legislature", "SubCommittee")

Combined <- rbind.fill(Combined, HouseFull)

Combined <- Combined[order(Combined$Date), ] 

# Create a Non-Fed Testimony Variable
Combined$NonFedFinanceCom[is.na(Combined$TestimonyHyperlink)] <- 1
Combined$NonFedFinanceCom[!is.na(Combined$TestimonyHyperlink)] <- 0

# Reorder variables
Combined <- Combined[, c("NonFedFinanceCom",  "Date", "HearingNumber", 
                         "TranscriptHyperlink", "legislature", "committefull",
                         "CleanFullCommitteeName1", "CleanFullCommitteeName2",
                         "SubCommittee", "attendance", "FedLetterCorrespondence",
                          "laughter", "SemiAnnualOversight", 
                         "Field", "speechtitle", "name", "position_cat1", 
                         "position_cat2", "TestimonyHyperlink", "SecondTestimony",
                         "duplicatespeech")]

Combined$committefull[is.na(Combined$committefull) & year(Combined$Date) > 2000] <- "Committee on Financial Services"

Combined$committefull[is.na(Combined$committefull) & year(Combined$Date) <= 2000] <- "Committee on Banking and Financial Services"

# Save Play Copy
write.csv(Combined, file = "TestFullPlay.csv")

# Save sorted copy for RAs
Combined <- Combined[order(Combined$committefull, Combined$Date),]
write.csv(Combined, file = "~/Desktop/TestimonyRawShareSORTING1June.csv")