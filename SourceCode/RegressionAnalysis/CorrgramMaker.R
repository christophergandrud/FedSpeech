#############
# Create correlation matrix diagram
# Christopher Gandrud
# 8 August 2013
#############

# Load package
library(corrgram)

# Load data
# Generate data frame Combined
source("~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/DataCleanMerge/MergeForRegressions.R")

# Variables to include
CorVars <- c("FedBoardCentrality", "HFSC_CombConnect", "SpeakerConnect",
             "Financial.Markets", "Macroeconomics", "Monetary.Policy",        
             "International.Economy", "Local.Housing.Dev", 
             "Banking.Regulation", "Scrutiny", "PCEPIPercent",
             "GDPC96Percent", "CaseShillerChange", 
             "UnemploymentRateChange")

# Subset 
CorData <- Combined[, CorVars]

# Convert Scrutiny to numeric
CorData$Scrutiny <- as.numeric(CorData$Scrutiny)

# Create graph
corrgram(CorData, order = TRUE, upper.panel = NULL, 
         diag.panel = panel.minmax)
