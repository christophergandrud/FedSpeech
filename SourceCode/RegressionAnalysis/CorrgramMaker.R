#############
# Create correlation matrix diagram
# Christopher Gandrud
# 5 March 2015
#############

# Load package
library(dplyr)
library(corrgram)

# Load data
# Generate data frame Combined
source("~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/DataCleanMerge/MergeForRegressions.R")

# Variables to include
CorVars <- c("FedSpoketoFed", "HFSC_CombConnect", "Scrutiny", "PCEPIPercent",
             "CaseShillerChange", "UnemploymentRateChange",
             "Financial.Markets", "Macroeconomics", "Monetary.Policy",        
             "International.Economy", "Local.Housing.Dev", 
             "Banking.Regulation")

# Subset 
CorData <- Combined[, CorVars]

# Convert Scrutiny to numeric
CorData$Scrutiny <- as.numeric(CorData$Scrutiny)

# Rename
CorData <- CorData %>% 
            dplyr::rename(`Fed. Venue` = FedSpoketoFed,
                        `HCFS Donor` = HFSC_CombConnect,
                        Inflation = PCEPIPercent,
                        `Case-Shiller Change` = CaseShillerChange,
                        `Unemployment Change` = UnemploymentRateChange)

# Create graph
corrgram(CorData, order = TRUE, upper.panel = NULL, 
         diag.panel = panel.minmax)
