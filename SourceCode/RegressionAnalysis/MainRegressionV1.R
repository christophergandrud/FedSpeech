#############
# Main Regression Analysis (v1)
# Christopher Gandrud
# 8 August 2013
#############

# Load packages
library(Zelig)

# Generate data frame Combined
source("~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/DataCleanMerge/MergeForRegressions.R")

#### Connections
# Multiple FedBoardCentrality by 100
Combined$FedBoardCentrality <- Combined$FedBoardCentrality*100

# Test Models for FedBoardCentrality
M1 <- zelig(FedBoardCentrality ~ CaseShillerChange, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
M2 <- zelig(FedBoardCentrality ~ PCEPIPercent, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
M3 <- zelig(FedBoardCentrality ~ UnemploymentRateChange, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
M5 <- zelig(FedBoardCentrality ~ GDPC96Percent, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
M6 <- zelig(FedBoardCentrality ~ Scrutiny, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
M7 <- zelig(FedBoardCentrality ~ Scrutiny + CaseShillerChange, 
            data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)

M3 <- lm(FedBoardCentrality ~ SPCS10RSA, data = Combined)
M4 <- lm(FedBoardCentrality ~ U6RATE, data = Combined)
M5 <- lm(FedBoardCentrality ~ UnemploymentRateChange, data = Combined)

# Test Models for HFSC_CombConnect
M1 <- lm(HFSC_CombConnect ~ CaseShillerChange, data = Combined)
M2 <- lm(HFSC_CombConnect ~ SPCS10RSA, data = Combined)
M3 <- lm(HFSC_CombConnect ~ U6RATE, data = Combined)
M4 <- lm(HFSC_CombConnect ~ UnemploymentRateChange, data = Combined)

#### Topics
# Test Models for Financial.Markets
M1 <- lm(Financial.Markets ~ CaseShillerChange, data = Combined)
M2 <- lm(Financial.Markets ~ SPCS10RSA, data = Combined)
M3 <- lm(Financial.Markets ~ U6RATE, data = Combined)
M4 <- lm(Financial.Markets ~ UnemploymentRateChange, data = Combined)