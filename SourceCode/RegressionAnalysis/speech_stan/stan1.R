# ---------------------------------------------------------------------------- #
# Clean data for Stan Speeches-Topics Regressions
# Christopher Gandrud
# 3 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Install/Load required packages
library(DataCombine)
library(lubridate)
library(dplyr)

# Set directory for data cleaner
PrePath = '~/Dropbox/Fed_Speeches_Paper/FedSpeech'

# ---------------------------------------------------------------------------- #
#### Get Data ####
# Generate data frame Combined
source(sprintf('%s/SourceCode/DataCleanMerge/MergeForRegressions.R',
        PrePath))

# Set working directory. Change as needed.
wd <- '~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/RegressionAnalysis/speech_stan/'
setwd(wd)

#### Reset Scrutiny to be base 0.
Combined$Scrutiny <- as.numeric(Combined$Scrutiny) - 1

#### Add Lags
LagVars <- c("CaseShillerChange", "UnemploymentRateChange", "U6RATE",
             "GDPC96Percent", "PCEPIPercent", "Scrutiny")

for (i in LagVars){
    Combined <- slide(Combined, Var = i, NewVar = paste0(i, "Lag3"),
                      slideBy = -3)
}

# Create dummies for greater than or equal to median topic proportion
topics <- c('Monetary.Policy', 'Local.Housing.Dev', 'Financial.Markets',
            'Banking.Regulation')

for (i in topics) {
    NewVar <- sprintf("%s_dummy", i)
    TopicMean <- mean(Combined[, i])
    message(sprintf("%s: %s", i, round(TopicMean, digits = 2)))
    Combined[, NewVar] <- 0
    Combined[, NewVar][Combined[, i] >= TopicMean] <- 1
}

# ---------------------------------------------------------------------------- #
# Save data
write.csv(Combined, file = 'combined_data.csv', row.names = F)
