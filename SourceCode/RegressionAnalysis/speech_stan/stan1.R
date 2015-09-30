# ---------------------------------------------------------------------------- #
# Clean data for Stan Speeches-Topics Regressions
# Christopher Gandrud
# 4 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
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

# Robustness dummies
## 50%
for (i in topics) {
    NewVar <- sprintf("%s_dummy_50", i)
    TopicMean <- mean(Combined[, i])
    Combined[, NewVar] <- 0
    Combined[, NewVar][Combined[, i] >= 0.5] <- 1
}

# Find previous year topic averages by month
topics_lag_y1 <- Combined %>% group_by(month_year) %>%
                summarise(mean_monetary_lag_y1 = mean(Monetary.Policy),
                          mean_housing_lag_y1 = mean(Local.Housing.Dev),
                          mean_financial_lag_y1 = mean(Financial.Markets),
                          mean_banking_lag_y1 = mean(Banking.Regulation)
                  )

topics_lag_y1$month_year <- topics_lag_y1$month_year + months(12)

# Find previous month topic averages
topics_lag_m1 <- Combined %>% group_by(month_year) %>%
                summarise(mean_monetary_lag_m1 = mean(Monetary.Policy),
                            mean_housing_lag_m1 = mean(Local.Housing.Dev),
                            mean_financial_lag_m1 = mean(Financial.Markets),
                            mean_banking_lag_m1 = mean(Banking.Regulation)
                     )

topics_lag_m1$month_year <- topics_lag_m1$month_year + months(1)

topics_lag_comb <- full_join(topics_lag_y1, topics_lag_m1, 
                             by = 'month_year') %>% arrange(month_year)

Combined <- full_join(Combined, topics_lag_comb, by = 'month_year')

# ---------------------------------------------------------------------------- #
# Save data
write.csv(Combined, file = 'combined_data.csv', row.names = F)
