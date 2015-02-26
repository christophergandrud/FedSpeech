# ---------------------------------------------------------------------------- #
# Multilevel logistic regressions for Fed Speeches
# Christopher Gandrud
# 26 February 2015
# MIT License
# ---------------------------------------------------------------------------- #

library(lme4)
library(lattice)

# Set working directory
PrePath = '~/Dropbox/Fed_Speeches_Paper'
setwd(PrePath)

# ---------------------------------------------------------------------------- #
#### Get Data ####
# Generate data frame Combined
source('FedSpeech/SourceCode/DataCleanMerge/MergeForRegressions.R')

#### Add Lags
LagVars <- c("CaseShillerChange", "UnemploymentRateChange", "U6RATE",
             "GDPC96Percent", "PCEPIPercent", "Scrutiny")

for (i in LagVars){
    Combined <- slide(Combined, Var = i, NewVar = paste0(i, "Lag3"),
                      slideBy = -3)
}

# Turn ScrutinyLag3 into a factor
Combined$ScrutinyLag3 <- factor(Combined$ScrutinyLag3,
                                labels = c("Low", "High"))

# Rescale quanty to a proportion from a percent
Combined$quanty <- Combined$quanty/100

# Convert groups to factors
Combined$name <- as.factor(Combined$name)
Combined$month_year <- as.factor(Combined$month_year)

# ---------------------------------------------------------------------------- #
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

rmExcept('Combined')

#### Models ####
m1 <- glmer(Monetary.Policy_dummy ~ FedSpoketoFed + HFSC_CombConnect + 
                ScrutinyLag3 + PCEPIPercentLag3 +
                (1 | name) + (1 | month_year), 
            data = Combined, family = binomial)

m2 <- glmer(Local.Housing.Dev_dummy ~ FedSpoketoFed + HFSC_CombConnect + 
                ScrutinyLag3 +
                (1 | name) + (1 | month_year), 
            data = Combined, family = binomial)

m3 <- glmer(Financial.Markets_dummy ~ FedSpoketoFed + HFSC_CombConnect + 
                ScrutinyLag3 +
                (1 | name) + (1 | month_year), 
            data = Combined, family = binomial)

m4 <- glmer(Banking.Regulation_dummy ~ FedSpoketoFed + HFSC_CombConnect + 
                ScrutinyLag3 +
                (1 | name) + (1 | month_year), 
            data = Combined, family = binomial)

## Plot intercepts
dotplot(ranef(m1, which = "name", condVar = T))
dotplot(ranef(m2, which = "name", condVar = T))
dotplot(ranef(m3, which = "name", condVar = T))
dotplot(ranef(m4, which = "name", condVar = T))
