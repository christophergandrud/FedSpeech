# ---------------------------------------------------------------------------- #
# Stan Speeches-Topics Regression v0.1
# Christopher Gandrud
# 2 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(DataCombine)
library(lubridate)
library(rstan)
library(parallel)

# Set directory for data cleaner
PrePath = '~/Dropbox/Fed_Speeches_Paper'

# ---------------------------------------------------------------------------- #
#### Get Data ####
# Generate data frame Combined
source(sprintf('%s/FedSpeech/SourceCode/DataCleanMerge/MergeForRegressions.R',
        PrePath))

# Set working directory. Change as needed.
wd <- '~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/RegressionAnalysis/speech_stan/'
setwd(wd)

# Load Stan Parallel Wrapper Function
source('parallel_4.R')

#### Reset Scrutiny to be base 0.
Combined$Scrutiny <- as.numeric(Combined$Scrutiny) - 1

#### Add Lags
LagVars <- c("CaseShillerChange", "UnemploymentRateChange", "U6RATE",
             "GDPC96Percent", "PCEPIPercent", "Scrutiny")

for (i in LagVars){
    Combined <- slide(Combined, Var = i, NewVar = paste0(i, "Lag3"),
                      slideBy = -3)
}

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

rmExcept(c('Combined', 'parallel_4'))

#### Keep Complete Cases ####
covars <- c('HFSC_CombConnect', 'FedSpoketoFed', 'ScrutinyLag3')
Combined <- Combined %>% DropNA(covars)

## Convert factor variables to numeric
Combined$name_num <- Combined$name %>% as.factor %>% as.numeric

# Data descriptions
N_names <- max(Combined$name_num)

# ---------------------------------------------------------------------------- #
#### Specify Model ####
speeches_code <- 'speech_topic.stan'

# Data
speeches_data <- list(
    N = nrow(Combined),
    n_names = N_names,
    name = Combined$name_num,
    fed_venue = Combined$FedSpoketoFed,
    donor = Combined$HFSC_CombConnect,
    scrutiny = Combined$ScrutinyLag3,
    ## Outcome
    y = Combined$Local.Housing.Dev_dummy
)


#### Run Model ####
# Create Empty Stan model (so it only needs to compile once)
empty_stan <- stan(file = speeches_code, data = speeches_data, chains = 0)

# Run on 4-cores
parallel_4 <- function(fit, data, iter = 2000, pars = c('a', 'b')){
    sflist <-
        mclapply(1:4, mc.cores = 4,
                 function(i) stan(fit = fit, data = data,
                                  seed = i, chains = 1,
                                  iter = iter, chain_id = i,
                                  pars = pars
                 )
        )
    
# Collect in to Stan fit object
fit <- sflist2stanfit(sflist)
