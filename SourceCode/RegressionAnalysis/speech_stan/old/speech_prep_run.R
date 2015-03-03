# ---------------------------------------------------------------------------- #
# Stan Speeches-Topics Regression v0.1
# Christopher Gandrud
# 3 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# Install/Load required packages
if (!('StanCat' %in% installed.packages()[, 1])) 
    devtools::install_github('christophergandrud/StanCat')
if (!('StanSpeeches' %in% installed.packages()[, 1])) 
    devtools::install_github('christophergandrud/StanSpeeches')
library(StanCat)
library(StanSpeeches)
library(DataCombine)
library(lubridate)
library(rstan)
library(dplyr)
library(ggplot2)

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

#### Keep Complete Cases ####
covars <- c('HFSC_CombConnect', 'FedSpoketoFed', 'ScrutinyLag3')
Combined <- Combined %>% DropNA(covars)

## Convert factor variables to numeric
Combined$name_num <- Combined$name %>% as.factor %>% as.numeric
full_names <- unique(Combined$name)

# Data descriptions
N_names <- max(Combined$name_num)

# ---------------------------------------------------------------------------- #
#### Specify Model ####
speeches_code <- 'speech_topic.stan'

# Data
speeches_data_housing <- list(
    N = nrow(Combined),
    K = length(covars),
    J = N_names,
    name = Combined$name_num,
    X = Combined[, covars] %>% as.matrix,
    ## Outcome
    y = Combined$Local.Housing.Dev_dummy
)


#### Run Model ####
# Create Empty Stan model (so it only needs to compile once)
empty_stan_housing <- stan(file = speeches_code, data = speeches_data_housing, 
                           chains = 0)

fit_housing <- parallel_4(fit = empty_stan_housing, 
                          data = speeches_data_housing)

#### Table results ####
basic_table <- stan_speeches_param_est(list(H1 = fit_housing), 
                                       obs = nrow(Combined), 
                                       pars_labels = c(covars, 'Intercept'))

#### Parameter estimate plots ####
stan_caterpillar(fit_housing, 'beta\\[[1-3]\\]', covars[1:3]) + 
    geom_vline(xintercept = 0, linetype = 'dotted')    

stan_caterpillar(fit_housing, '^a\\[.*\\]', full_names) + 
    geom_vline(xintercept = 0, linetype = 'dotted')    

#### Create predicted probability plots ####
fitted_1 <- c(0.023, 0, 0)
fitted_2 <- c(0.023, 1, 0)

fitted_venue <- rbind(fitted_1, fitted_2)

pred_prob_out <- predict_speeches_prob(stanfit = fit_housing, 
                     data = speeches_data_housing,
                     fitted_values = fitted_venue, a_num = 3)

pred_prob_out <- cbind(x_value = c('Not Fed Venue', 'Fed Venue' ), 
                       pred_prob_out)

ggplot(pred_prob_out, aes(x_value, medians, group = 1)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower_50, ymax = upper_50), alpha = 0.1) +
    geom_ribbon(aes(ymin = lower_95, ymax = upper_95), alpha = 0.1) +
    xlab('') + ylab('Predicted Prob. of Speaking about Housing\n') +
    scale_x_discrete(limits = rev(levels(pred_prob_out$x_value))) +
    theme_bw()
