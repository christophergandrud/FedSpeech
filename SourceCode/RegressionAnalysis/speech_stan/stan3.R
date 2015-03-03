# ---------------------------------------------------------------------------- #
# Stan Speeches-Topics Regression where Local Housing is the Topic
# Christopher Gandrud
# 3 March 2015
# MIT License
# ---------------------------------------------------------------------------- #

# ------------------------- Set Up ------------------------------------------- #
# Set working directory. Change as needed
wd <- '~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/RegressionAnalysis/speech_stan/'
setwd(wd)

# Install/Load required packages
if (!('StanCat' %in% installed.packages()[, 1]))
    devtools::install_github('christophergandrud/StanCat')
if (!('StanSpeeches' %in% installed.packages()[, 1]))
    devtools::install_github('christophergandrud/StanSpeeches')
library(StanCat)
library(StanSpeeches)
library(dplyr)
library(DataCombine)
library(rstan)
library(stargazer)
library(ggplot2)

# Load data
main <- read.csv('combined_data.csv', stringsAsFactors = F)

# --------------------------- Final cleaning --------------------------------- #

# Keep complete cases
covars_all <- c('FedSpoketoFed', 'HFSC_CombConnect', 'ScrutinyLag3',
                'CaseShillerChangeLag3', 'PCEPIPercentLag3',
                'UnemploymentRateChangeLag3', 'pres_party', 'house_dem_rep',
                'senate_dem_rep')
main <- main %>% DropNA(covars_all)

## Convert factor variables to numeric
main$name_num <- main$name %>% as.factor %>% as.numeric
full_names <- unique(main$name)

# Data descriptions
N_names <- max(main$name_num)

# ---------------------------- Model ----------------------------------------- #
# Model
speeches_code <- 'speech_topic.stan'

# Data
base <- list(
    N = nrow(main),
    S = N_names,
    speaker = main$name_num,
    ## Outcome
    y = main$Local.Housing.Dev_dummy
)

#### Run Models ####
# H1
vars_1 <- c('FedSpoketoFed', 'HFSC_CombConnect' 'ScrutinyLag3')
H1_data <- stan_lister(base = base, df = main, vars = vars_1)

H1_empty <- stan(file = speeches_code, data = H1_data, chains = 0)
H1 <- parallel_4(fit = H1_empty, data = H1_data)

# H2
vars_2 <- c('HFSC_CombConnect', 'FedSpoketoFed', 'CaseShillerChangeLag3')
H2_data <- stan_lister(base = base, df = main, vars = vars_2)

H2_empty <- stan(file = speeches_code, data = H2_data, chains = 0)
H2 <- parallel_4(fit = H2_empty, data = H2_data)

# H3
vars_3 <- c('HFSC_CombConnect', 'FedSpoketoFed', 'CaseShillerChangeLag3',
            'PCEPIPercentLag3')
H3_data <- stan_lister(base = base, df = main, vars = vars_3)

H3_empty <- stan(file = speeches_code, data = H3_data, chains = 0)
H3 <- parallel_4(fit = H3_empty, data = H3_data)

# ---------------------------- Output ---------------------------------------- #

#### Table results ####
housing_table <- stan_speeches_param_est(
                    stanfit = list(H1 = H1, 
                                   H2 = H2,
                                   H3 = H3),
                    pars_labels = list(
                        H1 = c('Fed. Venue', 'HCFS Donor', 
                               'High Scrutiny', 'Intercept'),
                        H2 = c('Fed. Venue', 'HCFS Donor', 
                               'Case-Shiller Change', 'Intercept'),
                        H3 = c('Fed. Venue', 'HCFS Donor', 
                               'Case-Shiller Change', 'Inflation', 
                               'Intercept')),
                    obs = nrow(main))

var_order <- c('Fed. Venue', 'Fed. Venue_ci', 'HCFS Donor', 'HCFS Donor_ci',
               'High Scrutiny', 'High Scrutiny_ci', 'Case-Shiller Change', 
               'Case-Shiller Change_ci', 'Inflation', 'Inflation_ci',
               'Intercept', 'Intercept_ci', 
               'Obs.', 'WAIC')
vars_order <- data.frame(Parameters = var_order, num_order = 1:length(var_order))

housing_table <- full_join(housing_table, vars_order, by = 'Parameters') %>%
                    arrange(num_order) %>% select(-num_order) 

housing_table$Parameters <- gsub('.*_ci$', '', housing_table$Parameters)
names(housing_table) <- c('', names(housing_table)[2:length(housing_table)])

stargazer(housing_table, summary = F, out = 'tables/housing.tex', 
          out.header = F, rownames = F, 
          title = 'Coefficient Estimates from the Posterior Distribution for Discussions of Local Housing and Development',
          notes = '95\\% credible intervals in parentheses. Speaker varying-intercepts not shown.')
