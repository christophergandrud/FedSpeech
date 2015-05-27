# ---------------------------------------------------------------------------- #
# Stan Speeches-Topics Regression where Financial Markets is the Topic
# Christopher Gandrud
# 5 March 2015
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
covars_all <- c('mean_financial_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
                'ScrutinyLag3', 'CaseShillerChangeLag3', 'PCEPIPercentLag3',
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
    y = main$Financial.Markets_dummy
)

#### Run Models ####
# F1
vars_1 <- c('mean_financial_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3')
F1_data <- stan_lister(base = base, df = main, vars = vars_1)

F1_empty <- stan(file = speeches_code, data = F1_data, chains = 0)
F1 <- parallel_4(fit = F1_empty, data = F1_data)

# F2
vars_2 <- c('mean_financial_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'CaseShillerChangeLag3')
F2_data <- stan_lister(base = base, df = main, vars = vars_2)

F2_empty <- stan(file = speeches_code, data = F2_data, chains = 0)
F2 <- parallel_4(fit = F2_empty, data = F2_data)

# F3
vars_3 <- c('mean_financial_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'CaseShillerChangeLag3', 'PCEPIPercentLag3')
F3_data <- stan_lister(base = base, df = main, vars = vars_3)

F3_empty <- stan(file = speeches_code, data = F3_data, chains = 0)
F3 <- parallel_4(fit = F3_empty, data = F3_data)

# F4
vars_4 <- c('mean_financial_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'CaseShillerChangeLag3', 'UnemploymentRateChangeLag3')
F4_data <- stan_lister(base = base, df = main, vars = vars_4)

F4_empty <- stan(file = speeches_code, data = F4_data, chains = 0)
F4 <- parallel_4(fit = F4_empty, data = F4_data)

# F5
vars_5 <- c('mean_financial_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3', 'pres_party', 'house_dem_rep', 'senate_dem_rep')
F5_data <- stan_lister(base = base, df = main, vars = vars_5)

F5_empty <- stan(file = speeches_code, data = F5_data, chains = 0)
F5 <- parallel_4(fit = F5_empty, data = F5_data)

# ---------------------------- Output ---------------------------------------- #

#### Table results ####
financial_table <- stan_speeches_param_est(
                    stanfit = list(F1 = F1,
                                   F2 = F2,
                                   F3 = F3,
                                   F4 = F4,
                                   F5 = F5),
                    pars_labels = list(
                        F1 = c('M. Financial Topic y-1', 'Fed. Venue',
                               'HCFS Donor', 'High Scrutiny', 'Intercept'),
                        F2 = c('M. Financial Topic y-1', 'Fed. Venue',
                             'HCFS Donor', 'Case-Shiller Change m-3', 'Intercept'),
                        F3 = c('M. Financial Topic y-1', 'Fed. Venue',
                              'HCFS Donor', 'Case-Shiller Change m-3',
                              'Inflation m-3', 'Intercept'),
                        F4 = c('M. Financial Topic y-1', 'Fed. Venue',
                              'HCFS Donor', 'Case-Shiller Change m-3',
                              'Unemployment Change m-3', 'Intercept'),
                        F5 = c('M. Financial Topic y-1', 'Fed. Venue',
                              'HCFS Donor', 'High Scrutiny', 'Pres. Party',
                              'House Dem. Prop.', 'Senate Dem. Prop.',
                              'Intercept')
                        ),
                    obs = nrow(main))

var_order <- c('M. Financial Topic y-1', 'M. Financial Topic y-1_ci',
               'Fed. Venue', 'Fed. Venue_ci', 'HCFS Donor', 'HCFS Donor_ci',
               'High Scrutiny', 'High Scrutiny_ci', 'Case-Shiller Change m-3',
               'Case-Shiller Change m-3_ci', 'Inflation m-3',
               'Inflation m-3_ci',
               'Unemployment Change m-3', 'Unemployment Change m-3_ci',
               'Pres. Party', 'Pres. Party_ci', 'House Dem. Prop.',
               'House Dem. Prop._ci', 'Senate Dem. Prop.',
               'Senate Dem. Prop._ci', 'Intercept', 'Intercept_ci',
               'Obs.', 'WAIC')
vars_order <- data.frame(Parameters = var_order,
                         num_order = 1:length(var_order))

financial_table <- full_join(financial_table, vars_order, by = 'Parameters') %>%
                    arrange(num_order) %>% select(-num_order)

financial_table$Parameters <- gsub('.*_ci$', '', financial_table$Parameters)
names(financial_table) <- c('', names(financial_table)[2:length(financial_table)])

stargazer(financial_table, summary = F, out = 'tables/financial.tex',
          out.header = F, rownames = F,
          title = 'Logistic Regression Coefficient Estimates from the Posterior Distribution for Discussing Financial Markets',
          label = 'financialTable',
          notes = '95\\% credible intervals in parentheses. Speaker varying-intercepts not shown. Please see Figure \\\\ref{speakerFinancial}.',
          font.size = 'small')


##### Speaker effect plot #####
stan_caterpillar(F1, '^a\\[.*\\]', full_names) +
                geom_vline(xintercept = 0, linetype = 'dotted')

ggsave(file = 'figures/financial_speakers.pdf')
