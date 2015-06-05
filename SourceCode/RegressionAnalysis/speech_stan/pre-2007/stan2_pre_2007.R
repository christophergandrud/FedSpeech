# ---------------------------------------------------------------------------- #
# Stan Speeches-Topics Regression where Monetary Policy is the Topic
# Pre 2007 subset
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# ------------------------- Set Up ------------------------------------------- #
# Set working directory. Change as needed
wd <- '~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/RegressionAnalysis/speech_stan/'
setwd(wd)

# Install/Load required packages
library(rio)
if (!('StanCat' %in% installed.packages()[, 1]))
    devtools::install_github('christophergandrud/StanCat')
if (!('StanSpeeches' %in% installed.packages()[, 1]))
    devtools::install_github('christophergandrud/StanSpeeches')
library(StanCat)
library(StanSpeeches)
library(dplyr)
library(lubridate)
library(DataCombine)
library(rstan)
library(stargazer)
library(ggplot2)

# Load data
main <- import('combined_data.csv')

# --------------------------- Final cleaning --------------------------------- #

# Keep complete cases
covars_all <- c('mean_monetary_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
                'ScrutinyLag3',
                'CaseShillerChangeLag3', 'PCEPIPercentLag3',
                'UnemploymentRateChangeLag3', 'pres_party', 'house_dem_rep',
                'senate_dem_rep')
main <- main %>% DropNA(covars_all)

# Subset to before 2007
main$month_year <- ymd(main$month_year)
main <- main %>% filter(month_year < '2007-01-01')

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
    y = main$Monetary.Policy_dummy
)

#### Run Models ####
# M1
vars_1 <- c('mean_monetary_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3')
M1_data <- stan_lister(base = base, df = main, vars = vars_1)

M1_empty <- stan(file = speeches_code, data = M1_data, chains = 0)
M1 <- parallel_4(fit = M1_empty, data = M1_data)

# H2
vars_2 <- c('mean_monetary_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'PCEPIPercentLag3')
M2_Data <- stan_lister(base = base, df = main, vars = vars_2)

M2_empty <- stan(file = speeches_code, data = M2_Data, chains = 0)
M2 <- parallel_4(fit = M2_empty, data = M2_Data)

# M3
vars_3 <- c('mean_monetary_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'PCEPIPercentLag3', 'CaseShillerChangeLag3')
M3_data <- stan_lister(base = base, df = main, vars = vars_3)

M3_empty <- stan(file = speeches_code, data = M3_data, chains = 0)
M3 <- parallel_4(fit = M3_empty, data = M3_data)

# M4
vars_4 <- c('mean_monetary_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'PCEPIPercentLag3', 'UnemploymentRateChangeLag3')
M4_data <- stan_lister(base = base, df = main, vars = vars_4)

M4_empty <- stan(file = speeches_code, data = M4_data, chains = 0)
M4 <- parallel_4(fit = M4_empty, data = M4_data)

# M5
vars_5 <- c('mean_monetary_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3', 'pres_party', 'house_dem_rep', 'senate_dem_rep')
M5_data <- stan_lister(base = base, df = main, vars = vars_5)

M5_empty <- stan(file = speeches_code, data = M5_data, chains = 0)
M5 <- parallel_4(fit = M5_empty, data = M5_data)

# ---------------------------- Output ---------------------------------------- #
#### Table results ####
monetary_table <- stan_speeches_param_est(
    stanfit = list(M1 = M1,
                   M2 = M2,
                   M3 = M3,
                   M4 = M4,
                   M5 = M5),
    pars_labels = list(
        M1 = c('M. Monetary Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'High Scrutiny', 'Intercept'),
        M2 = c('M. Monetary Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'Inflation m-3', 'Intercept'),
        M3 = c('M. Monetary Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'Inflation m-3',
               'Case-Shiller Change m-3', 'Intercept'),
        M4 = c('M. Monetary Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'Inflation m-3', 
               'Unemployment Change m-3', 'Intercept'),
        M5 = c('M. Monetary Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'High Scrutiny', 'Pres. Party',
               'House Dem. Prop.', 'Senate Dem. Prop.',
               'Intercept')
    ),
    obs = nrow(main))

var_order <- c('M. Monetary Topic y-1', 'M. Monetary Topic y-1_ci',
               'Fed. Venue', 'Fed. Venue_ci', 'HCFS Donor', 'HCFS Donor_ci',
               'High Scrutiny', 'High Scrutiny_ci', 'Inflation m-3',
               'Inflation m-3_ci','Case-Shiller Change m-3',
               'Case-Shiller Change m-3_ci',
               'Unemployment Change m-3', 'Unemployment Change m-3_ci',
               'Pres. Party', 'Pres. Party_ci', 'House Dem. Prop.',
               'House Dem. Prop._ci', 'Senate Dem. Prop.',
               'Senate Dem. Prop._ci', 'Intercept', 'Intercept_ci',
               'Obs.', 'WAIC')
vars_order <- data.frame(Parameters = var_order,
                         num_order = 1:length(var_order))

monetary_table <- full_join(monetary_table, vars_order, by = 'Parameters') %>%
    arrange(num_order) %>% select(-num_order)

monetary_table$Parameters <- gsub('.*_ci$', '', monetary_table$Parameters)
names(monetary_table) <- c('', names(monetary_table)[2:length(monetary_table)])

#stargazer(monetary_table, summary = F, out = 'tables/monetary.tex',
#          out.header = F, rownames = F,
#          title = 'Logistic Regression Coefficient Estimates from the Posterior Distribution for Discussing Monetary Policy',
#          label = 'monetaryTable',
#          notes = 'Posterior distribution medians, with 95\\% credible intervals in parentheses. Speaker varying-intercepts not shown. Please see Figure \\\\ref{speakerMonetary}.',
#          font.size = 'small')


##### Speaker effect plot #####
stan_caterpillar(M1, '^a\\[.*\\]', full_names) +
    geom_vline(xintercept = 0, linetype = 'dotted')

#ggsave(file = 'figures/monetary_speakers.pdf')
