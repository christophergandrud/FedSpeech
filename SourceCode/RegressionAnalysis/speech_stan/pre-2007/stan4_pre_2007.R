# ---------------------------------------------------------------------------- #
# Stan Speeches-Topics Regression where Banking Regulation is the Topic
# Pre 2007 subset
# Christopher Gandrud
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
covars_all <- c('mean_banking_lag_y1', 'FedSpoketoFed',
                'HFSC_CombConnect', 'ScrutinyLag3',
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
    y = main$Banking.Regulation_dummy
)

#### Run Models ####
# B1
vars_1 <- c('mean_banking_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3')
B1_data <- stan_lister(base = base, df = main, vars = vars_1)

B1_empty <- stan(file = speeches_code, data = B1_data, chains = 0)
B1 <- parallel_4(fit = B1_empty, data = B1_data)

# B2
vars_2 <- c('mean_banking_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'CaseShillerChangeLag3')
B2_data <- stan_lister(base = base, df = main, vars = vars_2)

B2_empty <- stan(file = speeches_code, data = B2_data, chains = 0)
B2 <- parallel_4(fit = B2_empty, data = B2_data)

# B3
vars_3 <- c('mean_banking_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'CaseShillerChangeLag3', 'PCEPIPercentLag3')
B3_data <- stan_lister(base = base, df = main, vars = vars_3)

B3_empty <- stan(file = speeches_code, data = B3_data, chains = 0)
B3 <- parallel_4(fit = B3_empty, data = B3_data)

# B4
vars_4 <- c('mean_banking_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'CaseShillerChangeLag3', 'UnemploymentRateChangeLag3')
B4_data <- stan_lister(base = base, df = main, vars = vars_4)

B4_empty <- stan(file = speeches_code, data = B4_data, chains = 0)
B4 <- parallel_4(fit = B4_empty, data = B4_data)

# B5
vars_5 <- c('mean_banking_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3', 'pres_party', 'house_dem_rep', 'senate_dem_rep')
B5_data <- stan_lister(base = base, df = main, vars = vars_5)

B5_empty <- stan(file = speeches_code, data = B5_data, chains = 0)
B5 <- parallel_4(fit = B5_empty, data = B5_data)

# ---------------------------- Output ---------------------------------------- #

#### Table results ####
banking_table <- stan_speeches_param_est(
    stanfit = list(B1 = B1,
                   B2 = B2,
                   B3 = B3,
                   B4 = B4,
                   B5 = B5),
    pars_labels = list(
        B1 = c('M. Banking Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'High Scrutiny', 'Intercept'),
        B2 = c('M. Banking Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'Case-Shiller Change m-3',
               'Intercept'),
        B3 = c('M. Banking Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'Case-Shiller Change m-3',
               'Inflation m-3', 'Intercept'),
        B3 = c('M. Banking Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'Case-Shiller Change m-3',
               'Unemployment Change m-3', 'Intercept'),
        B5 = c('M. Banking Topic y-1', 'Fed. Venue',
               'HCFS Donor', 'High Scrutiny', 'Pres. Party',
               'House Dem. Prop.', 'Senate Dem. Prop.',
               'Intercept')
    ),
    obs = nrow(main))

var_order <- c('M. Banking Topic y-1', 'M. Banking Topic y-1_ci',
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

banking_table <- full_join(banking_table, vars_order, by = 'Parameters') %>%
    arrange(num_order) %>% select(-num_order)

banking_table$Parameters <- gsub('.*_ci$', '', banking_table$Parameters)
names(banking_table) <- c('', names(banking_table)[2:length(banking_table)])

#stargazer(banking_table, summary = F, out = 'tables/banking.tex',
#          out.header = F, rownames = F,
#          title = 'Logistic Regression Coefficient Estimates from the Posterior Distribution for Discussing Banking Regulation',
#          label = 'bankingTable',
#          notes = c('Posterior distribution medians, with 95\\% credible intervals in parentheses.', 'Speaker varying-intercepts not shown. Please see Figure \\\\ref{speakerBanking} in the Appendix.'),
#          font.size = 'small')


##### Speaker effect plot #####
stan_caterpillar(B1, '^a\\[.*\\]', full_names) +
    geom_vline(xintercept = 0, linetype = 'dotted')

#ggsave(file = 'figures/banking_speakers.pdf')
