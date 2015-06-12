# ---------------------------------------------------------------------------- #
# Presidential/Congression interactions
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

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
library(DataCombine)
library(rstan)
library(stargazer)
library(ggplot2)

# Model
speeches_code <- 'speech_topic.stan'

# Load data
main <- import('combined_data.csv')

## Create interactions
main$pres_house <- main$pres_party * main$house_dem_rep
main$pres_senate <- main$pres_party * main$senate_dem_rep

# --------------------- Monetary Policy -------------------------------------- #
# Keep complete cases
covars_all <- c('mean_monetary_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
                'ScrutinyLag3',
                'CaseShillerChangeLag3', 'PCEPIPercentLag3',
                'UnemploymentRateChangeLag3', 'pres_party', 'house_dem_rep',
                'senate_dem_rep', 'pres_house', 'pres_senate')
mp <- main %>% DropNA(covars_all)


## Convert factor variables to numeric
mp$name_num <- mp$name %>% as.factor %>% as.numeric
full_names <- unique(mp$name)

# Data descriptions
N_names <- max(mp$name_num)

# ---------------------------- MP Model--------------------------------------- #
# Data
base <- list(
    N = nrow(mp),
    S = N_names,
    speaker = mp$name_num,
    ## Outcome
    y = mp$Monetary.Policy_dummy
)

vars_mp <- c('mean_monetary_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3', 'pres_party', 'house_dem_rep', 'senate_dem_rep',
            'pres_house', 'pres_senate')
mp_data <- stan_lister(base = base, df = mp, vars = vars_mp)

mp_empty <- stan(file = speeches_code, data = mp_data, chains = 0)
mp <- parallel_4(fit = mp_empty, data = mp_data)

# --------------------------- Financial Markets------------------------------- #

# Keep complete cases
covars_all <- c('mean_financial_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
                'ScrutinyLag3', 'CaseShillerChangeLag3', 'PCEPIPercentLag3',
                'UnemploymentRateChangeLag3', 'pres_party', 'house_dem_rep',
                'senate_dem_rep', 'pres_house', 'pres_senate')
fm <- main %>% DropNA(covars_all)

## Convert factor variables to numeric
fm$name_num <- fm$name %>% as.factor %>% as.numeric
full_names <- unique(fm$name)

# Data descriptions
N_names <- max(fm$name_num)

# ---------------------------- FM Model -------------------------------------- #
# Data
base <- list(
    N = nrow(fm),
    S = N_names,
    speaker = fm$name_num,
    ## Outcome
    y = fm$Financial.Markets_dummy
)

vars_fm <- c('mean_financial_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3', 'pres_party', 'house_dem_rep', 'senate_dem_rep',
            'pres_house', 'pres_senate')
fm_data <- stan_lister(base = base, df = fm, vars = vars_fm)

fm_empty <- stan(file = speeches_code, data = fm_data, chains = 0)
fm <- parallel_4(fit = fm_empty, data = fm_data)

# --------------------------- Banking Regulation ----------------------------- #

# Keep complete cases
covars_all <- c('mean_banking_lag_y1', 'FedSpoketoFed',
                'HFSC_CombConnect', 'ScrutinyLag3',
                'CaseShillerChangeLag3', 'PCEPIPercentLag3',
                'UnemploymentRateChangeLag3', 'pres_party', 'house_dem_rep',
                'senate_dem_rep', 'pres_house', 'pres_senate')
br <- main %>% DropNA(covars_all)

## Convert factor variables to numeric
br$name_num <- br$name %>% as.factor %>% as.numeric
full_names <- unique(br$name)

# Data descriptions
N_names <- max(br$name_num)

# ---------------------------- BR Model -------------------------------------- #
# Data
base <- list(
    N = nrow(br),
    S = N_names,
    speaker = br$name_num,
    ## Outcome
    y = br$Banking.Regulation_dummy
)

vars_br <- c('mean_banking_lag_y1', 'FedSpoketoFed', 'HFSC_CombConnect',
            'ScrutinyLag3', 'pres_party', 'house_dem_rep', 'senate_dem_rep',
            'pres_house', 'pres_senate')
br_data <- stan_lister(base = base, df = br, vars = vars_br)

br_empty <- stan(file = speeches_code, data = br_data, chains = 0)
br <- parallel_4(fit = br_empty, data = br_data)

# ---------------------------- Create Output Table --------------------------- #
#### Table results ####
banking_table <- stan_speeches_param_est(
                    stanfit = list(I1 = mp,
                                   I2 = fm,
                                   I3 = br),
                    pars_labels = list(
                        I1 = c('M. Monetary Topic y-1', 'Fed. Venue',
                              'HCFS Donor', 'High Scrutiny', 'Pres. Party',
                              'House Dem. Prop.', 'Senate Dem. Prop.',
                              'Pres. Party*House', 'Pres. Party*Senate',
                              'Intercept'),
                        I2 = c('M. Financial Topic y-1', 'Fed. Venue',
                               'HCFS Donor', 'High Scrutiny', 'Pres. Party',
                               'House Dem. Prop.', 'Senate Dem. Prop.',
                               'Pres. Party*House', 'Pres. Party*Senate',
                               'Intercept'),
                        I3 = c('M. Banking Topic y-1', 'Fed. Venue',
                               'HCFS Donor', 'High Scrutiny', 'Pres. Party',
                               'House Dem. Prop.', 'Senate Dem. Prop.',
                               'Pres. Party*House', 'Pres. Party*Senate',
                               'Intercept')
                        ),
                    obs = nrow(main %>% DropNA(covars_all)))

var_order <- c('M. Monetary Topic y-1', 'M. Monetary Topic y-1_ci',
                'M. Financial Topic y-1', 'M. Financial Topic y-1_ci',
                'M. Banking Topic y-1', 'M. Banking Topic y-1_ci',
                'Fed. Venue', 'Fed. Venue_ci', 'HCFS Donor', 'HCFS Donor_ci',
                'High Scrutiny', 'High Scrutiny_ci',
                'Pres. Party', 'Pres. Party_ci', 'House Dem. Prop.',
                'House Dem. Prop._ci', 'Senate Dem. Prop.',
                'Senate Dem. Prop._ci',
                'Pres. Party*House', 'Pres. Party*House_ci',
                'Pres. Party*Senate', 'Pres. Party*Senate_ci',
                'Intercept', 'Intercept_ci',
                'Obs.', 'WAIC')

vars_order <- data.frame(Parameters = var_order,
                         num_order = 1:length(var_order))

banking_table <- full_join(banking_table, vars_order, by = 'Parameters') %>%
                    arrange(num_order) %>% select(-num_order)

banking_table$Parameters <- gsub('.*_ci$', '', banking_table$Parameters)
names(banking_table) <- c('', names(banking_table)[2:length(banking_table)])

setwd('~/Dropbox/Fed_Speeches_Paper/JoPP/R1')

stargazer(banking_table, summary = F, out = 'tables/pres_cong_interactions.tex',
          out.header = F, rownames = F,
          title = 'Logistic Regression Coefficient Estimates from the Posterior Distribution for Topics with Interactions',
          column.labels = c('Monetary Policy', 'Financial Markets',
                            'Banking Regulation'),
          column.separate = c(1, 2, 3),
          label = 'interactionsTable',
          notes = c('Posterior distribution medians, with 95\\% credible intervals in parentheses.', 'Speaker varying-intercepts not shown.'),
          font.size = 'small')
