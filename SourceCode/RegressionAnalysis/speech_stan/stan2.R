# ---------------------------------------------------------------------------- #
# Stan Speeches-Topics Regression where Monetary Policy is the Topic
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
library(xtable)
library(ggplot2)

# Load data
main <- read.csv('combined_data.csv', stringsAsFactors = F)

# --------------------------- Final cleaning --------------------------------- #

# Keep complete cases
covars <- c('HFSC_CombConnect', 'FedSpoketoFed', 'ScrutinyLag3')
main <- main %>% DropNA(covars)

## Convert factor variables to numeric
main$name_num <- main$name %>% as.factor %>% as.numeric
full_names <- unique(main$name)

# Data descriptions
N_names <- max(main$name_num)
