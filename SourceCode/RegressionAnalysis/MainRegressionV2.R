# ---------------------------------------------------------------------------- #
# Zero inflated beta regressions for Fed Speeches
# Christopher Gandrud
# 25 July 2014
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(zoib)
library(gridExtra)

# Set working directory
setwd('~/Dropbox/Fed_Speeches_Paper/')

# Functions
source('FedSpeech/SourceCode/RegressionAnalysis/zibPlot.R')

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
                             labels = c("Low", "Medium", "High"))

# Rescale quanty to a proportion from a percent
Combined$quanty <- Combined$quanty/100

# ---------------------------------------------------------------------------- #
#### Zero inflated beta regression ####

# Set the number of iterations
nIter = 100

# Monetary Policy Topic ------------------------------------------------------ #
# Scrutiny
MP1 <- zoib(Monetary.Policy ~
        FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3 + PCEPIPercentLag3|1|
        FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3 + PCEPIPercentLag3|1,
        data = Combined, EUID = Combined$month_year, random = 1,
        one.inflation = FALSE, joint = FALSE, n.iter = nIter)

## Numerical Summaries/Diagnostics
MP1_G = GelmanDiag(MP1_post, iter = nIter)
SummaryZib(MP1, iter = nIter)

# Plot
vl_MP1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny Med.', 'Scrutiny High',
            'Inflation')
MP_plot1 <- zibPlot(MP1, max = nIter/2, variable_names = vl_MP1,
                    title = 'Monetary Policy\n')

pdf(file = 'ZOIBFigures/BankingPolicy.pdf')
    MP_plot1
dev.off()

# Housing and Development Topic ---------------------------------------------- #
HD1 <- zoib(Local.Housing.Dev ~
            FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3|1|
            FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

HD2 <- zoib(Local.Housing.Dev ~
            FedSpoketoFed + HFSC_CombConnect + CaseShillerChange|1|
            FedSpoketoFed + HFSC_CombConnect + CaseShillerChange|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

## Numerical Summaries/Diagnostics
HD1_post <- GetzibPost(HD1, max = nIter/2)
gelman.diag(HD1_post)
summary(HD1_post)

HD2_post <- GetzibPost(HD2, max = nIter/2)
gelman.diag(HD2_post)
summary(HD2_post)

# Plot
vl_HD1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny Med.', 'Scrutiny High')
HD_plot1 <- zibPlot(HD1, max = nIter/2, variable_names = vl_HD1,
                    title = 'Local Housing & Developement\n', xlab = '')

vl_HD2 <- c('Fed Venue', 'HCFS Donor', 'Case Shiller Change')
HD_plot2 <- zibPlot(HD2, max = nIter/2, variable_names = vl_HD2)

grid.arrange(HD_plot1, HD_plot2, nrow = 2)

# Financial Markets ---------------------------------------------------------- #
FM1 <- zoib(Financial.Markets ~
            FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3|1|
            FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

FM2 <- zoib(Financial.Markets ~
            FedSpoketoFed + HFSC_CombConnect + CaseShillerChange|1|
            FedSpoketoFed + HFSC_CombConnect + CaseShillerChange|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

FM1_post <- GetzibPost(FM1, max = nIter/2)
gelman.diag(FM1_post)
summary(FM1_post)

FM2_post <- GetzibPost(FM2, max = nIter/2)
gelman.diag(FM2_post)
summary(FM2_post)

# Plot
vl_FM1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny Med.', 'Scrutiny High')
FM_plot1 <- zibPlot(FM1, max = nIter/2, variable_names = vl_FM1,
                    title = 'Financial Markets\n', xlab = '')

vl_FM2 <- c('Fed Venue', 'HCFS Donor', 'Case Shiller Change')
FM_plot2 <- zibPlot(FM2, max = nIter/2, variable_names = vl_FM2)

grid.arrange(FM_plot1, FM_plot2, nrow = 2)

# Banking Regulation Topic --------------------------------------------------- #
BR1 <- zoib(Banking.Regulation ~
            FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3|1|
            FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

BR2 <- zoib(Banking.Regulation ~
            FedSpoketoFed + HFSC_CombConnect + CaseShillerChange|1|
            FedSpoketoFed + HFSC_CombConnect + CaseShillerChange|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

BR1_post <- GetzibPost(BR1, max = nIter/2)
gelman.diag(BR1_post)
summary(BR1_post)

FM2_post <- GetzibPost(FM2, max = nIter/2)
gelman.diag(FM2_post)
summary(FM2_post)

# Plot
vl_BR1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny Med.', 'Scrutiny High')
BR_plot1 <- zibPlot(BR1, max = nIter/2, variable_names = vl_BR1,
                    title = 'Banking Regulation\n', xlab = '')

vl_BR2 <- c('Fed Venue', 'HCFS Donor', 'Case Shiller Change')
BR_plot2 <- zibPlot(BR2, max = nIter/2, variable_names = vl_BR2)

grid.arrange(BR_plot1, BR_plot2, nrow = 2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Degree of quantification
QT1 <- zoib(quanty ~ CaseShillerChange|1|CaseShillerChange|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE,  n.iter = nIter)

QT1_post <- GetzibPost(QT1, max = nIter/2)
gelman.diag(QT1_post)
summary(QT1_post)
