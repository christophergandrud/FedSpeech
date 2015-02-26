# ---------------------------------------------------------------------------- #
# Zero inflated beta regressions for Fed Speeches
# Christopher Gandrud
# 8 August 2014
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(zoib)
library(zibHelpers) # devtools::install_github('christophergandrud/zibHelpers')
library(gridExtra)

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

# ---------------------------------------------------------------------------- #
#### Zero inflated beta regression ####

# Set the number of iterations
nIter = 3000

# Monetary Policy Topic ------------------------------------------------------ #
# Scrutiny
MP1 <- zoib(Monetary.Policy ~
        FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3 + PCEPIPercentLag3|1|
        FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3 + PCEPIPercentLag3|1,
        data = Combined, EUID = Combined$month_year, random = 1,
        one.inflation = FALSE, joint = FALSE, n.iter = nIter)

## Numerical Summaries/Diagnostics
MP1_G = GelmanDiag(MP1, iter = nIter)
SummaryZib(MP1, iter = nIter)

# Plot
vl_MP1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny High', 'Inflation')
MP_plot1 <- zibPlot(MP1, iter = nIter, variable_names = vl_MP1,
                    title = 'Monetary Policy\n')

pdf(file = '~/Dropbox/Fed_Speeches_Paper/ZOIBFigures/MonetaryPolicy.pdf')
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
HD1_G = GelmanDiag(HD1, iter = nIter)
SummaryZib(HD1, iter = nIter)

HD2_G = GelmanDiag(HD2, iter = nIter)
SummaryZib(HD2, iter = nIter)

# Plot
vl_HD1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny High')
HD_plot1 <- zibPlot(HD1, iter = nIter, variable_names = vl_HD1,
                    title = 'Local Housing & Developement\n', xlab = '')

vl_HD2 <- c('Fed Venue', 'HCFS Donor', 'Case Shiller Change')
HD_plot2 <- zibPlot(HD2, iter = nIter, variable_names = vl_HD2)

pdf(file = '~/Dropbox/Fed_Speeches_Paper/ZOIBFigures/HousingDev.pdf')
    grid.arrange(HD_plot1, HD_plot2, nrow = 2)
dev.off()

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

## Numerical Summaries/Diagnostics
FM1_G = GelmanDiag(FM1, iter = nIter)
SummaryZib(FM1, iter = nIter)

FM2_G = GelmanDiag(FM2, iter = nIter)
SummaryZib(FM2, iter = nIter)

# Plot
vl_FM1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny High')
FM_plot1 <- zibPlot(FM1, iter = nIter, variable_names = vl_FM1,
                    title = 'Financial Markets\n', xlab = '')

vl_FM2 <- c('Fed Venue', 'HCFS Donor', 'Case Shiller Change')
FM_plot2 <- zibPlot(FM2, iter = nIter, variable_names = vl_FM2)

pdf(file = '~/Dropbox/Fed_Speeches_Paper/ZOIBFigures/FinancialMarkets.pdf')
    grid.arrange(FM_plot1, FM_plot2, nrow = 2)
dev.off()

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

## Numerical Summaries/Diagnostics
BR1_G = GelmanDiag(BR1, iter = nIter)
SummaryZib(BR1, iter = nIter)

BR2_G = GelmanDiag(BR2, iter = nIter)
SummaryZib(BR2, iter = nIter)

# Plot
vl_BR1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny High')
BR_plot1 <- zibPlot(BR1, iter = nIter, variable_names = vl_BR1,
                    title = 'Banking Regulation\n', xlab = '')

vl_BR2 <- c('Fed Venue', 'HCFS Donor', 'Case Shiller Change')
BR_plot2 <- zibPlot(BR2, iter = nIter, variable_names = vl_BR2)

pdf(file = '~/Dropbox/Fed_Speeches_Paper/ZOIBFigures/BankingRegulation.pdf')
    grid.arrange(BR_plot1, BR_plot2, nrow = 2)
dev.off()


# Combine Gelman-Ruban Diagnostics
GR <- rbind(MP1_G, HD1_G, HD2_G, FM1_G, FM2_G, BR1_G, BR2_G)
write.csv(GR, 
    file = '~/Dropbox/Fed_Speeches_Paper/ZOIBFigures/GelmanRubinDiagDump.csv')
