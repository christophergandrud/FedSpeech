# ---------------------------------------------------------------------------- #
# Zero inflated beta regressions for Fed Speeches
# Christopher Gandrud
# 25 July 2014
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(zoib)

# Functions
## GETzoibPost (extract posterior distribution)
GetzoibPost <- function(obj, max){
    post.sample <- obj$oripara
    sample.c1<- post.sample[[1]][1:max,]
    sample.c2<- post.sample[[2]][1:max,]
    sample12 <- mcmc.list(as.mcmc(sample.c1),as.mcmc(sample.c2))
    return(sample12)
}

# ---------------------------------------------------------------------------- #
#### Get Data ####
# Generate data frame Combined
setwd('~/Dropbox/Fed_Speeches_Paper/')
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
nIter = 1000

# Monetary Policy Topic ------------------------------------------------------ #
# Scrutiny
MP1 <- zoib(Monetary.Policy ~
            FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3|1|
            FedSpoketoFed + HFSC_CombConnect + ScrutinyLag3|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

# Mandate
MP2 <- zoib(Monetary.Policy ~
            FedSpoketoFed + HFSC_CombConnect + PCEPIPercentLag3|1|
            FedSpoketoFed + HFSC_CombConnect + PCEPIPercentLag3|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

## Numerical Summaries/Diagnostics
MP1_post <- GetzoibPost(MP1, max = nIter/2)
gelman.diag(MP1_post)
summary(MP1_post)

MP2_post <- GetzoibPost(MP2, max = nIter/2)
gelman.diag(MP2_post)
summary(MP2_post)

# Plots
vl_MP1 <- c('Fed Venue', 'HCFS Donor', 'Scrutiny Med.', 'Scrutiny High')
zibPlot(MP1, max = nIter/2, variable_names = vl_MP1)

vl_MP2 = c('Fed Venue', 'HCFS Donor', 'Inflation')
zibPlot(MP2, max = nIter/2, variable_names = vl_MP2)




# Housing and Development Topic ---------------------------------------------- #
HD1 <- zoib(Local.Housing.Dev ~
            CaseShillerChange|1|
            CaseShillerChange|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

HD1_post <- GetzoibPost(HD1, max = nIter/2)
gelman.diag(HD1_post)
summary(HD1_post)

HD2 <- zoib(Local.Housing.Dev ~
            ScrutinyLag3|1|
            ScrutinyLag3|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

HD2_post <- GetzoibPost(HD2, max = nIter/2)
gelman.diag(HD2_post)
zibPlot(HD2, max = nIter/2)

# Financial Markets ---------------------------------------------------------- #
FM1 <- zoib(Financial.Markets ~
            CaseShillerChange + ScrutinyLag3|1|
            CaseShillerChange + ScrutinyLag3|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

FM1_post <- GetzoibPost(FM1, max = nIter/2)
gelman.diag(FM1_post)
summary(FM1_post)


# Banking Regulation Topic --------------------------------------------------- #
BR1 <- zoib(Banking.Regulation ~
            HFSC_CombConnect + ScrutinyLag3|1|
            HFSC_CombConnect + ScrutinyLag3|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE, n.iter = nIter)

BR1_post <- GetzoibPost(BR1, max = nIter/2)
gelman.diag(BR1_post)
summary(BR1_post)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Degree of quantification
QT1 <- zoib(quanty ~ CaseShillerChange|1|CaseShillerChange|1,
            data = Combined, EUID = Combined$month_year, random = 1,
            one.inflation = FALSE, joint = FALSE,  n.iter = nIter)

QT1_post <- GetzoibPost(QT1, max = nIter/2)
gelman.diag(QT1_post)
summary(QT1_post)
