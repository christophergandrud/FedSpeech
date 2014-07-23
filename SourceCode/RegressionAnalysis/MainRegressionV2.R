# ---------------------------------------------------------------------------- #
# Zero inflated beta regressions for Fed Speeches
# Christopher Gandrud
# 23 July 2014
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

Combined$ScrutinyLag3 <- factor(Combined$ScrutinyLag3,
                             labels = c("Low", "Medium", "High"))

# ---------------------------------------------------------------------------- #
#### Zero inflated beta regression ####

# Banking Policy Topic
BP1 <- zoib(Monetary.Policy ~ FedSpoketoFed +
                HFSC_CombConnect + ScrutinyLag3|1| 
                FedSpoketoFed + HFSC_CombConnect +
                ScrutinyLag3|1,
              data = Combined, EUID = Combined$month_year, random = 1,
              one.inflation = FALSE, joint = FALSE, 
              n.iter = 500)

BP1_post <- GetzoibPost(BP1, max = 250)
gelman.diag(BP1_post)
summary(BP1_post)


# Financial Markets
FM1 <- zoib(Financial.Markets ~ CaseShillerChange + UnemploymentRateChange|1|
                CaseShillerChange + UnemploymentRateChange|1,
             data = Combined, EUID = Combined$month_year, random = 1,
             one.inflation = FALSE, joint = FALSE, 
             n.iter = 500)

FM1_post <- GetzoibPost(FM1, max = 250)
gelman.diag(FM1_post)
summary(FM1_post)
gelman.diag(sample12)

