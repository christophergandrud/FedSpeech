
# Load required packages
library(rstan)

# Load basic data
# Generate data frame Combined
source("~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/DataCleanMerge/MergeForRegressions.R")

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

### GETzoibPost (extract posterior distribution)
GetzoibPost <- function(obj, max){
    post.sample <- obj$oripara
    sample.c1<- post.sample[[1]][1:max,]
    sample.c2<- post.sample[[2]][1:max,]
    sample12 <- mcmc.list(as.mcmc(sample.c1),as.mcmc(sample.c2))   
    return(sample12)
}

# Financial Markets
FM1 <- zoib(Financial.Markets ~ CaseShillerChange + UnemploymentRateChange|1|CaseShillerChange + UnemploymentRateChange|1,
              data = Combined, EUID = Combined$month_year, random = 1,
              one.inflation = FALSE, joint = FALSE, 
              n.iter = 500)

FM1_post <- GetzoibPost(FM1, max = 250)
gelman.diag(FM1_post)
summary(FM1_post)


# Financial Markets
Test2 <- zoib(Financial.Markets ~ CaseShillerChange + UnemploymentRateChange|1|CaseShillerChange + UnemploymentRateChange|1,
             data = Combined, EUID = Combined$month_year, random = 1,
             one.inflation = FALSE, joint = FALSE, 
             n.iter = 500)

FM1_post <- GetzoibPost(FM1, max = 250)
gelman.diag(FM1_post)
summary(FM1_post)
gelman.diag(sample12)

