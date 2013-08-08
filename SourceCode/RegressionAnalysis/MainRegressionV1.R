#############
# Main Regression Analysis (v1)
# Christopher Gandrud
# 8 August 2013
#############

# Load packages
library(Zelig)
library(DataCombine)
library(stargazer)

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

#### Connections
# Multiple FedBoardCentrality by 100
Combined$FedBoardCentrality <- Combined$FedBoardCentrality * 1000

# Test Models for FedBoardCentrality
MA1 <- zelig(FedBoardCentrality ~ ScrutinyLag3, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
MA2 <- zelig(FedBoardCentrality ~ CaseShillerChangeLag3 + PCEPIPercentLag3, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
MA3 <- zelig(FedBoardCentrality ~ CaseShillerChangeLag3 + UnemploymentRateChangeLag3, 
            data = Combined, model = "ls", robust = "month_year", 
            cite = FALSE)
MA4 <- zelig(FedBoardCentrality ~ CaseShillerChangeLag3 + GDPC96Percent, 
            data = Combined, model = "ls", robust = "month_year", 
            cite = FALSE)
MA5 <- zelig(FedBoardCentrality ~ ScrutinyLag3 + pres_party + house_dem_rep + 
              senate_dem_rep, data = Combined, model = "ls", robust = "month_year", 
            cite = FALSE)
MA6 <- zelig(FedBoardCentrality ~ CaseShillerChangeLag3 + pres_party + house_dem_rep +
            senate_dem_rep, 
            data = Combined, model = "ls", robust = "month_year", 
            cite = FALSE)
# Test For Donors
MA7 <- zelig(HFSC_CombConnect ~ ScrutinyLag3, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
MA8 <- zelig(HFSC_CombConnect ~ CaseShillerChangeLag3 + PCEPIPercentLag3, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)


# Create results table
CoVarLabs1 <- c("Scrutiny Med", "Scrutiny High", "Case-Shiller", "Inflation", 
                "Unemployment Change", "Growth", "Pres. Party", "House Dem Prop.", "Senate Dem Prop.")
ColLabs1 <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8")
Centrality <- stargazer(MA1, MA2, MA3, MA4, MA5, MA6, MA7, MA8,
                      title = "Coefficient Estimates for Organizations' Connectivity to the Fed Board and Congress",
                      label = "CentTable",
                      dep.var.labels = c("Fed Board Centrality", "HCFS Donor"),
                      column.labels = ColLabs1,
                      covariate.labels = CoVarLabs1,
                      notes = c("Robust standard errors clustered by month in parentheses.",
                                "All economic stressor and scrutiny covariates are lagged by 3 months.",
                                "Fed Board Centrality was multipled by 1000 to ease interpretation."),
                      notes.align = "l",
                      header = FALSE,
                      digits = 2,
                      font.size = "scriptsize",
                      column.sep.width = "0.01cm",
                      omit.stat = c("f", "SER"))
cat(Centrality, file = "~/Dropbox/Fed_Speeches_Paper/tables/FedCentrality.tex")

#### Org. Type ####
MB1 <- zelig(FedSpoketoFed ~ ScrutinyLag3, data = Combined, 
               model = "ls", robust = "month_year", cite = FALSE)

MB2 <- zelig(FedSpoketoFed ~ CaseShillerChangeLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)

MB3 <- zelig(university ~ ScrutinyLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)

MB4 <- zelig(university ~ CaseShillerChangeLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)

CoVarLabs2 <- c("Scrutiny Med", "Scrutiny High", "Case-Shiller")
ColLabs2 <- c("B1", "B2", "B3", "B4")
OrgType <- stargazer(MB1, MB2, MB3, MB4,
                      title = "Coefficient Estimates for Organizations' Type",
                      label = "OrgTypeTable",
                      dep.var.labels = c("Federal Reserve", "University"),
                      column.labels = ColLabs2,
                      covariate.labels = CoVarLabs2,
                      notes = c("Robust standard errors clustered by", 
                                "month in parentheses.",
                                "All stressor are lagged by 3 months."),
                      notes.align = "l",
                      header = FALSE,
                      digits = 2,
                      font.size = "small",
                      #column.sep.width = "0.1cm",
                      omit.stat = c("f", "SER"))
cat(OrgType, file = "~/Dropbox/Fed_Speeches_Paper/tables/OrgType.tex")


# Combined$Elite <- Combined$FedSpoketoFed + Combined$university
# Combined$Elite[Combined$Elite == 2] <- 1