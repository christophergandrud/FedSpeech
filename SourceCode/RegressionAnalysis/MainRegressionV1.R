#############
# Main Regression Analysis (v1)
# Christopher Gandrud
# 9 August 2013
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
MA6 <- zelig(FedBoardCentrality ~ CaseShillerChangeLag3 + 
            pres_party + house_dem_rep + senate_dem_rep, 
            data = Combined, model = "ls", robust = "month_year", 
            cite = FALSE)
# Test For Donors
MA7 <- zelig(HFSC_CombConnect ~ ScrutinyLag3, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)
MA8 <- zelig(HFSC_CombConnect ~ CaseShillerChangeLag3 + PCEPIPercentLag3, data = Combined, 
            model = "ls", robust = "month_year", cite = FALSE)


# Create results table
CoVarLabs1 <- c("Scrutiny Med.", "Scrutiny High", "Case-Shiller Change", "Inflation", 
                "Unemployment Change", "Growth", "Pres. Party", "House Dem Prop.", "Senate Dem Prop.")
ColLabs1 <- c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8")
Centrality <- stargazer(MA1, MA2, MA3, MA4, MA5, MA6, MA7, MA8,
                title = "Coefficient Estimates for Organizations' Connectivity to the Fed Board and Congress",
                label = "CentTable",
                dep.var.labels = c("Fed Board Centrality", "HCFS Donor"),
                column.labels = ColLabs1,
                covariate.labels = CoVarLabs1,
                notes = c("Robust standard errors clustered by month-years in parentheses.",
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

CoVarLabs2 <- c("Scrutiny Med.", "Scrutiny High", 
                "Case-Shiller Change")
ColLabs2 <- c("B1", "B2", "B3", "B4")
OrgType <- stargazer(MB1, MB2, MB3, MB4,
              title = "Coefficient Estimates for Organizations' Type",
              label = "OrgTypeTable",
              dep.var.labels = c("Fed. Venue", "University"),
              column.labels = ColLabs2,
              covariate.labels = CoVarLabs2,
              notes = c("Robust standard errors clustered by", 
                        "month-years in parentheses.",
                        "All stressors are lagged by 3 months."),
              notes.align = "l",
              header = FALSE,
              digits = 2,
              font.size = "small",
              omit.stat = c("f", "SER"))
cat(OrgType, file = "~/Dropbox/Fed_Speeches_Paper/tables/OrgType.tex")


# Combined$Elite <- Combined$FedSpoketoFed + Combined$university
# Combined$Elite[Combined$Elite == 2] <- 1

#### Topics ####
# Multiply topic vars by 100 to ease interpretation
TopicVars <- c("Financial.Markets", "Macroeconomics", "Monetary.Policy", "International.Economy",
                "Local.Housing.Dev", "Banking.Regulation", "Pres. Party", "House Dem Prop.", "Senate Dem Prop.")
for (u in TopicVars){
    Combined[, u] <- Combined[, u] * 100
}
+ 
            pres_party + house_dem_rep + senate_dem_rep
# Monetary Policy
MC1 <- zelig(Monetary.Policy ~ FedSpoketoFed + ScrutinyLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC2 <- zelig(Monetary.Policy ~ FedSpoketoFed + CaseShillerChangeLag3, 
             data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC3 <- zelig(Monetary.Policy ~ FedSpoketoFed + CaseShillerChangeLag3 +
             PCEPIPercentLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC4 <- zelig(Monetary.Policy ~ FedSpoketoFed + CaseShillerChangeLag3 +
            UnemploymentRateChangeLag3, 
            data = Combined, model = "ls", robust = "month_year", cite = FALSE)
MC5 <- zelig(Monetary.Policy ~ FedSpoketoFed + PCEPIPercentLag3 + 
               pres_party + house_dem_rep + senate_dem_rep, 
            data = Combined, model = "ls", robust = "month_year", cite = FALSE)

# Local Housing and Development
MC6 <- zelig(Local.Housing.Dev ~ ScrutinyLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC7 <- zelig(Local.Housing.Dev ~ CaseShillerChangeLag3, 
             data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC8 <- zelig(Local.Housing.Dev ~ CaseShillerChangeLag3 +PCEPIPercentLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC9 <- zelig(Local.Housing.Dev ~ CaseShillerChangeLag3 + 
             UnemploymentRateChangeLag3 + pres_party, 
             data = Combined, model = "ls", robust = "month_year", cite = FALSE)
MC10 <- zelig(Local.Housing.Dev ~ ScrutinyLag3 + pres_party + 
             house_dem_rep + senate_dem_rep, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)

# Financial Markets
MC11 <- zelig(Financial.Markets ~ HFSC_CombConnect + ScrutinyLag3,
             data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC12 <- zelig(Financial.Markets ~ CaseShillerChangeLag3, 
             data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC13 <- zelig(Financial.Markets ~ CaseShillerChangeLag3 +
             PCEPIPercentLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC14 <- zelig(Financial.Markets ~ CaseShillerChangeLag3 + 
            UnemploymentRateChangeLag3, 
            data = Combined, model = "ls", robust = "month_year", cite = FALSE)
MC15 <- zelig(Financial.Markets ~ HFSC_CombConnect + ScrutinyLag3+ 
             pres_party + house_dem_rep + senate_dem_rep,
             data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)

# Banking Regulation
MC16 <- zelig(Banking.Regulation ~ HFSC_CombConnect + ScrutinyLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC17 <- zelig(Banking.Regulation ~ CaseShillerChangeLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC18 <- zelig(Banking.Regulation ~ CaseShillerChangeLag3 + 
             PCEPIPercentLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC19 <- zelig(Banking.Regulation ~ CaseShillerChangeLag3 + 
             UnemploymentRateChangeLag3, data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)
MC20 <- zelig(Banking.Regulation ~ HFSC_CombConnect + ScrutinyLag3 +
             pres_party + house_dem_rep + senate_dem_rep, 
             data = Combined, 
             model = "ls", robust = "month_year", cite = FALSE)

# Create results table
CovarLabs3 <- c("Fed. Venue", "Scrutiny Med.", "Scrutiny High", 
                "Case-Shiller Change", "Inflation", "Unemploy. Change",
                "Pres. Party", "House Dem Prop.", "Senate Dem Prop.")
ColLabs3 <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", 
              "C10")
Topics1 <- stargazer(MC1, MC2, MC3, MC4, MC5, MC6, MC7, MC8, MC9, MC10,
            title = "Coefficient Estimates for Topics Discussed in Speeches (1)", 
            label = "TopicsRegress1",
            covariate.labels = CovarLabs3,
            column.labels = ColLabs3,
            dep.var.labels = c("Monetary Policy", "Local Housing and Development"),
            notes = c("Robust standard errors clustered by month-years in parentheses.",
                "All stressors are lagged by 3 months."),
            notes.align = "l",
            header = FALSE,
            digits = 2,
            font.size = "scriptsize",
            column.sep.width = "0.01cm",
            omit.stat = c("f", "SER"))
cat(Topics1, file = "~/Dropbox/Fed_Speeches_Paper/tables/TopicRegress1.tex")

CovarLabs4 <- c("HCFS Donor", "Scrutiny Med.", "Scrutiny High", 
                "Case-Shiller Change", "Inflation", "Unemploy. Change")
ColLabs4 <- c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "
              D10")
Topics2 <- stargazer(MC11, MC12, MC13, MC14, MC15, MC16, 
            MC17, MC18, MC19, MC20,
            title = "Coefficient Estimates for Topics Discussed in Speeches (2)", 
            label = "TopicsRegress2",
            covariate.labels = CovarLabs4,
            column.labels = ColLabs4,
            dep.var.labels = c("Financial Markets", "Banking Regulation"),
            notes = c("Robust standard errors clustered by month-years in parentheses.",
                "All stressors are lagged by 3 months."),
            notes.align = "l",
            header = FALSE,
            digits = 2,
            font.size = "scriptsize",
            column.sep.width = "0.01cm",
            omit.stat = c("f", "SER"))
cat(Topics2, file = "~/Dropbox/Fed_Speeches_Paper/tables/TopicRegress2.tex")

