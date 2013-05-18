##############
# Download econ data and save as monthly data file.
# Christopher Gandrud
# 18 May 2013
##############

# Packages 
library(quantmod)
library(xts)
library(plyr)
library(DataCombine)
library(xtable)

# Download data
## CPIAUCNS = Consumer Price Index for All Urban Consumers: All Items
## DFF = Effective Federal Funds Rate (daily)
## FEDFUNDS = Effective Federal Funds Rate (monthly)
## GDPDEF = Gross Domestic Product: Implicit Price Deflator
## GDPC96 = Real Gross Domestic Product, 3 Decimal
## U6RATE = Total unemployed, plus all marginally attached workers plus total employed part time for economic reasons
##
##

Symbols <- c("CPIAUCNS", "DFF", "FEDFUNDS", "GDPDEF", "GDPC96", "U6RATE")
getSymbols(Symbols, src = "FRED")

# Convert to data frames

# Function partially based on: http://stackoverflow.com/questions/4368861/r-converting-xts-or-zoo-object-to-a-data-frame

ToDF <- function(x){
  
  getSymbols(x, src = "FRED")
  
  First <- x[1]
  for (i in x){
    TempDF <- get(i)
    if (i == First){ 
      mthlySumm <- apply.monthly(TempDF, mean)
      DateField <- index(TempDF)
      TempData <- data.frame(DateField, TempDF)
    }
    else if (i != First){
      mthlySumm <- apply.monthly(TempDF, mean)
      DateField <- index(TempDF)
      TempDataMore <- data.frame(DateField, TempDF)
      TempData <- merge(TempData, TempDataMore, by = "DateField", all = TRUE)  
    }
  }
  TempData
}

CombinedEcon <- ToDF(Symbols)

# Keep the first day of the month
CombinedEcon$First <- grepl("-01$", as.character(CombinedEcon[, 1]))
CombinedEconSlim <- subset(CombinedEcon, First == TRUE)
Vars <- c("DateField", Symbols)
CombinedEconSlim <- CombinedEconSlim[, Vars]

# Extend quarterly variables through the quarter
Quart <- function(data, Var){
  TempPos <- match(Var, names(data))
  Variable <- data[, TempPos]
  Q2 <- c(NA, Variable[-length(Variable)])
  Q3 <- c(NA, Q2[-length(Variable)])
  LagsDF <- data.frame(data[, 1], Q2, Q3)
  names(LagsDF) <- c("DateField", "Q2", "Q3")
  TempComb <- FillIn(data, LagsDF, Var1 = Var, Var2 = "Q2", KeyVar = "DateField")
  TempComb <- TempComb[, -2]
  TempComb <- FillIn(TempComb, LagsDF, Var1 = Var, Var2 = "Q3", KeyVar = "DateField")
  TempComb <- TempComb[, -2]
  TempComb
}

CombinedEconSlim <- Quart(CombinedEconSlim, "GDPC96")
CombinedEconSlim <- Quart(CombinedEconSlim, "GDPDEF")

# Save as EconData.csv
write.csv(CombinedEconSlim,
          file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/FREDEconData.csv")

####----------- Variable Description Table ----------####
ColNames <- names(CombinedEconSlim[, 2:7])
Description <- c("Consumer Price Index for All Urban Consumers: All Items",
  "Effective Federal Funds Rate (daily)",
  "Effective Federal Funds Rate (monthly)",
  "GDPDEF = Gross Domestic Product: Implicit Price Deflator",
  "GDPC96 = Real Gross Domestic Product, 3 Decimal",
  "U6RATE = Total unemployed, plus all marginally attached workers plus total employed part time for economic reasons)")
Source <- "FRED: http://research.stlouisfed.org/fred2/"

VarList <- cbind(ColNames, Description, Source)
VarList <- xtable(VarList)
Table <- print(VarList, type = "html")

cat("#Variable Label and Descriptions For Economic Data", Table, 
    file = "~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data/EconDataDescription.md")