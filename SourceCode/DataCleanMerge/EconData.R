##############
# Download econ data and save as monthly data file.
# Christopher Gandrud
# 18 May 2013
##############

# Packages 
library(quantmod)
library(xts)
library(lubridate)

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

# Function based on: http://stackoverflow.com/questions/4368861/r-converting-xts-or-zoo-object-to-a-data-frame
ToDF <- function(x){
  mthlySumm <- apply.monthly(x, mean)
  index(mthlySumm) <- as.yearmon(index(mthlySumm))
  Data <- as.data.frame(mthlySumm)
  DateField <- index(x)
  Test <- data.frame(x, DateField)
  Test
}

apply 