###############
# Changepoint Play 2 (Using MultiVariate)
# Christopher Gandrud
# 30 May 2013
############### 

# Load packages
library(digest)
library(devtools)

# Load e.divGG function
source_gist("5675688")

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data")

MainMonth <- read.csv("MainMonth.csv")

# Drop if no hearings
MainMonth <- MainMonth[-1:-2, ]
MainMonth <- MainMonth[-193:-197, ]

# Scrutiny Change Point
ScrutVars <- c("TestCountMonthNoF", "MembPresMeanNoF", "LaughMeanNoF")
e.divGG(data = MainMonth, Vars = ScrutVars, TimeVar = "MonthYear", 
		sig.lvl = 0.05, R = 799, min.size = 12)

# Economic Indications Change Point
