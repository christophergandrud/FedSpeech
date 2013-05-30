###############
# Changepoint Play 2 (Using MultiVariate)
# Christopher Gandrud
# 30 May 2013
############### 

# Load packages
library(digest)
library(devtools)
library(plyr)

# Load e.divGG function
source_gist("5675688")

# Set working directory
setwd("~/Dropbox/Fed_Speeches_Paper/FedSpeech/Data")

MainMonth <- read.csv("MainMonth.csv")

# Drop if no hearings
MainMonth <- MainMonth[-1:-2, ]
MainMonth <- MainMonth[-193:-197, ]

# Rename variables so that they are more legible when plotted
MainMonth <- rename(MainMonth, c("TestCountMonthNoF" = "Hearings",
                                 "MembPresMeanNoF" = "Attendance",
                                 "LaughMeanNoF" = "Laughter",
                                 "U6RATE" = "Unemployment",
                                 "GDPC96Percent" = "Growth",
                                 "PCEPIPercent" = "Inflation"))

# Scrutiny Change Point
ScrutVars <- c("Hearings", "Attendance", "Laughter")
e.divGG(data = MainMonth, Vars = ScrutVars, TimeVar = "MonthYear", 
		sig.lvl = 0.05, R = 799, min.size = 2)

ScrutVars <- c("Hearings", "Attendance")
e.divGG(data = MainMonth, Vars = ScrutVars, TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 799, min.size = 2)

# Economic Indications Change Point
EconVars <- c("Unemployment", "Growth")
e.divGG(data = MainMonth, Vars = EconVars, TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 799, min.size = 24)

EconVars <- c("Inflation", "Growth")
e.divGG(data = MainMonth, Vars = EconVars, TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 799, min.size = 16)

EconVars <- c("Inflation", "Unemployment", "Growth")
e.divGG(data = MainMonth, Vars = EconVars, TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 799, min.size = 16)

EconVars <- c("Inflation", "Unemployment")
e.divGG(data = MainMonth, Vars = EconVars, TimeVar = "MonthYear", 
        sig.lvl = 0.05, R = 799, min.size = 16)