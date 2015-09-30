#####################
# Descriptive topic plot
# Christopher Gandrud
# MIT License
#####################

# Set working directory 
setwd('~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/RegressionAnalysis/')

# Load required packages
library(rio)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)

# Import and transform data for plotting
main <- import('Combined DatasetnoNA.csv')

sub <- main %>% select(month_year, Financial.Markets, Monetary.Policy,
                       Banking.Regulation)

sub$month_year <- sub$month_year %>% mdy

sub_gathered <- sub %>% gather(topic, proportion, 2:ncol(sub))
sub_gathered <- sub_gathered %>% filter(proportion > 0.33) # Keep only speeches with over 33% topic

# Function to create individual plots
plotter <- function(x) {
    title_clean <- gsub(pattern = '\\.', replacement = ' ', x)
    message(title_clean)
    temp <- subset(sub_gathered, topic == i)
    ggplot(temp, aes(month_year, proportion, group = topic)) +
        geom_point(alpha = 0.4) +
        stat_smooth(se = F, color = 'black') +
        geom_vline(xintercept = as.numeric(as.POSIXct("2008-09-01 12:00:00")),
                   linetype = 'dotted') +
        scale_y_continuous(limits = c(0.33, 1), breaks = c(0.33, 0.5, 0.8, 1)) +
        ylab('') + xlab('') +
        ggtitle(title_clean) +
        theme_bw() 
}

# Plot and combine
topic_list <- list()
for (i in unique(as.character(sub_gathered$topic))) {
    topic_list[[i]] <- plotter(i)
}

pdf(file = '~/Dropbox/Fed_Speeches_Paper/Journal_Submissions/current/figures/TopicBasic.pdf', 
    width = 5)
    do.call(grid.arrange, topic_list)
dev.off()
       
