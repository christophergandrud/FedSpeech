#####################
# Descriptive topic plot
# Christopher Gandrud
# MIT License
#####################


library(rio)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(gridExtra)

main <- import('~/Dropbox/Fed_Speeches_Paper/FedSpeech/SourceCode/RegressionAnalysis/Combined DatasetnoNA.csv')

sub <- main %>% select(month_year, Financial.Markets, Macroeconomics, 
                       Monetary.Policy, International.Economy, Local.Housing.Dev,
                       Banking.Regulation)

sub$month_year <- sub$month_year %>% mdy

sub_gathered <- sub %>% gather(topic, proportion, 2:ncol(sub))
sub_gathered <- sub_gathered %>% filter(proportion > 0.33) # Keep only speeches with over 33% topic


plotter <- function(x) {
    title_clean <- gsub(pattern = '\\.', replacement = ' ', x)
    message(title_clean)
    temp <- subset(sub_gathered, topic == i)
    ggplot(temp, aes(month_year, proportion, group = topic)) +
        geom_point(alpha = 0.4) +
        stat_smooth(se = F, color = 'black') +
        ylab('') + xlab('') +
        ggtitle(title_clean) +
        theme_bw() 
}

topic_list <- list()
for (i in unique(as.character(sub_gathered$topic))) {
    topic_list[[i]] <- plotter(i)
}

pdf(file = '~/Dropbox/Fed_Speeches_Paper/JoPP/R1/figures/TopicBasic.pdf')
    do.call(grid.arrange, topic_list)
dev.off()
       
