# ---------------------------------------------------------------------------- #
# Plot topic intrusion results
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(ggplot2)

setwd('~/Dropbox/Fed_Speeches_Paper/')

topic_intrusion <- import('FedSpeech/SourceCode/TopicIntrusion/topic_intrusion_results_May_2015.csv')

topic_intrusion$prop_identified <- rowMeans(topic_intrusion[, c(6:11)], na.rm = T)

topic_intrusion$top <- topic_intrusion$top %>% factor(levels = c(
                            "bankingreg", "finmarkets", "mp", 
                            "macroecon", "intecon", "localhousing"),
    labels = c("Banking Regulation", "Financial Markets", "Monetary Policy",
               "Semantically Quest. 1", "Semantically Quest. 2", 
               "Semantically Quest. 3"))

ggplot(topic_intrusion, aes(top, prop_identified)) +
    geom_boxplot() +
    xlab('') + ylab('Proportion of identified intruders\n') +
    theme_bw()

ggsave('JoPP/R1/figures/intruders_identified.pdf')  
