###########################
# LDA Topic model explore
# Christopher Gandrud
# MIT License
###########################

# Load required packages
library(tm)
library(dplyr)
library(stringi)
library(topicmodels)
library(LDAvis)

# Set working directory
setwd('~/Dropbox/Fed_Speeches_Data/fed.text.parsed/')

# Load and process corpus
corpus <- Corpus(DirSource()) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords('english'), mc.cores = 1) %>%
    tm_map(stemDocument, mc.cores = 1) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removePunctuation, mc.cores = 1) %>%
    tm_map(removeNumbers, mc.cores = 1)

# Create document-term matrix for LDA
doc_term <- DocumentTermMatrix(corpus)

#### Run topic models for a range of topics ####
topic_numbers <- c(3, 5, 7, 10, 15, 20, 30, 40, 50)
for (i in topic_numbers) {
    message(sprintf('Running %s', i))
    assign(sprintf('lda_%s', i), LDA(doc_term, k = i, seed = 1001))
}

#### Compare log-likelihood per number of topics, to assess model fit ####
lda_out <- sprintf('lda_%s', topic_numbers)
log_lik <- vector()
for (i in lda_out) log_lik <- c(log_lik, logLik(eval(parse(text = i)))[[1]])

plot(topic_numbers, log_lik)

#### Visualise topics and terms ####
# Create json objected needed for LDAvis
topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
    # Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    temp_frequency <- inspect(doc_term)
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    rm(temp_frequency)

    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                            vocab = vocab,
                            doc.length = doc_length,
                            term.frequency = freq_matrix$Freq)

    return(json_lda)
}

json_lda <- topicmodels_json_ldavis(fitted = lda_20, corpus = corpus,
                                doc_term = doc_term)


# Visualise with LDAvis
serVis(json_lda, out.dir = '~/Desktop/vis_20_topics_7May' open.browser = T)
