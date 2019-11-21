library(shiny)
library(okcupiddata)
library(tm)
library(SentimentAnalysis)
library(wordcloud)
library(tidyverse)
library(syuzhet)
library(memoise)
data(profiles)
profiles_clean <- profiles %>%
  filter(!is.na(essay0))
sex <<- list("Female" = "f",
               "Male" = "m")
# Using "memoise" to automatically cache the results
getTermMatrix <- memoise(function() {
  myCorpus = Corpus(VectorSource(profiles_clean$essay0))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords("english"), "like", "ive", "san", "francisco"))
  
  myDTM = TermDocumentMatrix(myCorpus,
                             control = list(minWordLength = 1))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = TRUE)
})

sent2 <- get_nrc_sentiment(profiles_clean$essay0)

# Let's look at the corpus as a whole again:
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")


