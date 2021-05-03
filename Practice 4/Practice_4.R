library(tidyverse)
setwd("C:/Users/Jordan Lian/OneDrive - Northeastern University/Spring 2021/DA 5030/Practice 4")

' Problem 1 - Build an R Notebook of the SMS message filtering example in the textbook on pages 103 to 123 (data set). 
Show each step and add appropriate documentation. Note that the attached data set differs slightly from the one used 
on the book; the number of cases differ.'
sms_raw <- read_csv('da5030.spammsgdataset.csv')
# step 2, Data Preparation, CLEANING AND STANDARDIZING TEXT DATA

  library(tm)
  # convert to factor
  sms_raw$type <- factor(sms_raw$type)
  
  str(sms_raw$type)
  table(sms_raw$type)
  
  # create a corpus, which is a collection of text documents
  sms_corpus <- VCorpus(VectorSource(sms_raw$text))
  sms_corpus
  
  # inspect the corpus
  inspect(sms_corpus[1:2])
  
  # view the text
  as.character(sms_corpus[[1]])
  
  # view multiple documents
  lapply(sms_corpus[1:2], as.character)
  
  # standardize to all lowercase
  sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower))
  
  # make sure tm_map() function worked
  as.character(sms_corpus[[1]])
  as.character(sms_corpus_clean[[1]])
  
  # remove all the numbers from the corpus
  sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
  
  # remove the words 
  sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
  
  # 
  sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
  
  library(SnowballC)
  sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

  sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)
  as.character(sms_corpus[[1]])
  as.character(sms_corpus_clean[[1]])

# Data Preparation, SPLITTING TEXT DOCUMENTS INTO WORDS
  sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
  sms_dtm
  
  sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list(
    tolower = TRUE,
    removeNumbers = TRUE,
    stopwords = TRUE,
    removePunctuation = TRUE,
    stemming = TRUE
  ))
  sms_dtm2
# create training/test datasets
  sms_dtm_train <- sms_dtm[1:4169, ]
  sms_dtm_test  <- sms_dtm[4170:5559, ]
  
  sms_train_labels <- sms_raw[1:4169, ]$type
  sms_test_labels  <- sms_raw[4170:5559, ]$type
  
  prop.table(table(sms_train_labels))
  prop.table(table(sms_test_labels))
  
# visualize text data - wordclouds
  library(wordcloud)
  wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
  spam <- subset(sms_raw, type == "spam")
  ham <- subset(sms_raw, type == "ham")
  
  wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
  wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
  
# CREATING INDICATOR FEATURES FOR FREQUENT WORDS
  findFreqTerms(sms_dtm_train, 5)
  sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
  str(sms_freq_words)
  
  sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
  sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]
  
  sms_dtm_freq_train
  sms_dtm_freq_test
  
  convert_counts <- function(x) {
    x <- ifelse(x > 0, "Yes", "No")
  }
  
  sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
                     convert_counts)
  sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
                      convert_counts)
  
  sms_train
  sms_test
  
# step 3, train a model on the data
  library(e1071)
  sms_classifier <- naiveBayes(sms_train, sms_train_labels)
  sms_classifier
  
# step 4, evaluate model performance
  sms_test_pred <- predict(sms_classifier, sms_test)

  library(gmodels)
  CrossTable(sms_test_pred, sms_test_labels,    prop.chisq = FALSE, prop.t = FALSE,    dnn = c('predicted', 'actual'))
  
# step 5, improve model performance
  sms_classifier2 <- naiveBayes(sms_train, sms_train_labels, laplace = 1)
  sms_test_pred2 <- predict(sms_classifier2, sms_test)
  CrossTable(sms_test_pred2, sms_test_labels,
             prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
             dnn = c('predicted', 'actual'))
  
' Problem 2 - Install the requisite packages to execute the following code that classifies the built-in iris data using 
Naive Bayes. Build an R Notebook and explain in detail what each step does. Be sure to look up each function to understand how it is used.'
library(klaR)
data(iris)

nrow(iris)
summary(iris)
head(iris)

testidx <- which(1:length(iris[, 1]) %% 5 == 0)

# separate into training and testing datasets
iristrain <- iris[-testidx,]
iristest <- iris[testidx,]

# apply Naive Bayes
library(naivebayes)
nbmodel <- naive_bayes(Species~., data=iristrain)
nbmodel

# check the accuracy
prediction <- predict(nbmodel, iristest[,-5])
prediction
table(prediction, iristest[,5])