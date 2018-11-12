## Messy Data and Machine Learning Homework 4
## Paul Sergent, Ruoyu Zhu, Frankie Tam
## Appeals_analysis.R

library(tidyverse)
library(tidytext)
library(tm)
library(ROCR)

## Part A

# Import data 
appeals.data <- read.delim('recent_opinions.tsv', header=TRUE, stringsAsFactors = FALSE, col.names=c('year','text','circuit'))
appeals.data <- as.tibble(appeals.data)
# add opinion_id 
appeals.data <- tibble::rowid_to_column(appeals.data, "opinion_id")

# custom words
custom_words <- read.table("custom_words.txt", header = FALSE, col.names=c('word'))
custom_words <- as.tibble(custom_words)

# Add lexicon column
custom_words <- custom_words %>% mutate(lexicon = "custom")

# Combine custom words and stop words
all_stop_words <- rbind(stop_words, custom_words)

## Part B

# a)
text_appeals <- appeals.data %>% unnest_tokens(word, text)

#stop words remove use anti-join
text_appeals <- text_appeals %>% anti_join(all_stop_words)

#stemming
text_appeals <- text_appeals %>% mutate(word = SnowballC::wordStem(word)) 

# top 10 most common words in the entire corpus
text_appeals %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)

#top 10 most common words in each circuit
text_appeals_fifth <- text_appeals %>% filter(circuit == 'fifth')
text_appeals_fifth %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)

text_appeals_ninth <- text_appeals %>% filter(circuit == 'ninth')
text_appeals_ninth %>% count(word) %>% arrange(desc(n)) %>% slice(1:10)

# b)

# create document term tibble
document_term <- appeals.data

# top 100 most common words in the entire corpus
top_100 <- text_appeals %>% count(word) %>% arrange(desc(n)) %>% slice(1:100)

# recode circuit to 1 and 0
document_term <- document_term %>% mutate (circuit = case_when (
                                          circuit == "fifth" ~ 1,
                                          circuit == "ninth" ~ 0))

# convert text to lower case
document_term <- document_term %>% mutate (text = tolower(text))

# add columns for top 100 words

for (i in 1:100){
  document_term <- document_term %>% mutate (newcol = str_count(document_term$text, top_100$word[i])) 
  names(document_term)[names(document_term) == "newcol"] <- top_100$word[i]
}

# remove extra columns
document_term <- document_term %>% select(-text, -year)

# shuffle the data
document_term <- document_term %>% slice(sample(1:n()))

# 50% for training set
split_size = floor(nrow(document_term)/2)
train <- document_term %>% slice(1:split_size)

# 50% for test set
test <- document_term %>% slice(split_size+1:n())

# c)

test_b <- test  #test set for part B

model <- glm(circuit ~ ., train, family=binomial())
#Warning messages:
#1: glm.fit: algorithm did not converge 
#2: glm.fit: fitted probabilities numerically 0 or 1 occurred 

# examine model
summary(model)

# AUC 
test_predict <- predict(model,newdata = test_b, type='response') 
test_b <-  test_b %>% mutate(prediction_prob = test_predict)
test_b.pred <- prediction(test_b$prediction_prob, test_b$circuit)
test_b.perf <- performance(test_b.pred, "auc")
AUC_b <- 100*test_b.perf@y.values[[1]]
cat('the AUC for part B is', AUC_b)

#Explain why your result is strange and which predictor is causing the strange result ??
# AUC = 99.56

# summary sorted by coefficients
coef <- summary(model)[["coefficients"]]
coef[order(coef[,1], decreasing = T),]

## d) 
test_b_new <- test


# drop problematic predictor 
#drop 'opinion_id' can solve 'glm.fit: algorithm did not converge'
# select(-requir, -decis, -deni, -judgment, -reason)
model2  <- glm(circuit ~ . -opinion_id, train, family=binomial())

#new AUC = 89.64
test_predict <- predict(model2,newdata = test_b_new, type='response') 
test_b_new <-  test_b_new %>% mutate(prediction_prob = test_predict)
test_b_new.pred <- prediction(test_b_new$prediction_prob, test_b_new$circuit)
test_b_new.perf <- performance(test_b_new.pred, "auc")
AUC_b_new <- 100*test_b_new.perf@y.values[[1]]
cat('the new AUC is', AUC_b_new)

# examine model
summary(model2)

#five smallest/largest coefficients
head(model2$coefficients,5)
tail(model2$coefficients, 5)

# summary sorted by coefficients
coef <- summary(model2)[["coefficients"]]
coef[order(coef[,1], decreasing = T),]

#interpretation of the largest model coefficient

## Part C bigram
#consider the top 100 bigrams instead of individual words.
#When you are removing stop words, make sure you remove bigrams 
#that contain a stop word as either the first or second word in the bigram. [5 pts]

# a)
#unnest
bigrams_appeals <- appeals.data%>% unnest_tokens(bigram, text, token = 'ngrams', n=2)

#bigrams_appeals.cp <- bigrams_appeals
#bigrams_appeals.cp %>% count(bigram, sort = TRUE)

#remove all stop words
bigrams_appeals <- bigrams_appeals %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% all_stop_words$word) %>%
  filter(!word2 %in% all_stop_words$word) %>% unite(bigram, word1, word2, sep = " ")

# top 10 most common bigrams in the entire corpus
bigrams_appeals %>% count(bigram) %>% arrange(desc(n)) %>% slice(1:10)

#top 10 most common bigrams in each circuit
bigrams_appeals_fifth <- bigrams_appeals %>% filter(circuit == 'fifth')
bigrams_appeals_fifth %>% count(bigram) %>% arrange(desc(n)) %>% slice(1:10)

bigrams_appeals_ninth <- bigrams_appeals %>% filter(circuit == 'ninth')
bigrams_appeals_ninth %>% count(bigram) %>% arrange(desc(n)) %>% slice(1:10)

# b)
# create document term tibble
bi_document_term <- appeals.data

# top 100 most common bigrams in the entire corpus
bigram_100 <- bigrams_appeals %>% count(bigram) %>% arrange(desc(n)) %>% slice(1:100)

# recode circuit to 1 and 0
bi_document_term <- bi_document_term %>% mutate (circuit = case_when (
  circuit == "fifth" ~ 1,
  circuit == "ninth" ~ 0))

# convert text to lower case
bi_document_term <- bi_document_term %>% mutate (text = tolower(text))

# add columns for top 100 bigrams

for (i in 1:100){
  bi_document_term <- bi_document_term %>% mutate (newcol = str_count(bi_document_term$text, bigram_100$bigram[i])) 
  names(bi_document_term)[names(bi_document_term) == "newcol"] <- bigram_100$bigram[i]
}

# remove extra columns
bi_document_term <- bi_document_term %>% select(-text, -year)

# shuffle the data
bi_document_term <- bi_document_term %>% slice(sample(1:n()))

# 50% for training set
bi_split_size = floor(nrow(bi_document_term)/2)
bi_train <- bi_document_term %>% slice(1:bi_split_size)

# 50% for test set
bi_test <- bi_document_term %>% slice(bi_split_size+1:n())

# d)

bi_model  <- glm(circuit ~ . -opinion_id, bi_train, family=binomial())

# examine model
summary(bi_model)

test_predict <- predict(bi_model,newdata = bi_test, type='response') 
bi_test <-  bi_test %>% mutate(prediction_prob = test_predict)
bi_test.pred <- prediction(bi_test$prediction_prob, bi_test$circuit)
bi_test.perf <- performance(bi_test.pred, "auc")
AUC_bi <- 100*bi_test.perf@y.values[[1]]
cat('the AUC score is', AUC_bi)
#AUC = 93.87

# summary sorted by coefficients
coef <- summary(bi_model)[["coefficients"]]
coef[order(coef[,1], decreasing = T),]

## Part D

# b)

# d)

