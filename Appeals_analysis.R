## Messy Data and Machine Learning Homework 4
## Paul Sergent, Ruoyu Zhu, Frankie Tam
## Appeals_analysis.R

library(tidyverse)
library(tidytext)
library(tm)

## Part A

# Import data 
appeals.data <- read.delim('recent_opinions.tsv', header=TRUE, stringsAsFactors = FALSE, col.names=c('year','text','circuit'))
appeals.data <- as.tibble(appeals.data)

# custom words
custom_words <- read.table("custom_words.txt", header = FALSE, col.names=c('word'))
custom_words <- as.tibble(custom_words)
# Add lexicon column
custom_words <- custom_words %>% mutate(lexicon = "custom")

# Combine custom words and stop words
all_stop_words <- rbind(stop_words, custom_words)

## Part B

# a)
text_appeals <- appeals.data%>% unnest_tokens(word, text)

#stop words remove use anti-join
#data(all_stop_words)
text_appeals <- text_appeals %>% anti_join(all_stop_words)
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

# add opinion_id 
document_term <- tibble::rowid_to_column(document_term, "opinion_id")

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