## Messy Data and Machine Learning Homework 4
## Paul Sergent, Ruoyu Zhu, Frankie Tam
## Appeals_analysis.R

library(tidyverse)
library(tidytext)
library(tm)

## Part A

# Import data 
appeals.data <- read.delim('recent_opinions.tsv', header=TRUE, col.names=c('year','text','circuit'))

# custom words
custom_words <- read.table("custom_words.txt", header = FALSE, col.names=c('word'))
custom_words <- as.tibble(custom_words)
# Add lexicon column
custom_words <- custom_words %>% mutate(lexicon = "custom")

# Combine custom words and stop words
all_stop_words <- rbind(stop_words, custom_words)

## Part B



