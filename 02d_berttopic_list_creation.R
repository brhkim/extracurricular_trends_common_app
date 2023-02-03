# This file intakes the output from the BERTopic analysis and puts the resulting files into a more amenable
# format for human analysis and interpretation


#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse"), require, character.only = TRUE)


# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# Get bertopic output
honors_lists <- read_csv("data/02c_honors_topics.csv") %>%
  rename(word=`...1`)
details_lists <- read_csv("data/02c_details_topics.csv") %>%
  rename(word=`...1`)

wordlist_cleaner <- function(df) {
  
  topic_list <- df %>%
    select(-word) %>%
    colnames(.)
  
  output <- data.frame(topicnum=as.numeric(), wordlist=as.character())
  for (i in topic_list) {
    tmp <- df %>%
      select(word, all_of(i)) %>%
      arrange(desc(get(i))) %>%
      select(word) %>%
      slice_head(n=20) %>%
      .$word
    
    output_tmp <- data.frame(topicnum=as.numeric(i), wordlist=paste(tmp, collapse=", "))
    output <- bind_rows(output, output_tmp)
  }
  
  output
}

honors_wordlist_cleaned <- wordlist_cleaner(honors_lists)
details_wordlist_cleaned <- wordlist_cleaner(details_lists)


write_csv(honors_wordlist_cleaned, file="data/02d_honors_wordlist_cleaned.csv")
write_csv(details_wordlist_cleaned, file="data/02d_details_wordlist_cleaned.csv")


