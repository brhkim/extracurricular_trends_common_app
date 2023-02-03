# This file conducts basic word frequency analysis on the extracurriculars write-in fields
# to examine the most commonly reported phrases/activities by each racial/ethnic group.
# This also outputs the preliminary text dataset used in the BERTopic analysis.

#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "readxl", "quanteda", "tidytext"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# Read in data
applications <- read_fst("data/01_applications.fst")
applicantprofile <- read_fst("data/01_applicantprofile.fst")
members <- read_fst("data/01_members.fst")
extracurriculars_long <- read_fst("data/01_activities.fst")

# Create function to determine most common tokens at 1, 2, and 3 token lengths
get_wordfreqs <- function(df, field, group_filename) {
  wordfreqs <- df %>%
    select(applicant_id, activitynum, {{field}}) %>%
    mutate(
      doc_id=paste0(applicant_id, "__", activitynum)
    ) %>%
    corpus(docid_field="doc_id", text_field=field, unique_docnames=TRUE) %>%
    tokens(remove_punct=TRUE, remove_symbols=TRUE, remove_numbers=TRUE, remove_url=TRUE) %>%
    tokens_tolower() 
  
  wordfreqs_1 <- wordfreqs %>%
    tokens_ngrams(n = 1L, skip = 0L, concatenator = "_") %>%
    dfm() %>%
    topfeatures(n=500, scheme="count") %>%
    as.data.frame() %>%
    rename(.data=., appearances=`.`) %>%
    filter(appearances>=10)
  
  write.csv(wordfreqs_1, file=paste0("data/02b_", group_filename, "_", field, "_1.csv"))
  
  wordfreqs_2 <- wordfreqs %>%
    tokens_ngrams(n = 2L, skip = 0L, concatenator = "_") %>%
    dfm() %>%
    topfeatures(n=500, scheme="count") %>%
    as.data.frame() %>%
    rename(.data=., appearances=`.`) %>%
    filter(appearances>=10)
  
  write.csv(wordfreqs_2, file=paste0("data/02b_", group_filename, "_", field, "_2.csv"))
  
  wordfreqs_3 <- wordfreqs %>%
    tokens_ngrams(n = 3L, skip = 0L, concatenator = "_") %>%
    dfm() %>%
    topfeatures(n=500, scheme="count") %>%
    as.data.frame() %>%
    rename(.data=., appearances=`.`) %>%
    filter(appearances>=10)
  
  write.csv(wordfreqs_3, file=paste0("data/02b_", group_filename, "_", field, "_3.csv"))
}

# Run function for honors and details fields for all applicants
extracurriculars_long %>%
  get_wordfreqs(., "honors", "all")

extracurriculars_long %>%
  get_wordfreqs(., "details", "all")

# Now proceed to run the function by student subgroups
extracurriculars_long %>%
  filter(applicant_raceethnicity=="White") %>%
  get_wordfreqs(., "honors", "white")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="White") %>%
  get_wordfreqs(., "details", "white")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="Black or African American") %>%
  get_wordfreqs(., "honors", "black")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="Black or African American") %>%
  get_wordfreqs(., "details", "black")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="Asian") %>%
  get_wordfreqs(., "honors", "asian")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="Asian") %>%
  get_wordfreqs(., "details", "asian")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="Latinx") %>%
  get_wordfreqs(., "honors", "latinx")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="Latinx") %>%
  get_wordfreqs(., "details", "latinx")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="American Indian or Alaska Native") %>%
  get_wordfreqs(., "honors", "amerind")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="American Indian or Alaska Native") %>%
  get_wordfreqs(., "details", "amerind")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="Native Hawaiian or Other Pacific Islander") %>%
  get_wordfreqs(., "honors", "nativehawaiian_pacisl")

extracurriculars_long %>%
  filter(applicant_raceethnicity=="Native Hawaiian or Other Pacific Islander") %>%
  get_wordfreqs(., "details", "nativehawaiian_pacisl")

# Generate a simple dataset for the BERTopic analysis
output_topicmodeling <- extracurriculars_long %>%
  select(honors, details) %>%
  filter(!(is.na(honors) & is.na(details)))

# Output the file for BERTopic
write.csv(output_topicmodeling, file="data/02b_topic_modeling_prep.csv")
