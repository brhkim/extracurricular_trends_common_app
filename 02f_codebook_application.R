
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "readxl", "quanteda", "tidytext"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# Load in activities
activities <- read_fst("data/01_activities.fst") %>%
  mutate(activitynum=as.numeric(activitynum)) %>%
  filter(!is.na(honors) & honors!="" &
           !is.na(details) & details!="")

# Load in finalized dictionaries
cat_1_list <- read_csv("data/02e_cat_1_list.csv")
cat_2_list <- read_csv("data/02e_cat_2_list.csv")
cat_5_list <- read_csv("data/02e_cat_5_list.csv")
anti_1_list <- read_csv("data/02e_anti_1_list.csv")
anti_2_list <- read_csv("data/02e_anti_2_list.csv")
anti_5_list <- read_csv("data/02e_anti_5_list.csv")
siu_list <- read_csv("data/02e_siu_list.csv")

# Count up the ngrams
ngram_max <- bind_rows(cat_1_list, cat_2_list, cat_5_list,
                       anti_1_list, anti_2_list, anti_5_list) %>%
  .$ngrams %>%
  max()

# Clean the data first
activities_sub <- activities %>%
  select(applicant_id, season, activitynum, text_honors=honors, text_details=details) %>%
  pivot_longer(cols=c(text_honors, text_details), names_to="text_type", names_prefix="text_", values_to="text") %>%
  arrange(applicant_id, activitynum, text_type) %>%
  mutate(doc_id=paste(applicant_id, season, activitynum, text_type, sep="_")) %>%
  {preclean <<- .; .} %>%
  mutate(text=str_to_lower(text),
         text=str_replace_all(text, "-", " "),
         text=str_replace_all(text, "–", " "),
         text=str_replace_all(text, "_", " "),
         text=str_replace_all(text, "&", " and "),
         text=str_replace_all(text, "\\.", " "),
         text=str_replace_all(text, "\\/", " "),
         text=str_replace_all(text, ",", " "),
         text=str_replace_all(text, "\\(", " "),
         text=str_replace_all(text, "\\)", " "),
         text=str_replace_all(text, "\\'s ", "s "),
         text=str_replace_all(text, "\\s' ", "s "),
         text=str_replace_all(text, "\\'", ""),
         text=str_replace_all(text, '\\"', ""),
         text=str_replace_all(text, ";", " "),
         text=str_replace_all(text, ":", " "),
         text=str_replace_all(text, "@", " at "),
         text=str_squish(text))

# Remove original file for memory preservation
rm(activities)

# Get character counts first and save separately
charcounts <- preclean %>%
  mutate(char_length=str_length(text)) %>%
  group_by(applicant_id, season, activitynum) %>%
  summarize(char_length=sum(char_length))

write_fst(charcounts, path="data/02f_charcounts.fst")

# Remove original file for memory preservation
rm(charcounts)

# Apply the dictionaries across different numbers of ngram tokenization schemes as appropriate
for(i in 1:ngram_max) {
  tokenized <- activities_sub %>%
    select(doc_id, text) %>%
    corpus(docid_field="doc_id", text_field="text", unique_docnames=TRUE) %>%
    tokens(split_hyphens=TRUE) %>%
    tokens_ngrams(n = i, concatenator = "_")
  
  dict_cats <- dictionary(
    list(
      cat_1 = cat_1_list %>% filter(ngrams==i) %>% .$Term,
      cat_2 = cat_2_list %>% filter(ngrams==i) %>% .$Term,
      cat_5 = cat_5_list %>% filter(ngrams==i) %>% .$Term,
      anti_1 = anti_1_list %>% filter(ngrams==i) %>% .$Term,
      anti_2 = anti_2_list %>% filter(ngrams==i) %>% .$Term,
      anti_5 = anti_5_list %>% filter(ngrams==i) %>% .$Term
    ))
  
  cat_check_df <- tokenized %>%
    tokens_lookup(dictionary = dict_cats) %>% 
    dfm() %>%
    convert(to="data.frame")
  
  colnames(cat_check_df) <- colnames(cat_check_df %>% select(-doc_id)) %>%
    paste0(., "_", i) %>%
    c("doc_id", .)
  
  if(i==1) {
    output <- cat_check_df
  } else {
    output <- output %>%
      left_join(cat_check_df, by="doc_id")
  }
  
  print(paste0("Done with ngrams of length ", i))
  print(object.size(tokenized), units="MB")
  print(object.size(output), units="MB")
  
  # Clean up memory
  rm(tokenized, dict_cats, cat_check_df)
}

# Get the rowsums really quickly to get totals
coltypes <- c("cat_1", "cat_2", "cat_5", "anti_1", "anti_2", "anti_5")
for(coltype in coltypes) {
  cols_to_sum <- output %>%
    select(starts_with(coltype)) %>%
    colnames()

  output[[coltype]] <- rowSums(output[,cols_to_sum], na.rm=TRUE)
}

# Run some quick verification checks to make sure things worked as expected
summed_output <- output %>%
  select(doc_id, cat_1, cat_2, cat_5, anti_1, anti_2, anti_5) %>%
  left_join(preclean %>% select(doc_id, text_raw=text), by="doc_id") %>%
  left_join(activities_sub %>% select(doc_id, text_clean=text), by="doc_id") %>%
  relocate(doc_id, text_raw, text_clean, everything())

verify2 <- summed_output %>%
  filter(cat_2<anti_2)

verify1 <- summed_output %>%
  filter(cat_1<anti_1)

verify5 <- summed_output %>%
  filter(cat_5<anti_5)

# Harmonize final results by removing counts of "anti" phrases from overall counts of leadership/excellence phrases
cat_by_activity <- summed_output %>%
  mutate(
    cat_1 = cat_1 - anti_1,
    cat_1 = case_when(
      cat_1<0 ~ 0,
      TRUE ~ cat_1
    ),
    cat_2 = cat_2 - anti_2,
    cat_2 = case_when(
      cat_2<0 ~ 0,
      TRUE ~ cat_2
    ),
    cat_5 = cat_5 - anti_5,
    cat_5 = case_when(
      cat_5<0 ~ 0,
      TRUE ~ cat_5
    ),
    applicant_id=as.numeric(str_split_fixed(doc_id,"_", 4)[,1]),
    season=as.numeric(str_split_fixed(doc_id,"_", 4)[,2]),
    activitynum=as.numeric(str_split_fixed(doc_id,"_", 4)[,3]),
    text_type=str_split_fixed(doc_id,"_", 4)[,4],
    ) %>%
  {spotcheck_df <<- .; .} %>% # Save a separate dataset before we collapse so we can review them manually for validation purposes
  group_by(applicant_id, season, activitynum) %>%
  summarize(across(.cols=starts_with("cat_"), ~ sum(.x, na.rm=TRUE))) %>%
  ungroup()

# Write results
write_fst(cat_by_activity, path="data/02f_cat_check_output.fst")
write_fst(spotcheck_df, path="data/02f_spotcheck_df.fst")



