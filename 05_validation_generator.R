
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "readxl", "quanteda", "tidytext"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

number_of_coders <- 4

covariates <- read_fst("data/02_covariates.fst")
spotcheck_df <- read_fst(path="data/02f_spotcheck_df.fst")

processed <- spotcheck_df %>%
  mutate(text_clean=str_squish(text_raw)) %>%
  group_by(applicant_id, season, activitynum) %>%
  arrange(desc(text_type)) %>%
  summarize(cat_1=sum(cat_1),
            cat_2=sum(cat_2),
            cat_5=sum(cat_5),
            text_all=paste0(text_clean, collapse=". "))

# Make sure we only look at applicants who don't have missing data for stratification
covariates_subset <- covariates %>%
  filter(international==0) %>%
  filter(female_miss==0 & race_miss==0 & firstgen_miss==0 & 
           school_other==0 & multischool_miss==0 & highest_income_quintile_miss==0) %>%
  select(applicant_id, season, female, school_public, firstgen, urm, feewaiver)

joined <- processed %>%
  inner_join(covariates_subset, by=c("applicant_id", "season"))

joined$cat_1[joined$cat_1>=1] <- 1
joined$cat_2[joined$cat_2>=1] <- 1
joined$cat_5[joined$cat_5>=1] <- 1

groups_check <- joined %>%
  group_by(cat_1, female, school_public, firstgen, urm, feewaiver) %>%
  summarize(count=n()) %>%
  ungroup() 

write_fst(joined, path="data/05_validation_set_full.fst")
joined <- read_fst(path="data/05_validation_set_full.fst")

# Create 5 sets of 64 per category
sampler <- function(df, groupname, category, categoryname) {
  tmp <- df %>%
    group_by({{category}}, female, school_public, firstgen, urm, feewaiver) %>%
    slice_sample(n=1) %>%
    ungroup() %>%
    mutate(groupset=paste0(groupname),
           random_sort=rnorm(n=1)) %>%
    arrange(random_sort) %>%
    select(groupset, applicant_id, season, activitynum, text_all)
  
  tmp[[categoryname]] <- ""
  
  write_csv(tmp, file=paste0("output/05_", groupname, ".csv"))
  
  tmp
}

# Set seed for replication purposes
set.seed(83191)

cat_1_1 <- joined %>%
  sampler(groupname="Positional Leadership Set 1", 
          category=cat_1, 
          categoryname="Positional Leadership") %>%
  {antijoiner <<- .; .}

cat_1_2 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Positional Leadership Set 2", 
          category=cat_1, 
          categoryname="Positional Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_1_3 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Positional Leadership Set 3", 
          category=cat_1, 
          categoryname="Positional Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_1_4 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Positional Leadership Set 4", 
          category=cat_1, 
          categoryname="Positional Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_1_5 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Positional Leadership Set 5", 
          category=cat_1, 
          categoryname="Positional Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

###

set.seed(83192)

cat_2_1 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Singular Leadership Set 1", 
          category=cat_2, 
          categoryname="Singular Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_2_2 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Singular Leadership Set 2", 
          category=cat_2, 
          categoryname="Singular Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_2_3 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Singular Leadership Set 3", 
          category=cat_2, 
          categoryname="Singular Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_2_4 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Singular Leadership Set 4", 
          category=cat_2, 
          categoryname="Singular Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_2_5 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Singular Leadership Set 5", 
          category=cat_2, 
          categoryname="Singular Leadership") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

###

set.seed(83193)

cat_5_1 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Excellence Set 1", 
          category=cat_5, 
          categoryname="Excellence") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_5_2 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Excellence Set 2", 
          category=cat_5, 
          categoryname="Excellence") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_5_3 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Excellence Set 3", 
          category=cat_5, 
          categoryname="Excellence") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_5_4 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Excellence Set 4", 
          category=cat_5, 
          categoryname="Excellence") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

cat_5_5 <- joined %>%
  anti_join(antijoiner, by=c("applicant_id", "season", "activitynum")) %>%
  sampler(groupname="Excellence Set 5", 
          category=cat_5, 
          categoryname="Excellence") %>%
  {antijoiner <<- bind_rows(antijoiner, .); .}

combined <- bind_rows(cat_1_1, cat_1_2, cat_1_3, cat_1_4, cat_1_5,
                      cat_2_1, cat_2_2, cat_2_3, cat_2_4, cat_2_5,
                      cat_5_1, cat_5_2, cat_5_3, cat_5_4, cat_5_5)

# Write all the validation sets together
validate <- combined %>%
  select(-`Positional Leadership`, -`Singular Leadership`, -`Excellence`) %>%
  left_join(joined) %>%
  mutate(group=case_when(
    str_detect(groupset, "Singular") ~ "Singular",
    str_detect(groupset, "Positional") ~ "Positional",
    str_detect(groupset, "Excellence") ~ "Excellence"
  )) 

write_fst(validate, path="data/05_validation_set_key.fst")

validate_check <- validate %>%
  filter(group=="Positional") %>%
  count(groupset, cat_2)
  

