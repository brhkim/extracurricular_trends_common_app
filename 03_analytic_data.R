
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "readxl", "quanteda", "tidytext"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# Read in activities and related files
activities <- read_fst("data/01_activities.fst") %>%
  mutate(activitynum=as.numeric(activitynum)) %>%
  filter(!is.na(honors) & honors!="" &
           !is.na(details) & details!="")

cat_check_df <- read_fst(path="data/02f_cat_check_output.fst")

charcounts <- read_fst(path="data/02f_charcounts.fst")

covariates <- read_fst("data/02_covariates.fst")

# Break data up into various type groups
type_names <- c("academic", "career", "arts", "other", "service", "athletics", "schoolgov", "cultureid")

activities_joined <- activities %>%
  mutate(
    type_group=case_when(
      type == "Academic" ~ "academic",
      type == "Career Oriented" ~ "career",
      type == "Dance" ~ "arts",
      type == "Foreign Exchange" ~ "cultureid",
      type == "Junior R.O.T.C." ~ "career",
      type == "Other Club/Activity" ~ "other",
      type == "School Spirit" ~ "schoolgov",
      type == "Theater/Drama" ~ "arts",
      type == "Art" ~ "arts",
      type == "Community Service (Volunteer)" ~ "service",
      type == "Debate/Speech" ~ "academic",
      type == "Foreign Language" ~ "cultureid",
      type == "LGBT" ~ "cultureid",
      type == "Religious" ~ "cultureid",
      type == "Science/Math" ~ "academic",
      type == "Work (Paid)" ~ "career",
      type == "Athletics: Club" ~ "athletics",
      type == "Computer/Technology" ~ "academic",
      type == "Environmental" ~ "other",
      type == "Internship" ~ "career",
      type == "Music: Instrumental" ~ "arts",
      type == "Research" ~ "academic",
      type == "Social Justice" ~ "cultureid",
      type == "Athletics: JV/Varsity" ~ "athletics",
      type == "Cultural" ~ "cultureid",
      type == "Family Responsibilities" ~ "other",
      type == "Journalism/Publication" ~ "academic",
      type == "Music: Vocal" ~ "arts",
      type == "Robotics" ~ "academic",
      type == "Student Govt./Politics" ~ "schoolgov"
    )) %>%
  right_join(cat_check_df, by=c("applicant_id", "season", "activitynum"))

# Collapse down to the activity level (any leadership or any excellence within activities)
activity_level <- activities_joined %>%
  mutate(across(starts_with("cat_"), ~ case_when(
    .x>=1 ~ 1,
    .x==0 ~ 0,
    is.na(.x) ~ 0
  ))) 

# Create summarization function for various levels of the analysis
summarizer <- function(df) {
  df_totals <- df %>%
    group_by(applicant_id, season) %>%
    summarize(count_total=n(),
              cat_1_total=sum(cat_1, na.rm=TRUE),
              cat_2_total=sum(cat_2, na.rm=TRUE),
              cat_5_total=sum(cat_5, na.rm=TRUE)) %>%
    ungroup()
  
  df_bytype <- df %>%
    group_by(applicant_id, season, type_group) %>%
    summarize(across(starts_with("cat_"), ~ sum(.x, na.rm=TRUE)),
              count=n()) %>%
    ungroup() %>%
    pivot_wider(id_cols=c(applicant_id, season), names_from=type_group, 
                names_vary="fastest", values_from=c(count, cat_1, cat_2, cat_5), values_fill=0)
  
  df_analysis <- df_totals %>%
    left_join(df_bytype, by=c("applicant_id", "season")) %>%
    left_join(covariates, by=c("applicant_id", "season"))
  
  df_analysis
}

# Create alphabetizer function so dataframes are consistent in column order
alphabetizer <- function(df) {
  df %>%
    select(
      applicant_id, season, 
      ends_with("_total"),
      ends_with("_academic"),
      ends_with("_arts"),
      ends_with("_athletics"),
      ends_with("_career"),
      ends_with("_cultureid"),
      ends_with("_schoolgov"),
      ends_with("_service"),
      ends_with("_other"),
      everything()
    )
}

# Create various analytic datasets at various levels
activity_level_analysis <- activity_level %>%
  summarizer() %>%
  alphabetizer()

mention_level_analysis <- activities_joined %>%
  summarizer() %>%
  alphabetizer()

binary_level_analysis <- activity_level_analysis %>%
  select(applicant_id, season, starts_with("count_"), starts_with("cat_1_"),
         starts_with("cat_2_"), starts_with("cat_5_")) %>%
  mutate(across(!c(applicant_id, season, count_total), ~ case_when(
    .x>=1 ~ 1,
    .x==0 ~ 0
  ))) %>%
  left_join(covariates, by=c("applicant_id", "season")) %>%
  alphabetizer()

# Proportion analysis takes particular care and separate approach
proportion_level_analysis <- activity_level_analysis 
type_names2 <- c("total", type_names)
cat_names <- c("1", "2", "5")

for(i in type_names2) {
  for(j in cat_names) {
    numeratorvar <- paste0("cat_", j, "_", i)
    denominatorvar <- paste0("count_", i)
    proportion_level_analysis[[numeratorvar]] <- proportion_level_analysis[[numeratorvar]] / proportion_level_analysis[[denominatorvar]]
  }
}

proportion_level_analysis <- proportion_level_analysis %>%
  mutate(across(starts_with("cat_"), 
                ~case_when(
                  is.nan(.x) ~ as.numeric(NA),
                  TRUE ~ .x
                ))) %>%
  alphabetizer()

# Run simple character level analysis dataframe
charlength_prep <- activities_joined %>%
  select(applicant_id, season, activitynum, type_group) %>%
  left_join(charcounts, by=c("applicant_id", "season", "activitynum"))

charlength_totals <- charlength_prep %>%
  group_by(applicant_id, season) %>%
  summarize(chars_total=mean(char_length)) %>%
  ungroup()

charlength_bytype <- charlength_prep %>%
  group_by(applicant_id, season, type_group) %>%
  summarize(char_length=mean(char_length)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c(applicant_id, season), names_from=type_group, 
              names_glue="chars_{type_group}", values_from=char_length)

charlength_level_analysis <- charlength_totals %>%
  left_join(charlength_bytype, by=c("applicant_id", "season")) %>%
  left_join(covariates, by=c("applicant_id", "season")) %>%
  alphabetizer()

# Write files
write_fst(activity_level_analysis, path="data/03_activity_level_analysis.fst")
write_fst(mention_level_analysis, path="data/03_mention_level_analysis.fst")
write_fst(binary_level_analysis, path="data/03_binary_level_analysis.fst")
write_fst(proportion_level_analysis, path="data/03_proportion_level_analysis.fst")
write_fst(charlength_level_analysis, path="data/03_charlength_level_analysis.fst")


