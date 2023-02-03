
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "kableExtra", "fixest"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# Need to first basically replicate 03_analytic_data.R
activities <- read_fst("data/01_activities.fst") %>%
  mutate(activitynum=as.numeric(activitynum)) %>%
  filter(!is.na(honors) & honors!="" &
           !is.na(details) & details!="")

cat_check_df <- read_fst(path="data/02f_cat_check_output.fst")

covariates <- read_fst("data/02_covariates.fst")

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

activity_level <- activities_joined %>%
  mutate(across(starts_with("cat_"), ~ case_when(
    .x>=1 ~ 1,
    .x==0 ~ 0,
    is.na(.x) ~ 0
  ))) 

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
  
  df_analysis %>%
    filter(international==0)
}

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
  
analytic_all <- activity_level %>%
  summarizer() %>%
  alphabetizer()

# Now we want comparison datasets for when we look only at activities the applicant intends to continue in college...
analytic_continue <- activity_level %>%
  filter(continue=="Yes") %>%
  summarizer() %>%
  alphabetizer()

# Now we want comparison datasets for when we look only at activities the applicant participated in both junior and senior years
analytic_jrsr <- activity_level %>%
  filter(str_detect(grade, "2") & str_detect(grade, "3")) %>%
  summarizer() %>%
  alphabetizer()

# Now we want comparison datasets for when we look only at activities the applicant participated in for at least 5hrs/week
analytic_hours5 <- activity_level %>%
  filter(total_hours/years>=260) %>%
  summarizer() %>%
  alphabetizer()

# Now we want comparison datasets for when we look only at activities that satisfy a combo of the above criteria
analytic_combo <- activity_level %>%
  filter(continue=="Yes") %>%
  filter(str_detect(grade, "2") & str_detect(grade, "3")) %>%
  filter(total_hours/years>=260) %>%
  summarizer() %>%
  alphabetizer()

# Another combo dataset comparison
analytic_continuejrsr <- activity_level %>%
  filter(str_detect(grade, "2") & str_detect(grade, "3")) %>%
  filter(continue=="Yes") %>%
  summarizer() %>%
  alphabetizer()

# Run basic descriptives:
descriptive_summarizer <- function(dataset) {
  subchain <- . %>%
    summarize(group="", students=n(), 
              across(starts_with("count_") | starts_with("cat_1_") | 
                       starts_with("cat_2_") | starts_with("cat_5_") | 
                       starts_with("prop_2_") | starts_with("chars_"), ~mean(.x, na.rm=TRUE)))
              
  descriptives <- dataset %>%
    subchain()
  
  descriptives[2,] <- dataset %>%
    filter(female==0) %>%
    subchain()
  
  descriptives[3,] <- dataset %>%
    filter(female==1) %>%
    subchain()
  
  descriptives[4,] <- dataset %>%
    filter(white==1) %>%
    subchain()
  
  descriptives[5,] <- dataset %>%
    filter(black==1) %>%
    subchain()
  
  descriptives[6,] <- dataset %>%
    filter(asian==1) %>%
    subchain()
  
  descriptives[7,] <- dataset %>%
    filter(latinx==1) %>%
    subchain()
  
  descriptives[8,] <- dataset %>%
    filter(amerind==1) %>%
    subchain()
  
  descriptives[9,] <- dataset %>%
    filter(nhopi==1) %>%
    subchain()
  
  descriptives[10,] <- dataset %>%
    filter(twomoreraces==1) %>%
    subchain()
  
  descriptives[11,] <- dataset %>%
    filter(urm==0 & race_miss==0) %>%
    subchain()
  
  descriptives[12,] <- dataset %>%
    filter(urm==1 & race_miss==0) %>%
    subchain()
  
  descriptives[13,] <- dataset %>%
    filter(firstgen==0 & firstgen_miss==0) %>%
    subchain()
  
  descriptives[14,] <- dataset %>%
    filter(firstgen==1 & firstgen_miss==0) %>%
    subchain()
  
  descriptives[15,] <- dataset %>%
    filter(feewaiver==0) %>%
    subchain()
  
  descriptives[16,] <- dataset %>%
    filter(feewaiver==1) %>%
    subchain()
  
  descriptives[17,] <- dataset %>%
    filter(highest_income_quintile==1 & highest_income_quintile_miss==0) %>%
    subchain()
  
  descriptives[18,] <- dataset %>%
    filter(highest_income_quintile==0 & highest_income_quintile_miss==0) %>%
    subchain()
  
  descriptives[19,] <- dataset %>%
    filter(school_public==1) %>%
    subchain()
  
  descriptives[20,] <- dataset %>%
    filter(school_private==1) %>%
    subchain()
  
  descriptives[21,] <- dataset %>%
    filter(school_other==1) %>%
    subchain()
  
  descriptives$group <- c("All", "Male", "Female", "White", "Black", "Asian", "Latinx", "American Indian or Alaska Native", 
                          "Native Hawaiian or Other Pacfiic Islander", "Two or More Races", "Non-URM", "URM",
                          "Continuing-gen", "First-gen", "Non-Recipient", "Recipient", "Highest Quintile", "Other Quintiles", 
                          "Public", "Private", "Other School Type")
  
  descriptives <- descriptives %>%
    relocate(group, everything()) 
  
  descriptives
}

# Generate summarized datasets for each of the comparison datasets
activity_level_summarized_all <- analytic_all %>%
  descriptive_summarizer() 

activity_level_summarized_continue <- analytic_continue %>%
  descriptive_summarizer() 

activity_level_summarized_jrsr <- analytic_jrsr %>%
  descriptive_summarizer() 

activity_level_summarized_hours5 <- analytic_hours5 %>%
  descriptive_summarizer() 

activity_level_summarized_continuejrsr <- analytic_continuejrsr %>%
  descriptive_summarizer() 

activity_level_summarized_combo <- analytic_combo %>%
  descriptive_summarizer() 

# Run simple viz on each of the comparison datasets for visual comparison
pointplotter <- function(df, varname, varlabel, vartitle, varsubtitle="", filename) {
  group_levels <- c("White", "Asian", "Black", "Latinx", "Native Hawaiian or Other Pacfiic Islander",
                    "American Indian or Alaska Native", "Two or More Races", "Non-URM", "URM", 
                    "Continuing-gen", "First-gen", "Non-Recipient", "Recipient", "Public", "Private")
  
  plot_df <- df %>%
    filter(group %in% group_levels) %>%
    mutate(group=factor(group, levels=group_levels),
           students=as.numeric(students))
  
  nudge_amt <- (max(plot_df[[varname]])) / 14
  max_amt <- max(plot_df[[varname]]) + (.1 *max(plot_df[[varname]]))
  
  ggplot(data=plot_df, aes(x=group, y=get(varname), size=students)) + 
    geom_text(aes(label=round(get(varname), 2)), color="black", nudge_y=nudge_amt, 
              size=3, angle=45, hjust=0.5, vjust=0.5) + 
    geom_point() +
    geom_vline(xintercept=7.5) +
    geom_vline(xintercept=9.5) +
    geom_vline(xintercept=11.5) +
    geom_vline(xintercept=13.5) +
    scale_x_discrete(labels=function(x) str_wrap(x, width=25)) +
    scale_size_continuous(labels = scales::comma) +
    scale_y_continuous(limits=c(0, max_amt)) + 
    labs(
      title=str_wrap(vartitle, width=80),
      subtitle=str_wrap(varsubtitle, width=80),
      x="\nStudent Group",
      y=paste0(str_wrap(varlabel, width=80), "\n"),
      size="Number of\nStudents",
    ) + 
    theme_bw(base_size=13) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  ggsave(file=file.path("output", paste0("06_", filename, ".png")), width=10, height=8)
  
}

pointplotter(df=activity_level_summarized_all,
             varname="count_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_count_total_all")

pointplotter(df=activity_level_summarized_continue,
             varname="count_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_count_total_continue")


pointplotter(df=activity_level_summarized_jrsr,
             varname="count_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_count_total_jrsr")

pointplotter(df=activity_level_summarized_hours5,
             varname="count_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_count_total_hours5")

pointplotter(df=activity_level_summarized_continuejrsr,
             varname="count_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_count_total_continuejrsr")

pointplotter(df=activity_level_summarized_combo,
             varname="count_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_count_total_combo")

####



pointplotter(df=activity_level_summarized_all,
             varname="cat_2_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_2_total_all")

pointplotter(df=activity_level_summarized_continue,
             varname="cat_2_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_2_total_continue")


pointplotter(df=activity_level_summarized_jrsr,
             varname="cat_2_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_2_total_jrsr")

pointplotter(df=activity_level_summarized_hours5,
             varname="cat_2_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_2_total_hours5")

pointplotter(df=activity_level_summarized_continuejrsr,
             varname="cat_2_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_2_total_continuejrsr")

pointplotter(df=activity_level_summarized_combo,
             varname="cat_2_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_2_total_combo")


###

pointplotter(df=activity_level_summarized_all,
             varname="cat_5_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_5_total_all")

pointplotter(df=activity_level_summarized_continue,
             varname="cat_5_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_5_total_continue")


pointplotter(df=activity_level_summarized_jrsr,
             varname="cat_5_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_5_total_jrsr")

pointplotter(df=activity_level_summarized_hours5,
             varname="cat_5_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_5_total_hours5")

pointplotter(df=activity_level_summarized_continuejrsr,
             varname="cat_5_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_5_total_continuejrsr")

pointplotter(df=activity_level_summarized_combo,
             varname="cat_5_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_cat_5_total_combo")

