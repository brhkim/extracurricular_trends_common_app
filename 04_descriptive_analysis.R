
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "kableExtra", "fixest"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# Load datasets and exclude international applicants
activity_level <- read_fst("data/03_activity_level_analysis.fst") %>%
  filter(international==0)

mention_level <- read_fst("data/03_mention_level_analysis.fst") %>%
  filter(international==0)

binary_level <- read_fst("data/03_binary_level_analysis.fst") %>%
  filter(international==0)

proportion_level <- read_fst("data/03_proportion_level_analysis.fst") %>%
  filter(international==0)

character_level <- read_fst("data/03_charlength_level_analysis.fst") %>%
  filter(international==0)

# Run basic descriptives function
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

# Create table process function
descriptives_table <- function(df, filenamesuffix, caption_set) {

  col_names_set <- colnames(df) %>%
    data.frame(vars=.) %>%
    mutate(vars=case_when(
      str_detect(vars, "group") ~ "Group",
      str_detect(vars, "students") ~ "Students (N)",
      str_detect(vars, "_total") ~ "All Categories (Total)",
      str_detect(vars, "_academic") ~ "Academic",
      str_detect(vars, "_career") ~ "Career",
      str_detect(vars, "_other") ~ "Other",
      str_detect(vars, "_service") ~ "Service",
      str_detect(vars, "_arts") ~ "Arts",
      str_detect(vars, "_schoolgov") ~ "School Involvement",
      str_detect(vars, "_athletics") ~ "Athletics",
      str_detect(vars, "_cultureid") ~ "Culture and Identity"
    )) %>%
    .$vars
  
  kable(df, digits=c(0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2), col.names = col_names_set, 
        align = "l", format="html", table.attr = "style='width:20%;'",
        caption=paste0("<span style='font-size:20px; font-color:black;'>", caption_set, "</span>")) %>%
    column_spec(1, bold = T) %>%
    kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
    pack_rows("Overall", 1, 1) %>%
    pack_rows("Gender", 2, 3) %>%
    pack_rows("Race/Ethnicity", 4, 10) %>%
    pack_rows("Underrepresented Racially Minoritized Student Status", 11, 12) %>%
    pack_rows("First-Gen Status", 13, 14) %>%
    pack_rows("Fee Waiver Receipt", 15, 16) %>%
    pack_rows("Community Income Level", 17, 18) %>%
    pack_rows("School Type", 19, 21) %>%
    save_kable(paste0("output/04_descriptives_", filenamesuffix, ".jpg"), zoom=3, bs_theme="default")

}

# Run descriptives process summarizer
activity_level_summarized <- activity_level %>%
  descriptive_summarizer() 

activity_level_summarized_high <- activity_level %>%
  filter((test_pctile_bucket=="95-98" | test_pctile_bucket==">=99")) %>%
  descriptive_summarizer() 

# Now create tables for each outcome of interest
activity_level_summarized %>%
  select(group, students, starts_with("count_")) %>%
  descriptives_table(df=., filenamesuffix="activity_count_all", caption_set="Number of Activities Reported by Activity Category")

activity_level_summarized %>%
  select(group, students, starts_with("cat_2_")) %>%
  descriptives_table(df=., filenamesuffix="cat_2_count_all", caption_set="Number of Reported Activities with Top-level Leadership Roles by Activity Category")

activity_level_summarized %>%
  select(group, students, starts_with("cat_1_")) %>%
  descriptives_table(df=., filenamesuffix="cat_1_count_all", caption_set="Number of Reported Activities with Positional Leadership Roles by Activity Category")

activity_level_summarized %>%
  select(group, students, starts_with("cat_5_")) %>%
  descriptives_table(df=., filenamesuffix="cat_5_count_all", caption_set="Number of Reported Activities with Excellence Distinctions by Activity Category")

###

mention_level_summarized <- mention_level %>%
  descriptive_summarizer() 

mention_level_summarized %>%
  select(group, students, starts_with("cat_2_")) %>%
  descriptives_table(df=., filenamesuffix="cat_2_mention_all", caption_set="Mentions of Top-level Leadership Roles by Activity Category")

mention_level_summarized %>%
  select(group, students, starts_with("cat_1_")) %>%
  descriptives_table(df=., filenamesuffix="cat_1_mention_all", caption_set="Mentions of Positional Leadership Roles by Activity Category")

mention_level_summarized %>%
  select(group, students, starts_with("cat_5_")) %>%
  descriptives_table(df=., filenamesuffix="cat_5_mention_all", caption_set="Mentions of Excellence Distinctions by Activity Category")

###

binary_level_summarized <- binary_level %>%
  descriptive_summarizer() 

binary_level_summarized %>%
  select(group, students, starts_with("count_")) %>%
  descriptives_table(df=., filenamesuffix="activity_binary_all", caption_set="Any Activities Reported by Activity Category")

binary_level_summarized %>%
  select(group, students, starts_with("cat_2_")) %>%
  descriptives_table(df=., filenamesuffix="cat_2_binary_all", caption_set="Any Activities with Top-level Leadership Roles by Activity Category")

binary_level_summarized %>%
  select(group, students, starts_with("cat_1_")) %>%
  descriptives_table(df=., filenamesuffix="cat_1_binary_all", caption_set="Any Reported Activities with Positional Leadership Roles by Activity Category")

binary_level_summarized %>%
  select(group, students, starts_with("cat_5_")) %>%
  descriptives_table(df=., filenamesuffix="cat_5_binary_all", caption_set="Any Reported Activities with Excellence Distinctions by Activity Category")

###

proportion_level_summarized <- proportion_level %>%
  descriptive_summarizer() 

proportion_level_summarized_high <- proportion_level %>%
  filter((test_pctile_bucket=="95-98" | test_pctile_bucket==">=99")) %>%
  descriptive_summarizer() 

proportion_level_summarized %>%
  select(group, students, starts_with("cat_2_")) %>%
  descriptives_table(df=., filenamesuffix="cat_2_prop_all", caption_set="Proportion of Reported Activities with Top-level Leadership Roles by Activity Category")

proportion_level_summarized %>%
  select(group, students, starts_with("cat_1_")) %>%
  descriptives_table(df=., filenamesuffix="cat_1_prop_all", caption_set="Proportion of Reported Activities with Positional Leadership Roles by Activity Category")

proportion_level_summarized %>%
  select(group, students, starts_with("cat_5_")) %>%
  descriptives_table(df=., filenamesuffix="cat_5_prop_all", caption_set="Proportion of Reported Activities with Excellence Distinctions by Activity Category")

###

character_level_summarized <- character_level %>%
  descriptive_summarizer() 

character_level_summarized_high <- character_level %>%
  filter((test_pctile_bucket=="95-98" | test_pctile_bucket==">=99")) %>%
  descriptive_summarizer() 

character_level_summarized %>%
  select(group, students, starts_with("chars_")) %>%
  descriptives_table(df=., filenamesuffix="chars_avg_all", caption_set="Average Character Length of Reported Activity Descriptions by Activity Category")

###

# Run visualizations of the above that are more concise
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
  
  ggsave(file=file.path("output", paste0("04_", filename, ".png")), width=10, height=8)
  
}


pointplotter(df=activity_level_summarized,
             varname="count_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             filename="activity_level_count_total")

pointplotter(df=activity_level_summarized_high,
             varname="count_total",
             varlabel="Number of Activities",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             varsubtitle="High SAT/ACT Subsample",
             filename="activity_level_count_total_high")

pointplotter(df=activity_level_summarized,
             varname="cat_2_total",
             varlabel="Number of Activities w/ Top-level Leadership Roles",
             vartitle="Average Number of Activities Reported with Top-level Leadership Roles by Key Applicant Demographics",
             filename="activity_level_cat_2_total")

pointplotter(df=activity_level_summarized_high,
             varname="cat_2_total",
             varlabel="Number of Activities w/ Top-level Leadership Roles",
             vartitle="Average Number of Activities Reported with Top-level Leadership Roles by Key Applicant Demographics",
             varsubtitle="High SAT/ACT Subsample",
             filename="activity_level_cat_2_total_high")

pointplotter(df=proportion_level_summarized,
             varname="cat_2_total",
             varlabel="Proportion of Activities w/ Top-level Leadership Roles",
             vartitle="Average Proportion of Reported Activities with Top-level Leadership Roles by Key Applicant Demographics",
             filename="proportion_level_cat_2_total")

pointplotter(df=proportion_level_summarized_high,
             varname="cat_2_total",
             varlabel="Proportion of Activities w/ Top-level Leadership Roles",
             vartitle="Average Proportion of Reported Activities with Top-level Leadership Roles by Key Applicant Demographics",
             varsubtitle="High SAT/ACT Subsample",
             filename="proportion_level_cat_2_total_high")

pointplotter(df=activity_level_summarized,
             varname="cat_1_total",
             varlabel="Number of Activities w/ Positional Leadership Roles",
             vartitle="Average Number of Activities Reported with Positional Leadership Roles by Key Applicant Demographics",
             filename="activity_level_cat_1_total")

pointplotter(df=proportion_level_summarized,
             varname="cat_1_total",
             varlabel="Proportion of Activities w/ Positional Leadership Roles",
             vartitle="Average Proportion of Reported Activities with Positional Leadership Roles by Key Applicant Demographics",
             filename="proportion_level_cat_1_total")

pointplotter(df=activity_level_summarized,
             varname="cat_5_total",
             varlabel="Number of Activities with Excellence, Honors, and Distinctions",
             vartitle="Average Number of Reported Activities with Excellence, Honors, and Distinctions by Key Applicant Demographics",
             filename="activity_level_cat_5_total")

pointplotter(df=activity_level_summarized_high,
             varname="cat_5_total",
             varlabel="Number of Activities with Excellence, Honors, and Distinctions",
             vartitle="Average Number of Activities Reported by Key Applicant Demographics",
             varsubtitle="High SAT/ACT Subsample",
             filename="activity_level_cat_5_total_high")

pointplotter(df=proportion_level_summarized,
             varname="cat_5_total",
             varlabel="Proportion of Activities with Excellence, Honors, and Distinctions",
             vartitle="Average Proportion of Reported Activities with Excellence, Honors, and Distinctions by Key Applicant Demographics",
             filename="proportion_level_cat_5_total")

pointplotter(df=proportion_level_summarized_high,
             varname="cat_5_total",
             varlabel="Proportion of Activities with Excellence, Honors, and Distinctions",
             vartitle="Average Proportion of Reported Activities with Excellence, Honors, and Distinctions by Key Applicant Demographics",
             varsubtitle="High SAT/ACT Subsample",
             filename="proportion_level_cat_5_total_high")

pointplotter(df=character_level_summarized,
             varname="chars_total",
             varlabel="Average Number of Characters in Activity Description",
             vartitle="Average Number of Characters in Activity Description",
             filename="character_level_chars_total")


###

# Make parallel function for activities by type instead
pointplotter_types <- function(df, vargroup, varlabel, vartitle, varsubtitle="", filename) {
  group_levels <- c("White", "Asian", "Black", "Latinx", "Native Hawaiian or Other Pacfiic Islander",
                    "American Indian or Alaska Native", "Two or More Races", "Non-URM", "URM", 
                    "Continuing-gen", "First-gen", "Non-Recipient", "Recipient", "Public", "Private")
  
  plot_df <- df %>%
    filter(group %in% group_levels) %>%
    mutate(group=factor(group, levels=group_levels),
           students=as.numeric(students)) %>%
    select(group, students, starts_with(vargroup), -ends_with("_total")) %>%
    pivot_longer(cols=starts_with(vargroup), names_to="activitytype", names_prefix=vargroup, values_to="value") %>%
    mutate(activitytype=factor(activitytype, levels=c("academic", "arts", "athletics", "career",
                                                      "cultureid", "schoolgov", "service", "other"),
                               labels=c("Academic", "Arts", "Athletics", "Career", 
                                        "Culture/Identity", "School Gov/Spirit", "Service", "Other")))
  
  max_amt <- max(plot_df$value) + (.1 *max(plot_df$value))
  
  ggplot(data=plot_df, aes(x=group, y=value, shape=activitytype, color=activitytype, fill=activitytype)) + 
    geom_point(size=2) +
    geom_vline(xintercept=7.5) +
    geom_vline(xintercept=9.5) +
    geom_vline(xintercept=11.5) +
    geom_vline(xintercept=13.5) +
    scale_x_discrete(labels=function(x) str_wrap(x, width=25)) +
    scale_shape_manual(values = c(15, 4, 5, 6, 8, 16, 17, 12)) + 
    scale_size_continuous(labels = scales::comma) +
    scale_y_continuous(limits=c(0, max_amt)) + 
    labs(
      title=str_wrap(vartitle, width=80),
      subtitle=str_wrap(varsubtitle, width=80),
      x="\nStudent Group",
      y=paste0(str_wrap(varlabel, width=80), "\n"),
      shape="Activity Type",
      color="Activity Type",
      fill="Activity Type"
    ) + 
    theme_bw(base_size=13) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  ggsave(file=file.path("output", paste0("04_", filename, ".png")), width=10, height=8)
  
}

pointplotter_types(df=activity_level_summarized,
                   vargroup="count_",
                   varlabel="Average Reported Activities by Type",
                   vartitle="Average Reported Activities by Type Across Key Applicant Demographics",
                   filename="activity_level_count_types")

pointplotter_types(df=activity_level_summarized_high,
                   vargroup="count_",
                   varlabel="Average Reported Activities by Type",
                   vartitle="Average Reported Activities by Type Across Key Applicant Demographics",
                   varsubtitle="High SAT/ACT Subsample",
                   filename="activity_level_count_types_high")

pointplotter_types(df=activity_level_summarized,
                   vargroup="cat_2_",
                   varlabel="Number of Activities w/ Top-level Leadership Roles",
                   vartitle="Average Reported Activities w/ Top-level Leadership Roles by Type Across Key Applicant Demographics",
                   filename="activity_level_cat_2_types")

pointplotter_types(df=activity_level_summarized_high,
                   vargroup="cat_2_",
                   varlabel="Number of Activities w/ Top-level Leadership Roles",
                   vartitle="Average Reported Activities w/ Top-level Leadership Roles by Type Across Key Applicant Demographics",
                   varsubtitle="High SAT/ACT Subsample",
                   filename="activity_level_cat_2_types_high")

pointplotter_types(df=activity_level_summarized,
                   vargroup="cat_5_",
                   varlabel="Number of Activities w/ Excellence",
                   vartitle="Average Reported Activities w/ Excellence by Type Across Key Applicant Demographics",
                   filename="activity_level_cat_5_types")

pointplotter_types(df=activity_level_summarized_high,
                   vargroup="cat_5_",
                   varlabel="Number of Activities w/ Excellence",
                   vartitle="Average Reported Activities w/ Excellence by Type Across Key Applicant Demographics",
                   varsubtitle="High SAT/ACT Subsample",
                   filename="activity_level_cat_5_types_high")

pointplotter_types(df=proportion_level_summarized,
                   vargroup="cat_2_",
                   varlabel="Proportion of Activities w/ Top-level Leadership Roles",
                   vartitle="Proportion of Reported Activities w/ Top-level Leadership Roles by Type Across Key Applicant Demographics",
                   filename="proportion_level_cat_2_types")

pointplotter_types(df=proportion_level_summarized,
                   vargroup="cat_5_",
                   varlabel="Proportion of Activities w/ Excellence",
                   vartitle="Proportion of Reported Activities w/ Excellence by Type Across Key Applicant Demographics",
                   filename="proportion_level_cat_5_types")



pointplotter_types(df=character_level_summarized,
                   vargroup="chars_",
                   varlabel="Average Number of Characters in Activity Description",
                   vartitle="Average Number of Characters in Activity Description",
                   filename="character_level_chars_types")

pointplotter_types(df=character_level_summarized_high,
                   vargroup="chars_",
             varlabel="Average Number of Characters in Activity Description",
             vartitle="Average Number of Characters in Activity Description",
             varsubtitle="High SAT/ACT Subsample",
             filename="character_level_chars_types_high")
