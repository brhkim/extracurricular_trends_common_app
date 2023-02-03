
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "readxl", "quanteda", "tidytext", "kableExtra"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

activity_level_analysis <- read_fst(path="data/03_activity_level_analysis.fst") %>%
  filter(international==0)

# Make a summarizer subchain
sample_descriptives <- . %>%
  summarize(Applicants=n_distinct(applicant_id),
            Activities=sum(count_total),
            `Activities Per Applicant`=Activities/Applicants,
            Female=mean(female),
            `First Generation`=mean(firstgen),
            `Fee Waiver Recipient`=mean(feewaiver),
            `Highest Income Community`=mean(highest_income_quintile),
            `Missing Community Income`=mean(highest_income_quintile_miss),
            White=mean(white),
            Black=mean(black),
            Latinx=mean(latinx),
            Asian=mean(asian),
            `American Indian or Alaska Native`=mean(amerind),
            `Native Hawaiian or Other Pacific Islander`=mean(nhopi),
            `Two or More Races`=mean(twomoreraces),
            URM=mean(urm),
            Missing=mean(race_miss),
            `Public School`=mean(school_public),
            `Private School`=mean(school_private),
            `Other School`=mean(school_other),

            total_apps_a=length(.$total_apps[.$total_apps=="1-3"])/length(.$applicant_id),
            total_apps_b=length(.$total_apps[.$total_apps=="4-7"])/length(.$applicant_id),
            total_apps_c=length(.$total_apps[.$total_apps==">=8"])/length(.$applicant_id),
            
            gpa_bucket_a=length(.$gpa_bucket[.$gpa_bucket=="Other/Missing"])/length(.$applicant_id),
            gpa_bucket_b=length(.$gpa_bucket[.$gpa_bucket=="<0.90"])/length(.$applicant_id),
            gpa_bucket_c=length(.$gpa_bucket[.$gpa_bucket=="0.90-0.99"])/length(.$applicant_id),
            gpa_bucket_d=length(.$gpa_bucket[.$gpa_bucket=="1.00"])/length(.$applicant_id),
            gpa_bucket_e=length(.$gpa_bucket[.$gpa_bucket==">1.00"])/length(.$applicant_id),
            
            test_bucket_a=length(.$test_pctile_bucket[.$test_pctile_bucket=="Missing"])/length(.$applicant_id),
            test_bucket_b=length(.$test_pctile_bucket[.$test_pctile_bucket=="<75"])/length(.$applicant_id),
            test_bucket_c=length(.$test_pctile_bucket[.$test_pctile_bucket=="75-89"])/length(.$applicant_id),
            test_bucket_d=length(.$test_pctile_bucket[.$test_pctile_bucket=="90-94"])/length(.$applicant_id),
            test_bucket_e=length(.$test_pctile_bucket[.$test_pctile_bucket=="95-98"])/length(.$applicant_id),
            test_bucket_f=length(.$test_pctile_bucket[.$test_pctile_bucket==">=99"])/length(.$applicant_id),
            
  ) %>%
  mutate(across(!c("Applicants", "Activities"), round, 3)) %>%
  # mutate(across(c(Female, `First Generation`, White, Black, Latinx, Asian, Other, Missing, URM,
  #                 `From Highest Income Quintile Community`, `Missing Community Income`,
  #                 `Fee Waiver Recipient`, `Public School`, `Private School`, `Other School`), round, 3)) %>%
  # mutate(across(starts_with("total_apps"), round, 3)) %>%
  # mutate(across(starts_with("gpa_bucket"), round, 3)) %>%
  # mutate(across(starts_with("test_bucket"), round, 3)) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  stack() %>%
  relocate(ind, values) %>%
  rename(Variable=ind, Values=values) %>%
  mutate(Variable=case_when(
    Variable=="total_apps_a" ~ "1-3",
    Variable=="total_apps_b" ~ "4-7",
    Variable=="total_apps_c" ~ ">=8",
    Variable=="gpa_bucket_a" ~ "Other/Missing",
    Variable=="gpa_bucket_b" ~ "<0.90",
    Variable=="gpa_bucket_c" ~ "0.90-0.99",
    Variable=="gpa_bucket_d" ~ "1.00",
    Variable=="gpa_bucket_e" ~ ">1.00",
    Variable=="test_bucket_a" ~ "Missing",
    Variable=="test_bucket_b" ~ "<75",
    Variable=="test_bucket_c" ~ "75-89",
    Variable=="test_bucket_d" ~ "90-94",
    Variable=="test_bucket_e" ~ "95-98",
    Variable=="test_bucket_f" ~ ">=99",
    TRUE ~ as.character(Variable)
  ))

# Run descriptives subchain for 2018, 2019, and combined
descriptives_2018_prep <- activity_level_analysis %>%
  filter(season==2018) %>%
  sample_descriptives()

descriptives_2019_prep <- activity_level_analysis %>%
  filter(season==2019) %>%
  sample_descriptives()

descriptives_pooled_prep <- activity_level_analysis %>%
  sample_descriptives()

# Run separately for high achievers just in case
descriptives_highachiever_prep <- activity_level_analysis %>%
  filter((test_pctile_bucket=="95-98" | test_pctile_bucket==">=99")) %>%
  sample_descriptives()

# Create final table
sample_descriptives_table <- bind_cols(descriptives_2018_prep, descriptives_2019_prep, 
                                       descriptives_pooled_prep, descriptives_highachiever_prep) %>%
  select(-Variable...3, -Variable...5, -Variable...7) %>%
  rename(Variable=`Variable...1`, `2018`=Values...2, `2019`=Values...4, `Full Sample`=Values...6, `High Test Score`=Values...8)

# Write two separate tables so they can fit on one page side-by-side
kable(sample_descriptives_table[1:20,],
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'", background="white",
      caption=paste0("<span style='font-size:20px; font-color:black;'>Sample Descriptives A</span>")) %>%
  column_spec(1, bold = T, width_max="2in") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Sample", 1, 3) %>%
  pack_rows("Student Demographics", 4, 8) %>%
  pack_rows("Student Race/Ethnicity", 9, 17) %>%
  pack_rows("Student School Sector", 18, 20) %>%
  save_kable("output/03b_sample_descriptives_a.jpg", zoom=3, bs_theme="default")

kable(sample_descriptives_table[21:34,], 
      align = "l", format="html", row.names=FALSE, table.attr = "style='width:20%;'", background="white",
      caption=paste0("<span style='font-size:20px; font-color:black;'>Sample Descriptives B</span>")) %>%
  column_spec(1, bold = T, width_max="2in") %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  pack_rows("Applications Sent", 1, 3) %>%
  pack_rows("Scaled GPA Group", 4, 8) %>%
  pack_rows("SAT/ACT Percentile Group", 9, 14) %>%
  save_kable("output/03b_sample_descriptives_b.jpg", zoom=3, bs_theme="default")

