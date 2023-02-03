
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# Read in files
applicants <- read_fst("data/01_applicants.fst")
applications <- read_fst("data/01_applications.fst")
applicantprofile <- read_fst("data/01_applicantprofile.fst")
members <- read_fst("data/01_members.fst")
zipincome <- read_fst(path="data/01_zipincome.fst")

# Create a cleaned dataset with transformed variables of interest
demographics <- applicants %>%
  left_join(applicantprofile %>% select(applicant_id, highschool_number_other_high_schools), by="applicant_id") %>%
  mutate(
    female = case_when(
      applicant_sex=="Female" ~ 1,
      applicant_sex=="Male" ~ 0,
      is.na(applicant_sex) ~ 0),
    female_miss=case_when(
      is.na(applicant_sex) ~ 1,
      TRUE ~ 0),
    international=case_when(
      applicant_citizenship=="International" ~ 1,
      TRUE ~ 0),
    urm=applicant_raceethnicity_isurm,
    race_miss=case_when(
      applicant_raceethnicity=="Nonresident Alien" | applicant_raceethnicity=="Unknown" ~ 1,
      TRUE ~ 0),
    white=case_when(
      applicant_raceethnicity=="White" ~ 1,
      TRUE ~ 0),
    black=case_when(
      applicant_raceethnicity=="Black or African American" ~ 1,
      TRUE ~ 0),
    asian=case_when(
      applicant_raceethnicity=="Asian" ~ 1,
      TRUE ~ 0),
    latinx=case_when(
      applicant_raceethnicity=="Latinx" ~ 1,
      TRUE ~ 0),
    amerind=case_when(
      applicant_raceethnicity=="American Indian or Alaska Native" ~ 1,
      TRUE ~ 0),
    nhopi=case_when(
      applicant_raceethnicity=="Native Hawaiian or Other Pacific Islander" ~ 1,
      TRUE ~ 0),
    twomoreraces=case_when(
      applicant_raceethnicity=="Two or More Races" ~ 1,
      TRUE ~ 0),
    #other_race=case_when(
    #  applicant_raceethnicity=="American Indian or Alaska Native" | applicant_raceethnicity=="Native Hawaiian or Other Pacific Islander" | applicant_raceethnicity=="Two or More Races" ~ 1,
    #  TRUE ~ 0),
    firstgen=case_when(
      applicant_firstgenstatus=="First-generation" ~ 1,
      TRUE ~ 0),
    firstgen_miss=case_when(
      is.na(applicant_firstgenstatus) ~ 1,
      TRUE ~ 0),
    school_public=case_when(
      applicant_highschooltype=="Public" | applicant_highschooltype=="Charter" ~ 1,
      TRUE ~ 0),
    school_private=case_when(
      applicant_highschooltype=="Independent" | applicant_highschooltype=="Religious" ~ 1,
      TRUE ~ 0),
    school_other=case_when(
      applicant_highschooltype=="Home School" | applicant_highschooltype=="Unknown" ~ 1,
      TRUE ~ 0),
    senior=case_when(
      applicant_classyr=="Senior" ~ 1,
      TRUE ~ 0),
    multischool=case_when(
      highschool_number_other_high_schools!=0 & !is.na(highschool_number_other_high_schools) ~ 1,
      TRUE ~ 0),
    multischool_miss=case_when(
      is.na(highschool_number_other_high_schools) ~ 1,
      TRUE ~ 0),
    feewaiver=case_when(
      applicant_anycommonappfeewaiver=="Yes" ~ 1,
      applicant_anycommonappfeewaiver=="No" ~ 0),
    hs_ceeb_season=paste0(applicant_highschoolceebcode, "_", season)
  ) %>%
  select(applicant_id, season, hs_ceeb=applicant_highschoolceebcode, hs_ceeb_season, female, female_miss, international, applicant_zip5digit, feewaiver,
         race_miss, urm, white, black, asian, latinx, amerind, nhopi, twomoreraces, 
         firstgen, firstgen_miss, school_public, school_private, school_other, 
         senior, multischool, multischool_miss) %>%
  mutate(applicant_id=as.numeric(applicant_id))

# Process class ranks and gpa buckets
classrank_gpa <- applicantprofile %>%
  # First convert rank variables into numeric as necessary
  mutate(
    class_rank=as.numeric(class_rank_exact),
    class_size=as.numeric(class_rank_classsize),
    rank_pctile=class_rank/class_size) %>%
  # Construct binary indicators by quintile, prioritizing calculated percentile first
  mutate(
    quintile_rank_clean=case_when(
      rank_pctile<=0.01 ~ "Top 1%",
      rank_pctile>0.01 & rank_pctile<=0.05 ~ "Top 5%",
      rank_pctile>0.05 & rank_pctile<=0.1 ~ "Top 10%",
      rank_pctile>0.1 & rank_pctile<=0.2 ~ "Top 20%",
      rank_pctile>0.2 & rank_pctile<=0.4 ~ "Top 40%",
      rank_pctile>0.4 & rank_pctile<=0.6 ~ "Top 60%",
      rank_pctile>0.6 & rank_pctile<=0.8 ~ "Top 80%",
      rank_pctile>0.8 & rank_pctile<=1.0 ~ "Top 100%")) %>%
  # Now fill in binary indicators if missing and other rank variables are available
  mutate(
    quintile_rank_clean=case_when(
      !is.na(quintile_rank_clean) ~ quintile_rank_clean,
      is.na(quintile_rank_clean) & (class_rank_decile=="Top 10%") ~ "Top 10%",
      is.na(quintile_rank_clean) & (class_rank_decile=="Top 20%" | class_rank_quintile=="Top 20%" | class_rank_quartile=="Top 25%") ~ "Top 20%",
      is.na(quintile_rank_clean) & (class_rank_decile=="Top 30%" | class_rank_decile=="Top 40%" | class_rank_quintile=="Top 40%" | class_rank_quartile=="Top 50%") ~ "Top 40%",
      is.na(quintile_rank_clean) & (class_rank_decile=="Top 50%" | class_rank_decile=="Top 60%" | class_rank_quintile=="Top 60%") ~ "Top 60%",
      is.na(quintile_rank_clean) & (class_rank_decile=="Top 70%" | class_rank_decile=="Top 80%" | class_rank_quintile=="Top 80%" | class_rank_quartile=="Top 75%") ~ "Top 80%",
      is.na(quintile_rank_clean) & (class_rank_decile=="Top 90%" | class_rank_decile=="Top 100%" | class_rank_quintile=="Top 100%" | class_rank_quartile=="Top 100%") ~ "Top 100%",
      TRUE ~ "Missing")) %>%
  mutate(
    quintile_rank_clean=case_when(
      quintile_rank_clean=="Top 100%" ~ "Top 40%", 
      quintile_rank_clean=="Top 80%" ~ "Top 40%", 
      quintile_rank_clean=="Top 60%" ~ "Top 40%",
      TRUE ~ quintile_rank_clean)
  ) %>%
  # Create ordered factor variable
  mutate(
    quintile_rank_clean=factor(quintile_rank_clean, levels=c("Missing", "Top 40%", "Top 20%", "Top 10%", "Top 5%", "Top 1%"))) %>%
  # Make cumulative GPA numeric
  mutate(
    cumulative_gpa=as.numeric(gpa_cumulative),
    # Get cumulative GPA into reasonable buckets by score ranges
    gpa_scaled=case_when(
      cumulative_gpa>1 & cumulative_gpa<=6 ~ cumulative_gpa/4,
      cumulative_gpa>7 & cumulative_gpa<=15 ~ cumulative_gpa/10,
      cumulative_gpa>50 & cumulative_gpa<=150 ~ cumulative_gpa/100),
    # Create bins of GPAs
    gpa_bucket=case_when(
      is.na(gpa_scaled) ~ "Other/Missing",
      gpa_scaled==1 ~ "1.00",
      gpa_scaled>=0.9 & gpa_scaled<1 ~ "0.90-0.99",
      gpa_scaled<0.9 ~ "<0.90",
      gpa_scaled>1 ~ ">1.00",
      TRUE ~ "Other/Missing")) %>%
  # Create ordered factor variable
  mutate(
    gpa_bucket=factor(gpa_bucket, levels=c("Other/Missing",  "<0.90", "0.90-0.99", "1.00", ">1.00"))) %>%
  select(applicant_id, quintile_rank_clean, gpa_bucket, gpa_scaled) %>%
  rename(quintile_rank=quintile_rank_clean) %>%
  mutate(applicant_id=as.numeric(applicant_id))

# Process SAT/ACT scores
sat_act <- applicants %>%
  # First get rid of any erroneous SAT scores just in case
  mutate(
    applicant_sat1600actconverted=as.numeric(applicant_sat1600actconverted),
    applicant_sat1600actconverted=case_when(
      applicant_sat1600actconverted<400 ~ as.numeric(NA),
      applicant_sat1600actconverted>1600 ~ as.numeric(NA),
      TRUE ~ applicant_sat1600actconverted)) %>%
  mutate(test_pctile_bucket=case_when(
    applicant_sat1600actconverted>=1500 ~ ">=99",
    applicant_sat1600actconverted>=1410 ~ "95-98",
    applicant_sat1600actconverted>=1340 ~ "90-94",
    applicant_sat1600actconverted>=1210 ~ "75-89",
    applicant_sat1600actconverted<1210 ~ "<75",
    TRUE ~ "Missing"
  ),
  test_pctile_bucket=factor(test_pctile_bucket, levels=c("Missing", "<75", "75-89", "90-94", "95-98", ">=99"))
  ) %>%
  select(applicant_id, applicant_sat1600actconverted, test_pctile_bucket) %>%
  mutate(applicant_id=as.numeric(applicant_id))
  
# Process application profiles
application_profiles <- applications %>%
  left_join(members, by="member_id") %>%
  select(applicant_id, member_id, member_ugsat1600) %>%
  mutate(test_miss=case_when(
    is.na(member_ugsat1600) ~ 1,
    TRUE ~ 0)) %>%
  # Get back down to the applicant-level characteristics
  group_by(applicant_id) %>%
  summarize(
    total_apps=n(),
    total_missing_satbin=sum(test_miss),
    prop_miss=total_missing_satbin/total_apps,
    total_nonmiss=total_apps-total_missing_satbin,
    satbin=mean(member_ugsat1600, na.rm=TRUE),
    # Note that we don't want to give an satbin when most of the colleges a student's applying to is missing data
    satbin=case_when(
      prop_miss>0.5 & total_nonmiss<2 ~ as.numeric(NA),
      !is.na(satbin) ~ satbin)) %>%
  ungroup() %>%
  select(applicant_id, total_apps, satbin) %>%
  # Get the satbin variable into quintiles
  mutate(
    satbin_quintile=ntile(satbin, 5),
    satbin_quintile=case_when(
      is.na(satbin_quintile) ~ "Missing",
      !is.na(satbin_quintile) ~ as.character(satbin_quintile)),
    satbin_quintile=factor(satbin_quintile,
                           levels=c("Missing", "1", "2", "3", "4", "5")),
    total_apps=case_when(
      total_apps>=1 & total_apps<=3 ~ "1-3",
      total_apps>=4 & total_apps<=7 ~ "4-7",
      total_apps>=8 ~ ">=8"
    )
  ) %>% 
  mutate(total_apps=factor(total_apps, levels=c("1-3", "4-7", ">=8"))) %>%
  # Rename it to not get confusing with their actual SAT score quintiles
  rename(app_selectivity_quintile=satbin_quintile) %>%
  select(-satbin) %>%
  mutate(applicant_id=as.numeric(applicant_id))

# create final covariates dataframe
covariates <- demographics %>%
  left_join(classrank_gpa, by="applicant_id") %>%
  left_join(sat_act, by="applicant_id") %>%
  left_join(application_profiles, by="applicant_id") %>%
  left_join(zipincome, by="applicant_zip5digit") %>%
  mutate(highest_income_quintile_miss=case_when(
    is.na(median_hh_income_quintile) ~ 1,
    TRUE ~ 0
  ),
  highest_income_quintile=case_when(
    median_hh_income_quintile==5 ~ 1,
    TRUE ~ 0
  ))

# Write it
write_fst(covariates, path="data/02_covariates.fst")

