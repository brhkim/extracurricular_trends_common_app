
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "odbc", "DBI", "dbplyr", "tidylog", "fst"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")
# The following script creates the connection needed to our data warehouse
source("~/DAR_BK/redshift_connect_bk.R")

# Get institutional data and filter sample
members <- tbl(con, in_schema('prod_bidw_zone3_external', 'dar_member')) %>%
  mutate(selective_member=case_when(
    member_ugadmitrate<=40 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(season==2018) %>%
  select(member_id, member_darmemberstatusflag, member_name, member_state,
         member_ugadmitrate, member_schooltype, member_ugsat1600)

# Create a manual list of selective members by our prior criterion
selective_members_list <- members %>%
  filter(selective_member==1) %>%
  .$member_id

applications <- tbl(con, in_schema('prod_bidw_zone3_external', 'dar_application')) %>%
  filter((season==2018 | season==2019) & application_submittedflag==1) %>%
  select(applicant_id, season, member_id) %>%
  # We only want applicants who are in their latest application season
  group_by(applicant_id) %>%
  mutate(latest_season=max(season, na.rm=TRUE)) %>%
  # So get rid of any applications not from their latest season
  filter(season==latest_season) %>%
  select(-latest_season) %>%
  ungroup()

# Get a list of applicants who submitted to at least one selective member in their latest season only
applicant_list <- applications %>%
  filter(member_id %in% selective_members_list) %>%
  select(applicant_id, season) %>%
  distinct()

# Get applicant demographics
applicants <- tbl(con, in_schema('prod_bidw_zone3_external', 'dar_applicant')) %>%
  right_join(applicant_list, by=c("applicant_id", "season")) %>%
  select(applicant_id, season, applicant_highschoolceebcode, applicant_anycommonappfeewaiver, 
         applicant_citizenship, applicant_zip5digit,
         applicant_raceethnicity_isurm,
         applicant_classyr, applicant_highschooltype, applicant_country,
         applicant_firstgenstatus, applicant_raceethnicity,
         applicant_sex, applicant_sat1600actconverted)

# Get additional applicant demographics
applicantprofile <- tbl(con, in_schema('prod_bidw_zone3_external', 'dar_applicantprofile')) %>%
  right_join(applicant_list, by=c("applicant_id", "season")) %>%
  select(applicant_id, season, applicant_visatype, gpa_cumulative, gpa_scale, gpa_weighting, highschool_number_other_high_schools,
         class_rank_exact, class_rank_classsize, class_rank_decile, class_rank_quartile, class_rank_quintile)
  
# Get applicant activities
activities <- tbl(con, in_schema('prod_bidw_zone3_external', 'jp_activities')) %>%
  right_join(applicant_list, by=c("applicant_id", "season")) %>%
  select(applicant_id, season, starts_with("activity_"))

# Get applicant zipcode demographics
zipincome <- tbl(con, in_schema('prod_bidw_dar_adhoc_internal', 'census_acszipcode')) %>%
  select(applicant_zip5digit=zip_code, median_hh_income, median_hh_income_quintile) %>%
  collect()

# Get application-level data
applications <- applications %>%
  right_join(applicant_list, by=c("applicant_id", "season")) %>%
  collect()

# Download processed tables for local use
applicants <- applicants %>%
  collect()

applicantprofile <- applicantprofile %>%
  collect()

activities <- activities %>%
  collect()

activities_long <- activities %>%
  # Error here is just due to extra separator for continue_college
  pivot_longer(cols=starts_with("activity_"), names_to=c("activitynum", ".value"), names_sep="_", names_prefix="activity_") %>%
  filter(!is.na(type)) %>%
  mutate(
    hours=as.numeric(hours),
    week=as.numeric(week),
    years=str_count(grade, ",")+1,
    total_hours=hours*week*years,
    applicant_id=as.numeric(applicant_id))

# Write to files
write_fst(applicants, path="data/01_applicants.fst")
write_fst(members, path="data/01_members.fst")
write_fst(applications, path="data/01_applications.fst")
write_fst(applicantprofile, path="data/01_applicantprofile.fst")
write_fst(activities_long, path="data/01_activities.fst")
write_fst(zipincome, path="data/01_zipincome.fst")

