
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "kableExtra", "fixest"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# Change table reporting decimals
options(scipen=999)

activity_level <- read_fst("data/03_activity_level_analysis.fst") %>%
  filter(international==0)

proportion_level <- read_fst("data/03_proportion_level_analysis.fst") %>%
  filter(international==0)

character_level <- read_fst("data/03_charlength_level_analysis.fst") %>%
  filter(international==0)

###

# Create a maybe overly complicated process to run regressions, store results, and process results into a format amenable to display
regression_tables <- function(df, outcomevar, caption_set, filenamesuffix, rounding=3) {
  sample_mean <- df[[outcomevar]] %>%
    mean(., na.rm=TRUE) %>%
    round(., digits=2)

  # First, run each demo separately with sparse controls
  separate_demos_race <- feols(data=df, fml=.[outcomevar] ~ 
                                  sw(black, asian, latinx, amerind, nhopi, twomoreraces) + race_miss + senior + multischool + multischool_miss, 
                                cluster=~factor(hs_ceeb)) %>%
    coeftable(keep=c("black", "asian", "latinx", "amerind", "nhopi", "twomoreraces")) %>%
    select(-id, -rhs)
    
  separate_demos_firstgen <- feols(data=df, fml=.[outcomevar] ~ 
                                     firstgen + firstgen_miss + senior + multischool + multischool_miss, 
                                   cluster=~factor(hs_ceeb)) %>%
    coeftable(keep=c("firstgen")) %>%
    as.data.frame() %>%
    mutate(coefficient="firstgen")
  
  separate_demos_feewaiver <- feols(data=df, fml=.[outcomevar] ~ 
                                      feewaiver + senior + multischool + multischool_miss, 
                                    cluster=~factor(hs_ceeb)) %>%
    coeftable(keep=c("feewaiver")) %>%
    as.data.frame() %>%
    mutate(coefficient="feewaiver")
  
  separate_demos_highestincome <- feols(data=df, fml=.[outcomevar] ~ 
                                          highest_income_quintile + highest_income_quintile_miss + senior + multischool + multischool_miss, 
                                        cluster=~factor(hs_ceeb)) %>%
    coeftable(keep=c("highest_income_quintile"), drop=c("highest_income_quintile_miss")) %>%
    as.data.frame() %>%
    mutate(coefficient="highest_income_quintile")
  
  separate_demos_private <- feols(data=df, fml=.[outcomevar] ~ 
                                          school_private + senior + multischool + multischool_miss, 
                                        cluster=~factor(hs_ceeb)) %>%
    coeftable(keep=c("school_private")) %>%
    as.data.frame() %>%
    mutate(coefficient="school_private")
    
  # Now run model 2
  separate_demos_combined <- bind_rows(separate_demos_race, separate_demos_firstgen, separate_demos_feewaiver,
                    separate_demos_highestincome, separate_demos_private) %>%
    bind_rows(data.frame(coefficient=c("Observations", "R2", "Within/Adj. R2"), Estimate=c(as.numeric(NA), as.numeric(NA), as.numeric(NA)))) %>%
    mutate(column=1)
  
  combined_demos <- feols(data=df, fml=.[outcomevar] ~ 
                            black + asian + latinx + amerind + nhopi + twomoreraces + race_miss + 
                            firstgen + firstgen_miss + 
                            feewaiver + 
                            highest_income_quintile + highest_income_quintile_miss +
                            school_private + 
                            senior + multischool + multischool_miss, 
                          cluster=~factor(hs_ceeb)) %>%
    {regression_output <<- etable(.); 
    tmp_observations <<- regression_output[regression_output[,1]=="Observations",2] %>% str_replace_all(",", "") %>% as.numeric();
    tmp_r2 <<- regression_output[regression_output[,1]=="R2",2] %>% as.numeric();
    tmp_adjr2 <<- regression_output[regression_output[,1]=="Adj. R2",2] %>% as.numeric();
    .} %>%
    coeftable(drop=c("senior", "multischool", "multischool_miss", "(Intercept)", "race_miss", "firstgen_miss", "highest_income_quintile_miss")) %>%
    as.data.frame() %>%
    mutate(coefficient=rownames(.)) %>%
    bind_rows(data.frame(coefficient=c("Observations", "R2", "Within/Adj. R2"), Estimate=c(tmp_observations, tmp_r2, tmp_adjr2))) %>%
    mutate(column=2)
  
  # Run model 3
  combined_demos_hsfe <- feols(data=df, fml=.[outcomevar] ~ 
                                 black + asian + latinx + amerind + nhopi + twomoreraces + race_miss + 
                                 firstgen + firstgen_miss + 
                                 feewaiver + 
                                 highest_income_quintile + highest_income_quintile_miss +
                                 senior + multischool + multischool_miss | factor(hs_ceeb), 
                               cluster=~factor(hs_ceeb)) %>%
    {regression_output <<- etable(.); 
    tmp_observations <<- regression_output[regression_output[,1]=="Observations",2] %>% str_replace_all(",", "") %>% as.numeric();
    tmp_r2 <<- regression_output[regression_output[,1]=="R2",2] %>% as.numeric();
    tmp_withinr2 <<- regression_output[regression_output[,1]=="Within R2",2] %>% as.numeric();
    .} %>%
    coeftable(drop=c("senior", "multischool", "multischool_miss", "(Intercept)", "race_miss", "firstgen_miss", "highest_income_quintile_miss")) %>%
    as.data.frame() %>%
    mutate(coefficient=rownames(.)) %>%
    bind_rows(data.frame(coefficient=c("Observations", "R2", "Within/Adj. R2"), Estimate=c(tmp_observations, tmp_r2, tmp_withinr2))) %>%
    mutate(column=3)
  
  # Run model 4
  combined_demos_hsfe_high <- feols(data=df %>% filter((test_pctile_bucket=="95-98" | test_pctile_bucket==">=99")), fml=.[outcomevar] ~ 
                                 black + asian + latinx + amerind + nhopi + twomoreraces + race_miss + 
                                 firstgen + firstgen_miss + 
                                 feewaiver + 
                                 highest_income_quintile + highest_income_quintile_miss +
                                 senior + multischool + multischool_miss | factor(hs_ceeb), 
                               cluster=~factor(hs_ceeb)) %>%
    {regression_output <<- etable(.); 
    tmp_observations <<- regression_output[regression_output[,1]=="Observations",2] %>% str_replace_all(",", "") %>% as.numeric();
    tmp_r2 <<- regression_output[regression_output[,1]=="R2",2] %>% as.numeric();
    tmp_withinr2 <<- regression_output[regression_output[,1]=="Within R2",2] %>% as.numeric();
    .} %>%
    coeftable(drop=c("senior", "multischool", "multischool_miss", "(Intercept)", "race_miss", "firstgen_miss", "highest_income_quintile_miss")) %>%
    as.data.frame() %>%
    mutate(coefficient=rownames(.)) %>%
    bind_rows(data.frame(coefficient=c("Observations", "R2", "Within/Adj. R2"), Estimate=c(tmp_observations, tmp_r2, tmp_withinr2))) %>%
    mutate(column=4)
  
  # Get the output together
  output_prep <- bind_rows(separate_demos_combined, combined_demos, combined_demos_hsfe, combined_demos_hsfe_high) %>%
    mutate(significance=case_when(
      `Pr(>|t|)` <= 0.001 ~ "***",
      `Pr(>|t|)` <= 0.01 ~ "**",
      `Pr(>|t|)` <= 0.05 ~ "*",
      `Pr(>|t|)` <= 0.10 ~ "+",
      TRUE ~ ""
    ),
    label=case_when(
      coefficient=="amerind" ~ "American Indian<br>or Alaska Native",
      coefficient=="asian" ~ "Asian",
      coefficient=="black" ~ "Black or African<br>American",
      coefficient=="latinx" ~ "Hispanic or<br>Latinx",
      coefficient=="nhopi" ~ "Native Hawaiian or<br>Other Pacific Islander",
      coefficient=="twomoreraces" ~ "Two or More Races",
      coefficient=="feewaiver" ~ "Fee Waiver Receipt",
      coefficient=="firstgen" ~ "First-Generation Student",
      coefficient=="highest_income_quintile" ~ "Highest Income Quintile",
      coefficient=="school_private" ~ "Private School",
      TRUE ~ coefficient
    ),
    displaytext=case_when(
      coefficient!="Observations" & coefficient!="R2" & coefficient!="Within/Adj. R2" ~ paste0(round(Estimate, rounding), " ", significance, "<br>", "(", round(`Std. Error`, rounding), ")"),
      coefficient=="Observations" ~ as.character(round(Estimate, 0)),
      TRUE ~ as.character(round(Estimate, 3))
    ),
    displaytext=case_when(
      is.na(displaytext) ~ "",
      TRUE ~ displaytext
    ))
    
  # Create an output dataframe that's more verbose
output <- output_prep %>%
  select(label, displaytext, column) %>%
  pivot_wider(id_cols=label, names_from=column, 
              names_prefix="Model ", values_from=displaytext,
              values_fill="") %>%
  rename(Variable=label) %>%
  bind_rows(data.frame(Variable=c("High School FEs", "High SAT/ACT Group"),
                       `Model 1`=c("", ""),
                       `Model 2`=c("", ""),
                       `Model 3`=c("Y", ""),
                       `Model 4`=c("Y", "Y"),
                       check.names=FALSE
  ))

# Create an actual kable table output
kable(output, row.names=F, escape=F,
      align = "l", format="html", table.attr = "style='width:20%;'",
      caption=paste0("<span style='font-size:20px; font-color:black;'>", caption_set, "</span><br><span style='font-size:16px; font-color:black;'>(Sample Mean: ", sample_mean, ")</span>")) %>%
  column_spec(1, bold = T) %>%
  kable_styling(full_width=F, bootstrap_options = c("condensed")) %>%
  row_spec(c(6, 10, 15), extra_css="border-bottom: 1px solid") %>%
  footnote(general="Each column represents a different regression specification as articulated in our methods section. 
           For models 2, 3, and 4, each column is a single regression with all indicated covariates included; for Model 
           1, each cell is a separate regression with only the indicated covariate included. For models 2, 3, and 4, 
           White applicants are the omitted reference group for each categorical value of race/ethnicity. The reference
           group for each other categorical variable are the inverse of the listed group (e.g., continuing-gen
           applicants for first-gen, public school applicants for private school, and so on). Finally, note that the
           private school coefficient cannot be estimated once high-school fixed effects are added, as private school
           status is invariant at the high school level and thus subsumed by the high-school fixed effect indicators.
           Coefficient estimates are displayed in each cell; standard errors in parentheses below each coefficient 
           estimate. Significance is indicated as follows: + for p<=0.10, * for p<=0.05, ** for p<=0.01, and 
           *** for p<=0.001") %>%
  save_kable(paste0("output/04b_regressions_", filenamesuffix, ".jpg"), zoom=3, bs_theme="default")

}

# Get a list of relevant variables
vars_list <- activity_level %>% 
  select(starts_with("count_"), starts_with("cat_1"), starts_with("cat_2"), starts_with("cat_5")) %>%
  colnames(.)

# Run at the activity level for each variable of interest
for(varname in vars_list) {
  regression_tables(df=activity_level, 
                    outcomevar=varname,
                    filenamesuffix=paste0("activity_", varname),
                    caption_set=paste0("activity_", varname))
}

# Run at the proportion level for each variable of interest
for(varname in vars_list) {
  regression_tables(df=proportion_level, 
                    outcomevar=varname,
                    filenamesuffix=paste0("proportion_", varname),
                    rounding=4,
                    caption_set=paste0("proportion_", varname))
}

# Run at the character level only for overall count
regression_tables(df=character_level, 
                  outcomevar="chars_total", 
                  filenamesuffix="character_chars_total",
                  caption_set="Average Character Length of Activity Descriptions")


