
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "readxl", "irr"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

key <- read_fst("data/05_validation_set_key.fst")

for(i in 1:5) {
  tmp5 <- read_excel(path=paste0("data/Coding/05_Excellence Set ", i, "_NW.xlsx"))
  tmp1 <- read_excel(path=paste0("data/Coding/05_Positional Leadership Set ", i, "_NW.xlsx"))
  tmp2 <- read_excel(path=paste0("data/Coding/05_Singular Leadership Set ", i, "_NW.xlsx"))
  
  if(i==1) {
    output5 <- tmp5
    output1 <- tmp1
    output2 <- tmp2
  } else {
    output5 <- bind_rows(output5, tmp5)
    output1 <- bind_rows(output1, tmp1)
    output2 <- bind_rows(output2, tmp2)
  }
}

nw_clean5 <- output5 %>%
  select(groupset, applicant_id, season, activitynum, cat_5NW=Excellence)

nw_clean1 <- output1 %>%
  select(groupset, applicant_id, season, activitynum, cat_1NW=`Positional Leadership`)

nw_clean2 <- output2 %>%
  select(groupset, applicant_id, season, activitynum, cat_2NW=`Singular Leadership`)

###

for(i in 1:5) {
  tmp5 <- read_csv(file=paste0("data/Coding/05_Excellence Set ", i, " PL.csv"))
  tmp1 <- read_csv(file=paste0("data/Coding/05_Positional Leadership Set ", i, " PL.csv"))
  tmp2 <- read_csv(file=paste0("data/Coding/05_Singular Leadership Set ", i, " PL.csv"))
  
  if(i==1) {
    output5 <- tmp5
    output1 <- tmp1
    output2 <- tmp2
  } else {
    output5 <- bind_rows(output5, tmp5)
    output1 <- bind_rows(output1, tmp1)
    output2 <- bind_rows(output2, tmp2)
  }
}

pl_clean5 <- output5 %>%
  select(groupset, applicant_id, season, activitynum, cat_5PL=Excellence)

pl_clean1 <- output1 %>%
  select(groupset, applicant_id, season, activitynum, cat_1PL=`Positional Leadership`)

pl_clean2 <- output2 %>%
  mutate(`Singular Leadership`=case_when(
    !is.na(`...7`) & is.na(`Singular Leadership`) ~ `...7`,
    TRUE ~ `Singular Leadership`
    )) %>%
  select(groupset, applicant_id, season, activitynum, cat_2PL=`Singular Leadership`)

###

for(i in 1:5) {
  tmp5 <- read_csv(file=paste0("data/Coding/05_Excellence Set ", i, "_SB.csv"))
  tmp1 <- read_csv(file=paste0("data/Coding/05_Positional Leadership Set ", i, "_SB.csv"))
  tmp2 <- read_csv(file=paste0("data/Coding/05_Singular Leadership Set ", i, "_SB.csv"))
  
  if(i==1) {
    output5 <- tmp5
    output1 <- tmp1
    output2 <- tmp2
  } else {
    output5 <- bind_rows(output5, tmp5)
    output1 <- bind_rows(output1, tmp1)
    output2 <- bind_rows(output2, tmp2)
  }
}

sb_clean5 <- output5 %>%
  mutate(Excellence=case_when(
    Excellence==90 | Excellence==9 ~ as.numeric(0),
    is.na(Excellence) & !is.na(`...7`) ~ as.numeric(`...7`),
    TRUE ~ Excellence
  )) %>%
  select(groupset, applicant_id, season, activitynum, cat_5SB=Excellence)

sb_clean1 <- output1 %>%
  mutate(`Positional Leadership`=case_when(
    `Positional Leadership`==90 | `Positional Leadership`==9 ~ as.numeric(0),
    is.na(`Positional Leadership`) & !is.na(`...7`) ~ as.numeric(`...7`),
    TRUE ~ `Positional Leadership`
  )) %>%
  select(groupset, applicant_id, season, activitynum, cat_1SB=`Positional Leadership`)

sb_clean2 <- output2 %>%
  mutate(`Singular Leadership`=case_when(
    `Singular Leadership`==90 | `Singular Leadership`==9 ~ as.numeric(0),
    is.na(`Singular Leadership`) & !is.na(`...7`) ~ as.numeric(`...7`),
    TRUE ~ `Singular Leadership`
  )) %>%
  select(groupset, applicant_id, season, activitynum, cat_2SB=`Singular Leadership`)

combined <- key %>%
  left_join(nw_clean5, by=c("groupset", "applicant_id", "season", "activitynum")) %>%
  left_join(nw_clean1, by=c("groupset", "applicant_id", "season", "activitynum")) %>%
  left_join(nw_clean2, by=c("groupset", "applicant_id", "season", "activitynum")) %>%
  left_join(pl_clean5, by=c("groupset", "applicant_id", "season", "activitynum")) %>%
  left_join(pl_clean1, by=c("groupset", "applicant_id", "season", "activitynum")) %>%
  left_join(pl_clean2, by=c("groupset", "applicant_id", "season", "activitynum")) %>%
  left_join(sb_clean5, by=c("groupset", "applicant_id", "season", "activitynum")) %>%
  left_join(sb_clean1, by=c("groupset", "applicant_id", "season", "activitynum")) %>%
  left_join(sb_clean2, by=c("groupset", "applicant_id", "season", "activitynum"))


icclist_to_df <- function(icclist) {
  output <- as.data.frame(unlist(icclist, use.names=FALSE))
  
  output <- unlist(icclist, use.names=FALSE)
  
  tmp <- data.frame(vars=names(icclist), vals=output) %>%
    pivot_wider(names_from="vars", values_from="vals")
}

ICC_check <- function(df, cattype) {

  df_without <- df %>%
    select(starts_with(cattype), -ends_with(cattype))
  
  icc_without <- df_without %>%
    icc(ratings=., model="twoway", type="agreement", unit="single")  %>%
    icclist_to_df() %>%
    mutate(icc_type="without")
  
  icc_with <- df %>%
    select(starts_with(cattype)) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single")  %>%
    icclist_to_df() %>%
    mutate(icc_type="with")
  
  # Hypothetical "worst case scenario"
  df_adversarial <- df_without
  df_adversarial$adversarial <- NA

  set.seed(1234)
  for (i in 1:nrow(df_adversarial)) {
    count_if_1 <- c(df_adversarial[i,1], df_adversarial[i,2], df_adversarial[i,3]) %>%
      .[. == 1] %>%
      length(.)
    
    if(count_if_1>=2) {
      df_adversarial$adversarial[i] <- 0
    } else {
      df_adversarial$adversarial[i] <- 1
    }
  }
  
  icc_adversarial <- df_adversarial %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="adversarial")
  
  icc_complementary <- df_adversarial %>%
    mutate(adversarial=(1-adversarial)) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="complementary")
  
  # Hypothetical uninformed guessing
  combinedvector <- c(df_without[,1], df_without[,2], df_without[,3]) %>%
    .[!is.na(.)]
  set.seed(1234)
  random <- sample(combinedvector, size=nrow(df), replace=TRUE)
  
  icc_random <- df_without %>%
    bind_cols(random) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="random")
  
  # All one answer
  icc_zeroes <- df_without %>%
    mutate(add=0) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="zeroes")
  
  icc_ones <- df_without %>%
    mutate(add=1) %>%
    icc(ratings=., model="twoway", type="agreement", unit="single") %>%
    icclist_to_df() %>%
    mutate(icc_type="ones")
  
  output <- bind_rows(icc_without, icc_with, icc_adversarial, icc_complementary, icc_random, icc_zeroes, icc_ones) %>%
    mutate(icc_type=case_when(
      icc_type=="with" ~ "With Algorithm",
      icc_type=="without" ~ "Without Algorithm",
      icc_type=="random" ~ "Random Distributional Guessing",
      icc_type=="adversarial" ~ "Adversarial Guessing",
      icc_type=="complementary" ~ "Complementary Guessing",
      icc_type=="zeroes" ~ "Always No",
      icc_type=="ones" ~ "Always Yes",
    ),
    icc_type=factor(icc_type, levels=c("Without Algorithm", "With Algorithm",
                                       "Random Distributional Guessing", 
                                       "Adversarial Guessing", "Complementary Guessing",
                                       "Always No", "Always Yes")),
    cattype=as.character(cattype))
}

###

cat_1_all <- combined %>%
  ICC_check(df=., cattype="cat_1") %>%
  mutate(subgroup="All")

cat_2_all <- combined %>%
  ICC_check(df=., cattype="cat_2") %>%
  mutate(subgroup="All")

cat_5_all <- combined %>%
  ICC_check(df=., cattype="cat_5") %>%
  mutate(subgroup="All")

group_plotter <- function(df, plottitle, filenameset) {
  topdashed <- df %>%
    filter(icc_type=="Complementary Guessing") %>%
    .$value %>%
    as.numeric()
  
  bottomdashed <- df %>%
    filter(icc_type=="Adversarial Guessing") %>%
    .$value %>%
    as.numeric()
  
  ggplot(df, aes(x=icc_type, y=as.numeric(value))) + 
    geom_pointrange(aes(ymin=as.numeric(lbound), ymax=as.numeric(ubound))) +
    geom_hline(yintercept=topdashed, linetype=2) + 
    geom_hline(yintercept=bottomdashed, linetype=2) + 
    scale_x_discrete(labels=scales::wrap_format(15)) + 
    labs(
      y="Intraclass Correlation Coefficient \n ",
      x=" \n Rater Group",
      title=plottitle
    ) + 
    theme_bw()
  
  ggsave(file=paste0("output/05b_groupplotter_", filenameset, ".png"), width=8, height=6)
}

group_plotter(df=cat_1_all, plottitle="Positional Leadership Intraclass Correlation Coefficient Analysis", filenameset="cat_1")
group_plotter(df=cat_2_all, plottitle="Singular Leadership Intraclass Correlation Coefficient Analysis", filenameset="cat_2")
group_plotter(df=cat_5_all, plottitle="Excellence and Distinction Intraclass Correlation Coefficient Analysis", filenameset="cat_5")
  
###

demographic_checker <- function(df, variable) {
  #df <- combined
  
  #variable <- female
  
  cat_1_1 <- combined %>%
    filter(.data[[variable]]==1) %>%
    ICC_check(df=., cattype="cat_1") %>%
    mutate(subgroup=variable,
           subgroup_value=1)
  
  cat_1_0 <- combined %>%
    filter(.data[[variable]]==0) %>%
    ICC_check(df=., cattype="cat_1") %>%
    mutate(subgroup=variable,
           subgroup_value=0)
  
  cat_2_1 <- combined %>%
    filter(.data[[variable]]==1) %>%
    ICC_check(df=., cattype="cat_2") %>%
    mutate(subgroup=variable,
           subgroup_value=1)
  
  cat_2_0 <- combined %>%
    filter(.data[[variable]]==0) %>%
    ICC_check(df=., cattype="cat_2") %>%
    mutate(subgroup=variable,
           subgroup_value=0)
  
  cat_5_1 <- combined %>%
    filter(.data[[variable]]==1) %>%
    ICC_check(df=., cattype="cat_5") %>%
    mutate(subgroup=variable,
           subgroup_value=1)
  
  cat_5_0 <- combined %>%
    filter(.data[[variable]]==0) %>%
    ICC_check(df=., cattype="cat_5") %>%
    mutate(subgroup=variable,
           subgroup_value=0)
  
  output <- bind_rows(cat_1_0, cat_1_1, cat_2_1, cat_2_0, cat_5_1, cat_5_0) %>%
    filter(icc_type=="With Algorithm")
}

democheck_female <- demographic_checker(combined, "female")
democheck_public <- demographic_checker(combined, "school_public")
democheck_firstgen <- demographic_checker(combined, "firstgen")
democheck_urm <- demographic_checker(combined, "urm")
democheck_feewaiver <- demographic_checker(combined, "feewaiver")

democheck_all <- bind_rows(democheck_female, democheck_public, democheck_firstgen, democheck_urm, democheck_feewaiver) %>%
  mutate(subgroup_name=case_when(
    subgroup=="female" & subgroup_value==0 ~ "Male",
    subgroup=="female" & subgroup_value==1 ~ "Female",
    subgroup=="school_public" & subgroup_value==0 ~ "Private",
    subgroup=="school_public" & subgroup_value==1 ~ "Public",
    subgroup=="firstgen" & subgroup_value==0 ~ "Continuing- gen",
    subgroup=="firstgen" & subgroup_value==1 ~ "First- gen",
    subgroup=="urm" & subgroup_value==0 ~ "Non-URM",
    subgroup=="urm" & subgroup_value==1 ~ "URM",
    subgroup=="feewaiver" & subgroup_value==0 ~ "Non- Recipient",
    subgroup=="feewaiver" & subgroup_value==1 ~ "Recipient"
  ), 
  subgroup_name=factor(subgroup_name, levels=c("Female", "Male",
                                               "Non-URM", "URM",
                                               "Continuing- gen", "First- gen",
                                               "Non- Recipient", "Recipient",
                                               "Public", "Private"
                                               )))


democheck_plotter <- function(df, plottitle, filenameset) {
  ggplot(df, aes(x=subgroup_name, y=as.numeric(value))) + 
    geom_pointrange(aes(ymin=as.numeric(lbound), ymax=as.numeric(ubound))) +
    geom_vline(xintercept=2.5) +
    geom_vline(xintercept=4.5) +
    geom_vline(xintercept=6.5) +
    geom_vline(xintercept=8.5) +
    scale_y_continuous(limits=c(0, max(as.numeric(df$ubound)))) + 
    scale_x_discrete(labels=scales::wrap_format(10)) + 
    labs(
      y="Intraclass Correlation Coefficient (With Algorithm)\n ",
      x=" \n Demographic Group",
      title=plottitle
    ) + 
    theme_bw()
  
  ggsave(file=paste0("output/05b_democheck_plotter_", filenameset, ".png"), width=8, height=6)
}

democheck_plotter(democheck_all %>% filter(cattype=="cat_1"),
                  plottitle="Demographic Differences in Intraclass Correlation: Positional Leadership",
                  filenameset="cat_1")

democheck_plotter(democheck_all %>% filter(cattype=="cat_2"),
                  plottitle="Demographic Differences in Intraclass Correlation: Singular Leadership",
                  filenameset="cat_2")

democheck_plotter(democheck_all %>% filter(cattype=="cat_5"),
                  plottitle="Demographic Differences in Intraclass Correlation: Excellence and Distinction",
                  filenameset="cat_5")

###

