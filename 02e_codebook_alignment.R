
#Clear environment
rm(list = ls())

# 1. Libraries  ----------------------
lapply(c("tidyverse", "tidylog", "fst", "readxl", "quanteda", "tidytext"), require, character.only = TRUE)

# 2. Filepathing and Setup ----------------
setwd("~/DAR_BK/22-03-27 Extracurricular Scoping BK")

# First need to load in and process each file separately
details_nw <- read_excel(path="data/Coding/details_wordlist_NWONG_updated.xlsx", sheet="Updated", guess_max=2000) %>%
  filter((`Category 1` %in% c(1, 2, 3, 4, 5))) %>%
  select(Term, `Category 1`, `Category 2`, Notes_NWD=Notes) %>%
  mutate(coder="NW") %>%
  mutate(Notes_NWD=case_when(
    Notes_NWD=="Duplicates exist" ~ as.character(NA),
    Notes_NWD=="Duplicates exist." ~ as.character(NA),
    Notes_NWD=="Duplicates Exist" ~ as.character(NA),
    Notes_NWD=="Duplicates" ~ as.character(NA),
    Notes_NWD=="Duplicate" ~ as.character(NA),
    str_detect(Notes_NWD, "Duplicates exist; ") ~ str_replace(Notes_NWD, "Duplicates exist; ", ""),
    str_detect(Notes_NWD, "Duplicate. ") ~ str_replace(Notes_NWD, "Duplicate. ", ""),
    str_detect(Notes_NWD, "Duplicates; ") ~ str_replace(Notes_NWD, "Duplicates; ", ""),
    TRUE ~ Notes_NWD
  ))

honors_nw <- read_excel(path="data/Coding/honors_wordlist_NWONG_updated.xlsx", sheet="Honors Wordlist", guess_max=3000) %>%
  filter((`Category 1` %in% c(1, 2, 3, 4, 5))) %>%
  select(Term, `Category 1`, `Category 2`, Notes_NWH=Notes) %>%
  mutate(coder="NW") %>%
  mutate(Notes_NWH=case_when(
    Notes_NWH=="Duplicates exist" ~ as.character(NA),
    str_detect(Notes_NWH, "Duplicate word; ") ~ str_replace(Notes_NWH, "Duplicate word; ", ""),
    TRUE ~ Notes_NWH
  ))

honors_jp <- read_csv(file="data/Coding/honors_wordlist_JP coding_8.11_updated.csv") %>%
  filter(!is.na(`Code 1`)) %>%
  select(Term, `Category 1`=`Code 1`, `Category 2`=`Code 2`, Notes_JP=Notes) %>%
  mutate(coder="JP")

honors_bk <- read_csv(file="data/Coding/honors_wordlist_BK.csv") %>%
  select(Term=`Word/Phrase`, `Category 1`, `Category 2`, Notes_BK=Notes) %>%
  mutate(`Category 1`=case_when(
    `Category 1`=="positional" ~ "1",
    `Category 1`=="singular" ~ "2",
    `Category 1`=="neutral" ~ "3",
    `Category 1`=="barrier" ~ "4"
  ), `Category 1`=as.numeric(`Category 1`),
  `Category 2`=case_when(
    `Category 2`=="positional" ~ "1",
    `Category 2`=="singular" ~ "2",
    `Category 2`=="neutral" ~ "3",
    `Category 2`=="barrier" ~ "4"
  ), `Category 2`=as.numeric(`Category 2`),
  coder="BK"
  )

details_sb <- read_csv(file="data/Coding/details_wordlist_SB coding_UPDATED.csv", skip=1) %>%
  filter(!is.na(Term)) %>%
  mutate(Notes_SBD=case_when(
    !is.na(Notes) & !is.na(Inclusions) & !is.na(Exclusions) ~ paste0(Notes, ", Inclusion if ", Inclusions, ", Exclusion if ", Exclusions),
    !is.na(Notes) & is.na(Inclusions) & is.na(Exclusions) ~ Notes,
    !is.na(Notes) & !is.na(Inclusions) & is.na(Exclusions) ~ paste0(Notes, ", Inclusion if ", Inclusions),
    !is.na(Notes) & is.na(Inclusions) & !is.na(Exclusions) ~ paste0(Notes, ", Exclusion if ", Exclusions),
    is.na(Notes) & !is.na(Inclusions) & !is.na(Exclusions) ~ paste0("Inclusion if ", Inclusions, ", Exclusion if ", Exclusions),
    is.na(Notes) & !is.na(Inclusions) & is.na(Exclusions) ~ paste0("Inclusion if ", Inclusions),
    is.na(Notes) & is.na(Inclusions) & !is.na(Exclusions) ~ paste0("Exclusion if ", Exclusions),
    is.na(Notes) & is.na(Inclusions) & is.na(Exclusions) ~ as.character(NA),
  ), coder="SB") %>%
  select(Term, `Category 1`=`Code Type`, `Category 2`=`Secondary Code (If needed)`, Notes_SBD, coder)
    
honors_sb <- read_csv(file="data/Coding/honors_wordlist_SB coding_UPDATED.csv", skip=1) %>%
  filter(!is.na(Term)) %>%
  mutate(Notes_SBH=case_when(
    !is.na(Notes) & !is.na(Inclusions) & !is.na(Exclusions) ~ paste0(Notes, ", Inclusion if ", Inclusions, ", Exclusion if ", Exclusions),
    !is.na(Notes) & is.na(Inclusions) & is.na(Exclusions) ~ Notes,
    !is.na(Notes) & !is.na(Inclusions) & is.na(Exclusions) ~ paste0(Notes, ", Inclusion if ", Inclusions),
    !is.na(Notes) & is.na(Inclusions) & !is.na(Exclusions) ~ paste0(Notes, ", Exclusion if ", Exclusions),
    is.na(Notes) & !is.na(Inclusions) & !is.na(Exclusions) ~ paste0("Inclusion if ", Inclusions, ", Exclusion if ", Exclusions),
    is.na(Notes) & !is.na(Inclusions) & is.na(Exclusions) ~ paste0("Inclusion if ", Inclusions),
    is.na(Notes) & is.na(Inclusions) & !is.na(Exclusions) ~ paste0("Exclusion if ", Exclusions),
    is.na(Notes) & is.na(Inclusions) & is.na(Exclusions) ~ as.character(NA),
  ), coder="SB") %>%
  select(Term, `Category 1`=`Code Type`, `Category 2`=`Secondary Code (If needed)`, Notes_SBH, coder)

# TO DO: need to load in all the separate sheets and transform properly to align across data files
davis_a <- read_excel(path="data/Coding/dictionary working draft with ucdavis.xlsx", sheet="leadership", guess_max=2000, col_names=FALSE) %>%
  mutate(Term=`...1`,
         `Category 1`=2,
         `Category 2`=as.numeric(NA),
         coder="DA",
         Notes_DA=`...5`) %>%
  select(Term, `Category 1`, `Category 2`, Notes_DA, coder)

davis_b <- read_excel(path="data/Coding/dictionary working draft with ucdavis.xlsx", sheet="excellence", guess_max=2000, col_names=FALSE) %>%
  mutate(Term=`...1`,
         `Category 1`=5,
         `Category 2`=as.numeric(NA),
         coder="DB",
         Notes_DB=case_when(
           !is.na(`...2`) ~ `...2`,
           !is.na(`...3`) ~ `...3`,
           !is.na(`...5`) ~ `...5`,
           !is.na(`...7`) ~ `...7`,
           !is.na(`...9`) ~ `...9`)) %>%
  select(Term, `Category 1`, `Category 2`, Notes_DB, coder)

davis_c <- read_excel(path="data/Coding/dictionary working draft with ucdavis.xlsx", sheet="terms from ucdavis", guess_max=2000, col_names=FALSE) %>%
  mutate(Term=`...1`,
         `Category 1`=`...6`,
         `Category 2`=as.numeric(NA),
         coder="DC",
         Notes_DC=case_when(
           !is.na(`...4`) ~ `...4`,
           !is.na(`...11`) ~ `...11`)) %>%
  select(Term, `Category 1`, `Category 2`, Notes_DC, coder)

davis_d <- read_excel(path="data/Coding/dictionary working draft with ucdavis.xlsx", sheet="others julie thought of", guess_max=2000, col_names=FALSE) %>%
  mutate(Term=`...1`,
         `Category 1`=`...4`,
         `Category 2`=as.numeric(NA),
         coder="DD",
         Notes_DD=`...6`) %>%
  select(Term, `Category 1`, `Category 2`, Notes_DD, coder)

appended <- bind_rows(davis_a, davis_b, davis_c, davis_d,
                      honors_jp,
                      honors_nw, details_nw,
                      honors_sb, details_sb,
                      honors_bk) %>%
  pivot_longer(cols=starts_with("Category "), values_to="Category", values_drop_na=TRUE) %>%
  select(-name)
  
processed <- appended %>%
  mutate(
    cat_1=case_when(
      `Category`==1 ~ 1,
      TRUE ~ 0
    ),
    cat_2=case_when(
      `Category`==2 ~ 1,
      TRUE ~ 0
    ),
    cat_3=case_when(
      `Category`==3 ~ 1,
      TRUE ~ 0
    ),
    cat_4=case_when(
      `Category`==4 ~ 1,
      TRUE ~ 0
    ),
    cat_5=case_when(
      `Category`==5 ~ 1,
      TRUE ~ 0
    )) %>%
  # First collapse within coders
  group_by(coder, Term) %>%
  mutate(across(starts_with("cat_"), ~ max(.x, na.rm=TRUE)),
         count=n()) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  select(-Category, -count, -coder) %>%
  group_by(Term) %>%
  mutate(across(starts_with("cat_"), ~ mean(.x, na.rm=TRUE)),
         count=n(),
         appearance=row_number()) %>%
  unite(col=Notes_All, sep=", ", na.rm=TRUE, c("Notes_DA", "Notes_DB", "Notes_DC", "Notes_DD", "Notes_JP",
                                               "Notes_NWH", "Notes_NWD", "Notes_SBH", "Notes_SBD", "Notes_BK")) %>%
  mutate(Notes_All=case_when(
    Notes_All=="" ~ as.character(NA),
    TRUE ~ Notes_All
  )) %>%
  pivot_wider(names_from="appearance", values_from="Notes_All") %>%
  unite(col=Notes_All, c("1", "2", "3", "4", "5", "6"), sep=", ", na.rm=TRUE) %>%
  ungroup()

write_fst(processed, "data/02e_processed.fst")
processed <- read_fst("data/02e_processed.fst")

deduplicator <- function(df, dictset, ngrams) {
  tmp <- df %>%
    mutate(doc_id=as.character(row_number()))
  
  tmp_dedup <- tmp %>%
    select(-ngrams) %>%
    corpus(docid_field="doc_id", text_field="Term", unique_docnames=TRUE) %>%
    tokens() %>%
    tokens_ngrams(n = ngrams, concatenator = " ") %>%
    tokens_lookup(dictionary = dictset) %>% 
    dfm() %>%
    convert(to="data.frame") %>%
    right_join(tmp, by="doc_id") %>%
    filter(redundant==0) %>%
    select(Term, ngrams)
  
  tmp_dedup
}

final_cleaner <- function(df) {
  lowerdf <- df %>%
    mutate(Term=str_to_lower(Term))
  
  dashes <- lowerdf %>%
    filter(str_detect(Term, "-"))
  
  dashesremove <- dashes %>%
    mutate(Term=str_replace_all(Term, "-", ""))
  
  dashesspace <- dashes %>%
    mutate(Term=str_replace_all(Term, "-", " "))
  
  joined <- lowerdf %>%
    filter(!str_detect(Term, "-")) %>%
    bind_rows(dashesremove) %>%
    bind_rows(dashesspace) %>%
    distinct() %>%
    mutate(ngrams=1+str_count(Term, " "))
  
  maxngram <- joined$ngrams %>%
    max()
  
  tmp <- joined %>%
    filter(ngrams==1)
  
  for(i in 2:maxngram){
    ngram_dict <- dictionary(
      list(
        redundant = tmp %>% .$Term
      )
    )
    
    oneless <- i-1
    
    tmp2 <- joined %>%
      filter(ngrams==i) %>%
      deduplicator(dictset=ngram_dict, ngrams=c(1:oneless))
    
    tmp <- bind_rows(tmp, tmp2)
    
  }
  
  output <- tmp %>%
    mutate(Term=str_replace_all(Term, " ", "_"))
  
  output
}

cat_2_manual <- data.frame(
  Term=c("cadet colonel", "cadet commander", "cadet sergeant major",
         "cadet chief master sergeant", "corps commander", "corp commander",
         "company commander", "group commander", "battalion commander",
         "commander",
         "chair", "chairwoman", "cocaptain", "committee chair",
         "concert master", "concertmaster",
         "conductor", "drummajor", "editor in chief", "executiveofficer",
         "key club lieutenant governor", "key club lt governor",
         "lieutenant governor key club", "lt governor key club",
         "secretarygeneral", "sole proprietor"))

cat_2_list <- processed %>%
  mutate(final_cat_2=case_when(
    Term %in% c("captains", "cofounder", "copresident", "drum major",
                "entrepreneur", "entrepeneur",
                "executive officer") ~ 1,
    Term %in% c("marshal1", "leadership", "leading",
                "business manager", "founding", "coach", 
                "chief", "commander", "chief editor") ~ 0,
    cat_2==1 & count==1 & !(Term %in% c("eboard", "senator", "senate", 
                                        "facilitator", "cabinet", "board")) ~ 1,
    cat_2==1 & count>1 ~ 1, # If two people list and both agree
    cat_2>.5 & count>2 ~ 1, # If more than two people list and more than half agree
    cat_2<cat_3 & count>2 ~ 0, # Set to zero if more people think that it should be neutral membership instead
    TRUE ~ 0
  )) %>%
  filter(final_cat_2==1) %>%
  select(Term) %>%
  bind_rows(cat_2_manual) %>%
  distinct() %>%
  final_cleaner() %>%
  distinct() %>%
  arrange(Term)

cat_2_list$Term

disqualifying_prefixes_1 <- c("my", "our", "with the", "with my", "with our", "with a", "with an",
                              "w the", "w my", "w our", "w a", "w an",
                              "for my", "for our", "for the", "for a", "for an",
                              "to the", "to our", "to my", "to a", "to an",
                              "by the", "by a", "by an",
                              "help", "help the", "helped", "helped the",
                              "assist", "assist the", "assisted", "assisted the",
                              "liaison to", "liaison for",
                              "presented to", "presented for",
                              "mentored by", "mentee of",
                              "met the", "met with",
                              "apprenticed for", "apprentice for", "apprenticed to", "apprentice to", "apprentice",
                              "work for", "work for the", "worked for", "worked for the", 
                              "work under", "work under the", "worked under", "worked under the",
                              "intern for", "intern for the", "interned for", "interned for the",
                              "intern under", "intern under the", "interned under", "interned under the",
                              "support", "support the", "supported", "supported the",
                              "shadow", "shadow the", "shadowed", "shadowed the",
                              "aspiring", "future",
                              "coordinate with", "coordinate with the", "coordinated with", "coordinated with the")
disqualifying_prefixes_2 <- c("deputy", "dep", "vice", "assistant", "asst", "associate", "assc", "junior", "jr", disqualifying_prefixes_1)

disqualifying_suffixes_1 <- c("intern", "apprentice", "mentee")
disqualifying_suffixes_2 <- c("assistant", "asst", "liaison", disqualifying_suffixes_1)

prefixes_output_2 <- data.frame(Term=as.character())
for(i in disqualifying_prefixes_2) {
  for(j in cat_2_list$Term) {
    output_tmp <- data.frame(Term=paste0(i, " ", str_replace_all(j, "_", " ")))
    prefixes_output_2 <- bind_rows(prefixes_output_2, output_tmp)
  }
}

suffixes_output_2 <- data.frame(Term=as.character())
for(i in disqualifying_suffixes_2) {
  for(j in cat_2_list$Term) {
    output_tmp1 <- data.frame(Term=paste0(str_replace_all(j, "_", " "), " ", i))
    #output_tmp2 <- data.frame(Term=paste0(str_replace_all(j, "_", " "), "s ", i))
    suffixes_output_2 <- bind_rows(suffixes_output_2, output_tmp1)
  }
}

anti_2_list <- data.frame(Term=c("first chair", "1st chair", "second chair", "2nd chair",
                                 "third chair", "3rd chair", "deputy chair", "vice chair",
                                 "department chair", "wheel chair", "social chair", "service chair",
                                 "a chair", "public relations chair", "enhancement chair",
                                 "head of the charles", "head of the fish",
                                 "squadron commander", "squad commander", "team commander", "flight commander",
                                 "mission support commander", "platoon commander",
                                 "head of the hooch", "head of the schuylkill",
                                 "head of the ohio", "head of the river",
                                 "entrepreneur academy", "entrepreneur club", "entrepreneur congress", "entrepreneur program",
                                 "entrepeneur academy", "entrepeneur club", "entrepeneur congress", "entrepeneur program",
                                 "captains award", "captains council", "captain award", "captain council")) %>%
  bind_rows(prefixes_output_2, suffixes_output_2) %>%
  final_cleaner() %>%
  distinct() %>%
  arrange(Term)

cat_1_manual <- data.frame(Term=c("advisor", "businessmanager", "vicepresident", "copresident", "concert master", 
                                  "concertmaster", "quartermaster", "facilitator",
                                  "broad caster", "clubadvisor", "eaglescout", "film maker",
                                  "goldaward", "lifescout", "peertrainer", "qtrmaster", "student govt",
                                  "cfo", "coo", "leadership team", "leadership council"))

cat_1_list <- processed %>%
  full_join(cat_2_list %>% 
              mutate(final_cat_2=1) %>%
              mutate(Term=str_replace_all(Term, "_", " "))
            , by="Term") %>%
  mutate(final_cat_1=case_when(
    final_cat_2==1 ~ 1,
    Term %in% c("leadership team", "leadership council", "board", "cabinet", "committee", "eboard", "executive", "student government", 
                "student council", "elected",
                "club officer", "club secretary", "marshal", "leadership council", "senator",
                "events committee", "event planner", "event organizer", "event manager",
                "event leader", "educator", "officer", "marshal1", "senate", "1st chair", "first chair",
                "broadcaster", "announcer", "Lead in play", "mediator", "business manager") ~ 1,
    Term %in% c("eagle", "leadership camp", "leadership academy", 
                "caregiver", "caretaker", "camp counselor", "camp coach",
                "premier", "provider", "varsity", "management", "managing",
                "champion", "contractor", "lifeguard", "supervised",
                "mayor", "governor", "interperter", "teachers aide", "teachers assistant", "affiliate") ~ 0,
    cat_1>0 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(final_cat_1==1) %>%
  select(Term) %>%
  bind_rows(cat_1_manual) %>%
  distinct() %>%
  final_cleaner() %>%
  distinct() %>%
  arrange(Term)

cat_1_closecalls <- processed %>%
  filter(cat_1>0) %>%
  select(Term) %>%
  anti_join(cat_1_list, by="Term") %>%
  distinct() %>%
  arrange(Term)

cat_1_list %>% anti_join(cat_2_list) %>% .$Term
cat_1_list %>% .$Term
cat_1_closecalls$Term

prefixes_output_1 <- data.frame(Term=as.character())
for(i in disqualifying_prefixes_1) {
  for(j in cat_1_list$Term) {
    output_tmp <- data.frame(Term=paste0(i, " ", str_replace_all(j, "_", " ")))
    prefixes_output_1 <- bind_rows(prefixes_output_1, output_tmp)
  }
}

suffixes_output_1 <- data.frame(Term=as.character())
for(i in disqualifying_suffixes_1) {
  for(j in cat_1_list$Term) {
    output_tmp1 <- data.frame(Term=paste0(str_replace_all(j, "_", " "), " ", i))
    #output_tmp2 <- data.frame(Term=paste0(str_replace_all(j, "_", " "), "s ", i))
    suffixes_output_1 <- bind_rows(suffixes_output_1, output_tmp1)
  }
}

anti_1_list <- data.frame(Term=c("second chair", "2nd chair",
                                 "third chair", "3rd chair", "peer coach", 
                                 "head of the charles", "head of the fish",
                                 "head of the hooch", "head of the schuylkill",
                                 "head of the ohio", "head of the river",
                                 "board game", "bulletin board", "skate board", "main board", "circuit board", "college board",
                                 "game cabinet", "supply cabinet",
                                 "editorial shoots", "editorial meetings", "editorial assistance", "editorial letters",
                                 "future educator", 
                                 "host exchange",
                                 "executive decisions", "executive decisionmaking", "executive decision",
                                 "police officer", "teacher appreciation"
                                 )) %>%
  bind_rows(anti_2_list %>%
              select(Term) %>%
              mutate(Term=str_replace_all(Term, "_", " "))) %>%
  bind_rows(prefixes_output_1, suffixes_output_1) %>%
  # Results in duplicates but should be fine because final_cleaner takes care of dupes
  final_cleaner() %>%
  distinct() %>%
  arrange(Term)

cat_5_manual <- data.frame(Term=c("mvp", "most valuable player", "allstar", "all star", "all-star", "lifescout", "eaglescout",
                                  "all-american", "awarded", "life scout", "statelevel", "second place", "1st place", "first place",
                                  "first chair", "1st chair", "medalist", "medal", "black belt", 
                                  "2nd place", "3rd place", "third place", "finalist", "semi-finalist",
                                  "co captain", "co president", "mostvaluableplayer",
                                  "semi finalist", "talentsearch", "captian", "olympic development", "olympics development",
                                  "junior olympic", "junior olympics",
                                  "isef", "author", "coauthor", "co author", "co captain",
                                  "boys state", "boys nation", "girls state", "girls nation",
                                  "governors school", "merit scholar", "scholar athlete", "upward bound scholar",
                                  "tedx speaker", "tedx talk", "delivered tedx", "inaugural tedx",
                                  "award winning"))

cat_5_list <- processed %>%
  mutate(final_cat_5=case_when(
    Term %in% c("intellectual", "Olympic development", "chair", "producer", 
                "eagle", "olympic", "olympics", "first", "1st", "editor", "director",
                "chem olympiad", "co founder", "cofounder", "co-founder", "cochair", "co chair", "co-chair",
                "competitor", "scholar", "tedx", "winning") ~ 0,
    cat_5>0 ~ 1,
    TRUE ~ 0
  )) %>%
  filter(final_cat_5==1) %>%
  select(Term) %>%
  bind_rows(cat_5_manual) %>%
  distinct() %>%
  final_cleaner() %>%
  distinct() %>%
  arrange(Term)

cat_5_closecalls <- processed %>%
  filter(cat_5>0) %>%
  select(Term) %>%
  anti_join(cat_5_list, by="Term") %>%
  distinct() %>%
  arrange(Term)

cat_5_list$Term
cat_5_closecalls$Term

cat_5_positions <- cat_5_list %>%
  filter(Term %in% c("author", "captain", "captian", "coauthor", "cocaptain", "copresident", 
                     "editor in chief", "editorinchief", "first chair", "lead", "life scout", 
                     "lifescout", "president", "soloist", "tedx speaker", "winner"))

prefixes_output_5 <- data.frame(Term=as.character())
for(i in disqualifying_prefixes_2) {
  for(j in cat_5_positions$Term) {
    output_tmp <- data.frame(Term=paste0(i, " ", str_replace_all(j, "_", " ")))
    prefixes_output_5 <- bind_rows(prefixes_output_5, output_tmp)
  }
}

suffixes_output_5 <- data.frame(Term=as.character())
for(i in disqualifying_suffixes_2) {
  for(j in cat_5_positions$Term) {
    output_tmp1 <- data.frame(Term=paste0(str_replace_all(j, "_", " "), " ", i))
    #output_tmp2 <- data.frame(Term=paste0(str_replace_all(j, "_", " "), "s ", i))
    suffixes_output_5 <- bind_rows(suffixes_output_5, output_tmp1)
  }
}

anti_5_list <- data.frame(Term=c("junior olympic volunteer", "junior olympics volunteer", "junior varsity",
                                 "all american steakhouse", "all american steak house", "all county festival",
                                 "all star cheer", "allstar cheer", "allstate insurance", "all state insurance",
                                 "award show", "food champion", "champs sports", "shadowed at intel", "shadowed intel",
                                 "non competitive", "not competitive", "competitive marketplace", "competitive market place",
                                 "toward eagle scout", "becoming an eagle scout", "elite learning group",
                                 "boys nation finalist", "girls nation finalist", "boys nation nomination", "girls nation nomination",
                                 "nominated for boys nation", "nominated for girls nation", "adobe premier", "premier software",
                                 "wrote premier", "premier service", "service premier", "prize ceremony", "prize desk", "gave prize",
                                 "award committee", "focused on scholarship", "scholarship organization", "scholarship organizer",
                                 "solo expedition", "solo expeditionist", "solo expeditions", "starter culture", "starter cultures",
                                 "on top of", "top colleges"
                                 )) %>%
  bind_rows(prefixes_output_5, suffixes_output_5) %>%
  final_cleaner() %>%
  distinct() %>%
  arrange(Term)

# See in use list for later
noteschecker <- processed %>%
  select(Term, Notes_All) %>%
  mutate(Term=str_to_lower(Term)) %>%
  full_join(cat_1_list %>% select(Term) %>% mutate(cat_1=1)) %>%
  full_join(cat_2_list %>% select(Term) %>% mutate(cat_2=1)) %>%
  full_join(cat_5_list %>% select(Term) %>% mutate(cat_5=1))

siu_manual <- data.frame(Term=c("llc", "head", "sole proprietor", "olympic", "olympics", "chair", "conductor", "governor", "head", "premier", "provider", "affiliate", "teacher", "interpreter", "interperter")) %>%
  mutate(ngrams=1+str_count(Term, " "))

siu_list <- noteschecker %>%
  filter(str_detect(str_to_lower(Notes_All), "useage") | str_detect(str_to_lower(Notes_All), "usage")) %>%
  mutate(ngrams=1+str_count(Term, " ")) %>%
  bind_rows(siu_manual) %>%
  distinct() %>%
  final_cleaner() %>%
  distinct() %>%
  select(Term) %>%
  arrange(Term)

write_csv(cat_1_list, "data/02e_cat_1_list.csv")
write_csv(cat_2_list, "data/02e_cat_2_list.csv")
write_csv(cat_5_list, "data/02e_cat_5_list.csv")
write_csv(anti_1_list, "data/02e_anti_1_list.csv")
write_csv(anti_2_list, "data/02e_anti_2_list.csv")
write_csv(anti_5_list, "data/02e_anti_5_list.csv")
write_csv(siu_list, "data/02e_siu_list.csv")

# Make a nice list of words in each group and their relevant exceptions
cat_2_list <- read_csv("data/02e_cat_2_list.csv")
anti_2_list <- read_csv("data/02e_anti_2_list.csv")
cat_5_list <- read_csv("data/02e_cat_5_list.csv")
anti_5_list <- read_csv("data/02e_anti_5_list.csv")

nice_list_maker <- function(wordlist, antilist) {
  
  wordlist <- wordlist %>%
    mutate(exceptions="")
  
  for (i in 1:nrow(wordlist)) {
    relevant_term <- wordlist$Term[i]
    
    exceptions <- antilist %>%
      mutate(includes = case_when(
        str_detect(Term, pattern=paste0("_", relevant_term)) ~ 1,
        str_detect(Term, pattern=paste0(relevant_term, "_")) ~ 1,
        TRUE ~ 0
      )) %>%
      filter(includes==1) 
    
    if(nrow(exceptions)>5) {
      exceptions <- exceptions %>%
        slice_sample(n=5)
    }
    
    
    wordlist$exceptions[i] <- paste0(exceptions$Term, collapse=", ")
    
  }
  
  wordlist %>%
    select(Phrase=Term, `Exceptions Examples`=exceptions)
  
}

nice_2 <- nice_list_maker(cat_2_list, anti_2_list)
write_csv(nice_2, file="output/02e_nice_2_list.csv")

nice_1 <- nice_list_maker(cat_1_list, anti_1_list)
write_csv(nice_1, file="output/02e_nice_1_list.csv")

nice_5 <- nice_list_maker(cat_5_list, anti_5_list)
write_csv(nice_5, file="output/02e_nice_5_list.csv")
