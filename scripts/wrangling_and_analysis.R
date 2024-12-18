###################################################################################################
# Load necessary libraries
###################################################################################################
library(data.table)
library(nnet)
library(readxl)
library(tidyverse)

###################################################################################################
# Initialize environment
###################################################################################################

# Clears your current environment!
rm(list = ls())
setwd("/Users/jamesguevara/sebatlab Dropbox/James Guevara/smokingExposureAnalysis")

###################################################################################################
# Load ID files
###################################################################################################
# REACH
#REACH_MASTER_09.13.18.xlsx_master_table.tsv = fread("resources/REACH_MASTER_09.13.18.xlsx - master_table.tsv") %>%
#  select(Sample_ID, Family_ID, Rel_Proband) %>%
#  filter(Rel_Proband == "Mom" | Rel_Proband == "Dad")
REACH_MASTER_09.13.18.xlsx_master_table.tsv = fread("resources/REACH_MASTER_09.13.18.xlsx - master_table.tsv")
REACH_MASTER_09.13.18.xlsx_master_table.tsv_fathers = REACH_MASTER_09.13.18.xlsx_master_table.tsv %>%
  filter(Rel_Proband == "Dad") %>%
  select(Family_ID, Sample_ID) %>%
  rename("Father_ID" = "Sample_ID")
REACH_MASTER_09.13.18.xlsx_master_table.tsv_mothers = REACH_MASTER_09.13.18.xlsx_master_table.tsv %>%
  filter(Rel_Proband == "Mom") %>%
  select(Family_ID, Sample_ID) %>%
  rename("Mother_ID" = "Sample_ID")
REACH_PARENTS = full_join(REACH_MASTER_09.13.18.xlsx_master_table.tsv_fathers, REACH_MASTER_09.13.18.xlsx_master_table.tsv_mothers, by = "Family_ID") %>%
  mutate(Cohort = "REACH")

# SPARK
individuals_registration_2022_12_12.csv = fread("resources/SPARK_collection_v9_2022-12-12/individuals_registration_2022-12-12.csv") %>%
  select(subject_sp_id, family_sf_id, biomother_sp_id, biofather_sp_id)
individuals_registration_2022_12_12.csv_fathers = individuals_registration_2022_12_12.csv %>%
  select(family_sf_id, biofather_sp_id) %>%
  filter(biofather_sp_id != "") %>%
  distinct()
individuals_registration_2022_12_12.csv_mothers = individuals_registration_2022_12_12.csv %>%
  select(family_sf_id, biomother_sp_id) %>%
  filter(biomother_sp_id != "") %>%
  distinct()
SPARK_PARENTS = full_join(individuals_registration_2022_12_12.csv_fathers, individuals_registration_2022_12_12.csv_mothers, by = "family_sf_id") %>%
  rename("Family_ID" = "family_sf_id", "Father_ID" = "biofather_sp_id", "Mother_ID" = "biomother_sp_id") %>%
  mutate(Cohort = "SPARK")
### Warning message:
###   In full_join(individuals_registration_2022_12_12.csv_fathers, individuals_registration_2022_12_12.csv_mothers,  :
###                  Detected an unexpected many-to-many relationship between `x` and `y`.
###                ℹ Row 497 of `x` matches multiple rows in `y`.
###                ℹ Row 598 of `y` matches multiple rows in `x`.
###                ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.
# Specifically:
# SF0004088 appears twice in individuals_registration_2022_12_12.csv_mothers
# SF0002632 appears twice in individuals_registration_2022_12_12.csv_fathers

# SSC
nygc_sfari_id_map.csv = fread("resources/nygc_sfari_id_map.csv") # SSC mapping
nygc_sfari_id_map.csv_fathers = nygc_sfari_id_map.csv %>%
  filter(grepl("\\.fa$", `SFARI ID`)) %>%
  mutate(Family_ID = sub("\\..*", '', `SFARI ID`)) %>%
  select(Family_ID, `Repository Id`) %>%
  rename("Father_ID" = "Repository Id")
nygc_sfari_id_map.csv_mothers = nygc_sfari_id_map.csv %>%
  filter(grepl("\\.mo$", `SFARI ID`)) %>%
  mutate(Family_ID = sub("\\..*", '', `SFARI ID`)) %>%
  select(Family_ID, `Repository Id`) %>%
  rename("Mother_ID" = "Repository Id")
SSC_PARENTS = full_join(nygc_sfari_id_map.csv_fathers, nygc_sfari_id_map.csv_mothers, by = "Family_ID") %>%
  mutate(Cohort = "SSC")

# *** Output *** #
PARENTAL_IDS = bind_rows(REACH_PARENTS, SPARK_PARENTS, SSC_PARENTS)

###################################################################################################
# Preprocess surveys
###################################################################################################

# REACH
PregnancyExposureSurvey_FINAL_06.11.18.xlsx_REACH_cohort = read_excel("exposureData/REACH/Environmental Exposures/1. ORIGINAL - June 11, 2018/PregnancyExposureSurvey_FINAL_06.11.18.xlsx", sheet = "REACH cohort", skip = 3) %>%
  select(1, 3, "Mom smoking PRIOR preg of Child #1", "Dad smoking PRIOR preg of Child #1") %>%
  rename("Family_ID" = "External Data Reference (FID)", "relationship_proband" = "Relationship to REACH child/ren", "mom_smoking_prior" = "Mom smoking PRIOR preg of Child #1", "dad_smoking_prior" = "Dad smoking PRIOR preg of Child #1")
PregnancyExposureSurvey_FINAL_06.11.18.xlsx_Barcelona_cohort = read_excel("exposureData/REACH/Environmental Exposures/1. ORIGINAL - June 11, 2018/PregnancyExposureSurvey_FINAL_06.11.18.xlsx", sheet = "Barcelona cohort", skip = 3) %>%
  select(1, 3, "Mom smoking PRIOR preg of Child #1", "Dad smoking PRIOR preg of Child #1") %>%
  rename("Family_ID" = "External Data Reference (FID)", "relationship_proband" = "Relationship to REACH child/ren", "mom_smoking_prior" = "Mom smoking PRIOR preg of Child #1", "dad_smoking_prior" = "Dad smoking PRIOR preg of Child #1")
PregnancyExposureSurvey = bind_rows(PregnancyExposureSurvey_FINAL_06.11.18.xlsx_REACH_cohort,
                                    PregnancyExposureSurvey_FINAL_06.11.18.xlsx_Barcelona_cohort) %>%
  left_join(REACH_PARENTS, by = "Family_ID") %>%
  select(Family_ID, Father_ID, dad_smoking_prior, Mother_ID, mom_smoking_prior)

Envir_Exp_FINAL_03.27.19_May = read_excel("exposureData/REACH/Environmental Exposures/2. FOLLOW-UP - March 27, 2019/Envir Exp  - FINAL 03.27.19_May 3, 2019_11.36_SORTED_numerical_SHORT.xls", sheet = "Envir Exp  - FINAL 03.27.19_May", skip = 2) %>%
  select(2, contains("Q1.2"), contains("Q1.4"), contains("Q4.2"), contains("Q4.4"), contains("Q1.10"), contains("Q4.10"))
colnames(Envir_Exp_FINAL_03.27.19_May) = c("Family_ID", "past_smoking_mother", "currently_smoking_mother", "past_smoking_father", "currently_smoking_father",
                                           "cig_freq_age_1_mother", "cig_freq_age_2_mother", "cig_freq_age_3_mother", "cig_freq_age_4_mother", "cig_freq_age_5_mother", "cig_freq_age_6_mother", "cig_freq_age_7_mother", "cig_freq_age_8_mother", "cig_freq_age_9_mother",
                                           "cig_freq_age_1_father", "cig_freq_age_2_father", "cig_freq_age_3_father", "cig_freq_age_4_father", "cig_freq_age_5_father", "cig_freq_age_6_father", "cig_freq_age_7_father", "cig_freq_age_8_father", "cig_freq_age_9_father"
                                           )
Envir_Exp_SIMPLIFIED_REACH_04.22.19_May = read_excel("exposureData/REACH/Environmental Exposures/3. SIMPLIFED - April 22, 2019/Envir Exp SIMPLIFIED - REACH 04.22.19_May 3, 2019_11.17_SORTED_numerical.xls", sheet = "Envir Exp SIMPLIFIED - REACH 04", skip = 1) %>%
  select(2, contains("Q1.2"), contains("Q1.4"), contains("Q4.2"), contains("Q4.4"), contains("Q1.10"), contains("Q4.10"))
colnames(Envir_Exp_SIMPLIFIED_REACH_04.22.19_May) = c("Family_ID", "past_smoking_mother", "currently_smoking_mother", "past_smoking_father", "currently_smoking_father",
                                           "cig_freq_age_1_mother", "cig_freq_age_2_mother", "cig_freq_age_3_mother", "cig_freq_age_4_mother", "cig_freq_age_5_mother", "cig_freq_age_6_mother", "cig_freq_age_7_mother", "cig_freq_age_8_mother", "cig_freq_age_9_mother",
                                           "cig_freq_age_1_father", "cig_freq_age_2_father", "cig_freq_age_3_father", "cig_freq_age_4_father", "cig_freq_age_5_father", "cig_freq_age_6_father", "cig_freq_age_7_father", "cig_freq_age_8_father", "cig_freq_age_9_father"
                                           )
REACH_SURVEY = bind_rows(Envir_Exp_FINAL_03.27.19_May, Envir_Exp_SIMPLIFIED_REACH_04.22.19_May) %>%
  left_join(REACH_PARENTS, by = "Family_ID") %>%
  mutate(Survey_ID = "REACH_Envir_Exp") %>%
  rename("past_smoke_cig_father" = "past_smoking_father",
         "currently_smoke_cig_father" = "currently_smoking_father") %>%
  rename("past_smoke_cig_mother" = "past_smoking_mother",
         "currently_smoke_cig_mother" = "currently_smoking_mother") %>%
  mutate(past_smoke_cig_father = if_else(past_smoke_cig_father == 1, 0, 1)) %>%
  mutate(currently_smoke_cig_father = if_else(currently_smoke_cig_father == 1, 0, 1)) %>%
  mutate(past_smoke_cig_mother = if_else(past_smoke_cig_mother == 1, 0, 1)) %>%
  mutate(currently_smoke_cig_mother = if_else(currently_smoke_cig_mother == 1, 0, 1))
  
# SPARK
# Survey 1
RM0053Sebat_P1_ParentSurvey.csv = fread("exposureData/SPARK/RM0053Sebat/RM0053Sebat_P1/RM0053Sebat_P1_ParentSurvey.csv") %>%
  select(SFID, Authorizer_SPID, authorizer_sex, relationship_proband, past_smoke_cig, currently_smoke_cig, contains("cig_freq_age"))
RM0053Sebat_P2_ParentSurvey.csv = fread("exposureData/SPARK/RM0053Sebat/RM0053Sebat_P2/RM0053Sebat_P2_ParentSurvey.csv") %>%
  select(SFID, Expected_SPID_P2, Expected_SPID_P2_sex, relationship_proband, past_smoke_cig, currently_smoke_cig, contains("cig_freq_age"))
RM0053Sebat_ParentSurvey.csv = full_join(RM0053Sebat_P1_ParentSurvey.csv, RM0053Sebat_P2_ParentSurvey.csv, by = "SFID") %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., "-666"))) %>%
  mutate(across(everything(), ~ na_if(., "-999")))
RM0053Sebat_ParentSurvey.csv_female = RM0053Sebat_ParentSurvey.csv %>%
  filter(authorizer_sex == "female") %>%
  rename_with(~ str_replace(., "\\.x$", "_mother"), ends_with(".x")) %>%
  rename_with(~ str_replace(., "\\.y$", "_father"), ends_with(".y")) %>%
  rename("Mother_ID" = "Authorizer_SPID", "Father_ID" = "Expected_SPID_P2") %>%
  select(SFID, Mother_ID, contains("mother"), Father_ID, contains("father"))
RM0053Sebat_ParentSurvey.csv_male = RM0053Sebat_ParentSurvey.csv %>%
  filter(authorizer_sex == "male") %>%
  rename_with(~ str_replace(., "\\.x$", "_father"), ends_with(".x")) %>%
  rename_with(~ str_replace(., "\\.y$", "_mother"), ends_with(".y")) %>%
  rename("Father_ID" = "Authorizer_SPID", "Mother_ID" = "Expected_SPID_P2") %>%
  select(SFID, Mother_ID, contains("mother"), Father_ID, contains("father"))
SPARK_RESEARCH_MATCH_SURVEY_01 = bind_rows(RM0053Sebat_ParentSurvey.csv_female, RM0053Sebat_ParentSurvey.csv_male) %>%
  rename("Family_ID" = "SFID") %>%
  left_join(SPARK_PARENTS, by = "Family_ID") %>%
  mutate("Mother_ID" = coalesce(Mother_ID.y, Mother_ID.x)) %>%
  mutate("Father_ID" = coalesce(Father_ID.y, Father_ID.x)) %>%
  mutate(Survey_ID = "RM0053Sebat") %>%
  select(Cohort, Survey_ID, Family_ID, Father_ID, Mother_ID, contains("_father"), contains("_mother"), -relationship_proband_mother, -relationship_proband_father)

# Survey 2
RM0053Sebat_2021_P1_Parent_Survey.xlsx = read_excel("exposureData/SPARK/SPARK 07.2021/2_files_from_RM0053Sebat_2021_dataset/RM0053Sebat_2021_P1/RM0053Sebat_2021_P1_Parent Survey.xlsx", sheet = 1) %>%
  select(SFID, Authorizer_SPID, authorizer_sex, relationship_proband, past_smoke_cig, currently_smoke_cig, contains("cig_freq_age"))
RM0053Sebat_2021_P2_Parent_Survey.xlsx = read_excel("exposureData/SPARK/SPARK 07.2021/2_files_from_RM0053Sebat_2021_dataset/RM0053Sebat_2021_P2/RM0053Sebat_2021_P2_Parent Survey P2.xlsx", sheet = 1) %>%
  select(SFID, Authorizer_SPID, authorizer_sex, relationship_proband, past_smoke_cig, currently_smoke_cig, contains("cig_freq_age"))
RM0053Sebat_2021_Parent_Survey.xlsx = full_join(RM0053Sebat_2021_P1_Parent_Survey.xlsx, RM0053Sebat_2021_P2_Parent_Survey.xlsx, by = "SFID") %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., "-666"))) %>%
  mutate(across(everything(), ~ na_if(., "-999"))) %>%
  mutate(across(everything(), ~ na_if(., "NULL")))
RM0053Sebat_2021_Parent_Survey.xlsx_female = RM0053Sebat_2021_Parent_Survey.xlsx %>%
  filter(authorizer_sex.x == "female") %>%
  rename_with(~ str_replace(., "\\.x$", "_mother"), ends_with(".x")) %>%
  rename_with(~ str_replace(., "\\.y$", "_father"), ends_with(".y")) %>%
  rename("Mother_ID" = "Authorizer_SPID_mother", "Father_ID" = "Authorizer_SPID_father")
RM0053Sebat_2021_Parent_Survey.xlsx_male = RM0053Sebat_2021_Parent_Survey.xlsx %>%
  filter(authorizer_sex.x == "male") %>%
  rename_with(~ str_replace(., "\\.x$", "_father"), ends_with(".x")) %>%
  rename_with(~ str_replace(., "\\.y$", "_mother"), ends_with(".y")) %>%
  rename("Mother_ID" = "Authorizer_SPID_mother", "Father_ID" = "Authorizer_SPID_father")
SPARK_RESEARCH_MATCH_SURVEY_02 = bind_rows(RM0053Sebat_2021_Parent_Survey.xlsx_female, RM0053Sebat_2021_Parent_Survey.xlsx_male) %>%
  rename("Family_ID" = "SFID") %>%
  left_join(SPARK_PARENTS, by = "Family_ID") %>%
  mutate("Mother_ID" = coalesce(Mother_ID.y, Mother_ID.x)) %>%
  mutate("Father_ID" = coalesce(Father_ID.y, Father_ID.x)) %>%
  mutate(Survey_ID = "RM0053Sebat_2021") %>%
  select(Cohort, Survey_ID, Family_ID, Father_ID, Mother_ID, contains("_father"), contains("_mother"), -relationship_proband_mother, -relationship_proband_father, -authorizer_sex_mother, -authorizer_sex_father)

# SSC
ssc_father_substance_abuse_history.csv = fread("exposureData/SSC/Phenotype + Ages/ssc_father_substance_abuse_history.csv") %>%
  select(individual, tobacco_currently, tobacco_past) %>%
  mutate(Family_ID = str_extract(individual, "^[^\\.]+"))
ssc_mother_substance_abuse_history.csv = fread("exposureData/SSC/Phenotype + Ages/ssc_mother_substance_abuse_history.csv") %>%
  select(individual, tobacco_currently, tobacco_past) %>%
  mutate(Family_ID = str_extract(individual, "^[^\\.]+"))
SSC_SUBSTANCE_ABUSE_HISTORY_SURVEY = full_join(ssc_father_substance_abuse_history.csv, ssc_mother_substance_abuse_history.csv, by = "Family_ID", suffix = c("_father", "_mother")) %>%
  select(Family_ID, contains("father"), contains("mother")) %>% select(-contains("individual")) %>%
  mutate(across(everything(), as.character)) %>%
  mutate_all(~ na_if(., "")) %>%
  left_join(SSC_PARENTS, by = "Family_ID") %>%
  mutate(Survey_ID = "SSC_substance_abuse_history") %>%
  select(Cohort, Survey_ID, Family_ID, Father_ID, Mother_ID, tobacco_past_father, tobacco_currently_father, tobacco_past_mother, tobacco_currently_mother) %>%
  mutate(across(c(tobacco_past_father, tobacco_currently_father, tobacco_past_mother, tobacco_currently_mother), ~ ifelse(. == "yes", 1, 0))) %>%
  rename("past_smoke_cig_father_old_survey" = "tobacco_past_father", "currently_smoke_cig_father_old_survey" = "tobacco_currently_father") %>%
  rename("past_smoke_cig_mother_old_survey" = "tobacco_past_mother", "currently_smoke_cig_mother_old_survey" = "tobacco_currently_mother") %>%
  rename("old_survey" = "Survey_ID")

# SSC0004SebatP1_ParentSurvey.csv = fread("exposureData/SSC/SSC0004Sebat/SSC0004Sebat_P1/SSC0004SebatP1_ParentSurvey.csv") %>%
#   select(FamilyID, SFARI_ID_Authorizer, Gender_Authorizer, past_smoke_cig, currently_smoke_cig)
# SSC0004SebatP2_ParentSurvey.csv = fread("exposureData/SSC/SSC0004Sebat/SSC0004Sebat_P2/SSC0004SebatP2_ParentSurvey.csv") %>%
#   select(FamilyID, SFARI_ID_Authorizer, Gender_Authorizer, past_smoke_cig, currently_smoke_cig)
SSC0004Sebat_04.03.20_P1_ParentSurvey.csv = fread("exposureData/SSC/SSC0004Sebat_04.03.20/SSC0004Sebat_P1/SSC0004SebatP1_ParentSurvey.csv") %>%
  select(FamilyID, SFARI_ID_Authorizer, Gender_Authorizer, past_smoke_cig, currently_smoke_cig, contains("cig_freq_age"))
SSC0004Sebat_04.03.20_P2_ParentSurvey.csv = fread("exposureData/SSC/SSC0004Sebat_04.03.20/SSC0004Sebat_P2/SSC0004SebatP2_ParentSurvey.csv") %>%
  select(FamilyID, SFARI_ID_Authorizer, Gender_Authorizer, past_smoke_cig, currently_smoke_cig, contains("cig_freq_age"))
SSC_RESEARCH_MATCH_SURVEY = full_join(SSC0004Sebat_04.03.20_P1_ParentSurvey.csv, SSC0004Sebat_04.03.20_P2_ParentSurvey.csv, by = "FamilyID") %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., "-666"))) %>%
  mutate(across(everything(), ~ na_if(., "-999"))) %>%
  mutate(across(everything(), ~ na_if(., "NULL"))) %>%
  rename_with(~ str_replace(., "\\.x$", "_mother"), ends_with(".x")) %>%
  rename_with(~ str_replace(., "\\.y$", "_father"), ends_with(".y")) %>%
  rename("Family_ID" = "FamilyID") %>%
  left_join(SSC_PARENTS, by = "Family_ID") %>%
  mutate(Survey_ID = "SSC0004Sebat_04.03.20") %>%
  mutate(in_ssc_research_match_survey = 1) %>%
  select(Cohort, Survey_ID, Family_ID, Father_ID, Mother_ID, contains("_father"), contains("_mother"), -SFARI_ID_Authorizer_father, -Gender_Authorizer_father, -SFARI_ID_Authorizer_mother, -Gender_Authorizer_mother, in_ssc_research_match_survey)
  
SSC_COMBINED_SURVEY = full_join(SSC_SUBSTANCE_ABUSE_HISTORY_SURVEY, SSC_RESEARCH_MATCH_SURVEY, by = c("Family_ID", "Father_ID", "Mother_ID")) %>%
  mutate(Cohort = "SSC") %>%
  select(Cohort, Survey_ID, Family_ID, Father_ID, Mother_ID, 
         past_smoke_cig_father, currently_smoke_cig_father,
         past_smoke_cig_mother, currently_smoke_cig_mother,
         "cig_freq_age_1_father", "cig_freq_age_2_father", "cig_freq_age_3_father", "cig_freq_age_4_father", "cig_freq_age_5_father", "cig_freq_age_6_father", "cig_freq_age_7_father", "cig_freq_age_8_father", "cig_freq_age_9_father",
         "cig_freq_age_1_mother", "cig_freq_age_2_mother", "cig_freq_age_3_mother", "cig_freq_age_4_mother", "cig_freq_age_5_mother", "cig_freq_age_6_mother", "cig_freq_age_7_mother", "cig_freq_age_8_mother", "cig_freq_age_9_mother",
         old_survey, past_smoke_cig_father_old_survey, currently_smoke_cig_father_old_survey, past_smoke_cig_mother_old_survey, currently_smoke_cig_mother_old_survey, in_ssc_research_match_survey) %>%
  mutate(Survey_ID = coalesce(Survey_ID, old_survey)) %>%
  mutate(in_ssc_substance_abuse_survey = ifelse(is.na(old_survey), 0, 1)) %>%
  mutate(in_ssc_research_match_survey = ifelse(is.na(in_ssc_research_match_survey), 0, 1)) %>%
  rename("past_smoke_cig_father_substance_abuse_survey" = "past_smoke_cig_father_old_survey",
         "currently_smoke_cig_father_substance_abuse_survey" = "currently_smoke_cig_father_old_survey",
         "past_smoke_cig_mother_substance_abuse_survey" = "past_smoke_cig_mother_old_survey",
         "currently_smoke_cig_mother_substance_abuse_survey" = "currently_smoke_cig_mother_old_survey") %>%
  select(-old_survey) %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(past_smoke_cig_father_research_match = past_smoke_cig_father,
         currently_smoke_cig_father_research_match = currently_smoke_cig_father,
         past_smoke_cig_mother_research_match = past_smoke_cig_mother,
         currently_smoke_cig_mother_research_match = currently_smoke_cig_mother) %>%
  mutate(past_smoke_cig_father = coalesce(past_smoke_cig_father, past_smoke_cig_father_substance_abuse_survey),
         currently_smoke_cig_father = coalesce(currently_smoke_cig_father, currently_smoke_cig_father_substance_abuse_survey),
         past_smoke_cig_mother = coalesce(past_smoke_cig_mother, past_smoke_cig_mother_substance_abuse_survey),
         currently_smoke_cig_mother = coalesce(currently_smoke_cig_mother, currently_smoke_cig_mother_substance_abuse_survey)
  )

COMBINED_SURVEY_DATA = bind_rows(
  REACH_SURVEY %>% mutate(across(everything(), ~ as.character(.))),
  SPARK_RESEARCH_MATCH_SURVEY_01 %>% mutate(across(everything(), ~ as.character(.))),
  SPARK_RESEARCH_MATCH_SURVEY_02 %>% mutate(across(everything(), ~ as.character(.))),
  SSC_COMBINED_SURVEY %>% mutate(across(everything(), ~ as.character(.)))
  ) %>%
  select(Family_ID, Cohort, Survey_ID, in_ssc_research_match_survey, in_ssc_substance_abuse_survey,
         Father_ID, Mother_ID,
         past_smoke_cig_father, currently_smoke_cig_father, past_smoke_cig_father_substance_abuse_survey, currently_smoke_cig_father_substance_abuse_survey, past_smoke_cig_father_research_match, currently_smoke_cig_father_research_match,
         past_smoke_cig_mother, currently_smoke_cig_mother, past_smoke_cig_mother_substance_abuse_survey, currently_smoke_cig_mother_substance_abuse_survey, past_smoke_cig_mother_research_match, currently_smoke_cig_mother_research_match,
         "cig_freq_age_1_father", "cig_freq_age_2_father", "cig_freq_age_3_father", "cig_freq_age_4_father", "cig_freq_age_5_father", "cig_freq_age_6_father", "cig_freq_age_7_father", "cig_freq_age_8_father", "cig_freq_age_9_father",
         "cig_freq_age_1_mother", "cig_freq_age_2_mother", "cig_freq_age_3_mother", "cig_freq_age_4_mother", "cig_freq_age_5_mother", "cig_freq_age_6_mother", "cig_freq_age_7_mother", "cig_freq_age_8_mother", "cig_freq_age_9_mother"
         ) %>%
  mutate(across(starts_with("past_smoke_cig"), as.numeric)) %>%
  mutate(across(starts_with("currently_smoke_cig"), as.numeric)) %>%
  mutate(across(starts_with("cig_freq_age"), as.numeric)) %>%
  mutate(across(
    .cols = starts_with("cig_freq_age_"),
    .fns = ~ case_when(
      . == 1    ~ 1.5,
      . == 2    ~ 4,
      . == 3    ~ 8,
      . == 4    ~ 15.5,
      . == 5    ~ 30,
      TRUE      ~ .
    ),
    .names = "mean_{col}"
  )) 
  

###################################################################################################
# Basic discrepancy analysis
###################################################################################################

# SSC smoking discrepancies between the old substance abuse survey and the ResearchMatch survey
father_smoke_data_summary = SSC_COMBINED_SURVEY %>%
  count(past_smoke_cig_father_substance_abuse_survey, currently_smoke_cig_father_substance_abuse_survey, past_smoke_cig_father_research_match, currently_smoke_cig_father_research_match)
mother_smoke_data_summary = SSC_COMBINED_SURVEY %>%
  count(past_smoke_cig_mother_substance_abuse_survey, currently_smoke_cig_mother_substance_abuse_survey, past_smoke_cig_mother_research_match, currently_smoke_cig_mother_research_match)
ssc_father_smoking_discrepancies = SSC_COMBINED_SURVEY %>%
  filter((past_smoke_cig_father_substance_abuse_survey == 1 | currently_smoke_cig_father_substance_abuse_survey == 1) & (past_smoke_cig_father_research_match == 0) ) %>%
  mutate(ssc_father_smoking_discrepancy = 1) # 24
ssc_mother_smoking_discrepancies = SSC_COMBINED_SURVEY %>%
  filter((past_smoke_cig_mother_substance_abuse_survey == 1 | currently_smoke_cig_mother_substance_abuse_survey == 1) & (past_smoke_cig_mother_research_match == 0) ) %>%
  mutate(ssc_mother_smoking_discrepancy = 1) # 13
# There are some other prima facie discrepancies, but I doubt they're important enough to note.


# Sex discrepancies in the ResearchMatch smoking surveys
spark_survey_01_sex_discrepancies = RM0053Sebat_ParentSurvey.csv %>%
  select(SFID, Authorizer_SPID, authorizer_sex, relationship_proband.x, Expected_SPID_P2, Expected_SPID_P2_sex, relationship_proband.y) %>%
  filter(
    (authorizer_sex == "female" & relationship_proband.x == 2) | 
    (authorizer_sex == "male" & relationship_proband.x == 1) |
    (Expected_SPID_P2_sex == "female" & relationship_proband.y == 2) | 
    (Expected_SPID_P2_sex == "male" & relationship_proband.y == 1) |
    (relationship_proband.x == relationship_proband.y)
  ) %>% # 7
  mutate(survey_sex_discrepancy = 1)

spark_survey_02_sex_discrepancies = RM0053Sebat_2021_Parent_Survey.xlsx %>%
  select(SFID, Authorizer_SPID.x, authorizer_sex.x, relationship_proband.x, Authorizer_SPID.y, authorizer_sex.y, relationship_proband.y) %>%
  filter(
    (authorizer_sex.x == "female" & relationship_proband.x == 2) | 
    (authorizer_sex.x == "male" & relationship_proband.x == 1) |
    (relationship_proband.x == relationship_proband.y)
  ) %>% # 5
  mutate(survey_sex_discrepancy = 1)

ssc_research_match_survey_sex_discrepancies = full_join(SSC0004Sebat_04.03.20_P1_ParentSurvey.csv, SSC0004Sebat_04.03.20_P2_ParentSurvey.csv, by = "FamilyID") %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  mutate(across(everything(), ~ na_if(., "-666"))) %>%
  mutate(across(everything(), ~ na_if(., "-999"))) %>%
  mutate(across(everything(), ~ na_if(., "NULL"))) %>%
  rename_with(~ str_replace(., "\\.x$", "_mother"), ends_with(".x")) %>%
  rename_with(~ str_replace(., "\\.y$", "_father"), ends_with(".y")) %>%
  rename("Family_ID" = "FamilyID") %>%
  left_join(SSC_PARENTS, by = "Family_ID") %>%
  select(Family_ID, SFARI_ID_Authorizer_mother, Gender_Authorizer_mother, SFARI_ID_Authorizer_father, Gender_Authorizer_father) %>%
  filter(Gender_Authorizer_mother == "M") %>% # 2
  mutate(survey_sex_discrepancy = 1)

# Families with NA in the ID columns
reach_na_ids = REACH_SURVEY %>%
  filter(is.na(Father_ID) | is.na(Mother_ID)) %>% # 5 samples 
  mutate(missing_id_reach = 1)
# Family F0120 is in the table 2 times, with different Father_IDs and no Mother_IDs... I suspect one of the Father_IDs is the Mother_ID but won't check right now.
# The other 3 rows have Family_IDs F0162-A, F0162-B, and F0162-C. I'm not going to bother to determine why they have these different suffixes.

spark_survey_01_na_ids = SPARK_RESEARCH_MATCH_SURVEY_01 %>%
  filter(is.na(Father_ID) | is.na(Mother_ID)) # 0

spark_survey_02_na_ids = SPARK_RESEARCH_MATCH_SURVEY_02 %>%
  filter(is.na(Father_ID) | is.na(Mother_ID)) %>% # 1
  mutate(missing_id_spark = 1)
# Family SF0248875 only has Mother_ID SP0248874.

ssc_substance_abuse_survey_na_ids = SSC_SUBSTANCE_ABUSE_HISTORY_SURVEY %>%
  filter(is.na(Father_ID) | is.na(Mother_ID)) # 342 
# Father_ID and Mother_ID all NA...likely meaning these families don't appear in the joint-genotyped SSC VCF that I use for DNM calling.
ssc_research_match_survey_na_ids = SSC_RESEARCH_MATCH_SURVEY %>%
  filter(is.na(Father_ID) | is.na(Mother_ID)) # 20
# Again, Father_ID and Mother_ID all NA (for same reason).
ssc_combined_survey_na_ids = SSC_COMBINED_SURVEY %>%
  filter(is.na(Father_ID) | is.na(Mother_ID)) %>% # 343
  mutate(missing_id_ssc = 1)
# Again, Father_ID and Mother_ID all NA


# Redo this part later:
### # Basic summary statistics (number of complete data...)
### x0 = REACH_SURVEY %>%
###   mutate(non_na_count = rowSums(!is.na(select(., past_smoke_cig_mother, past_smoke_cig_father))))
### sum(x0$non_na_count)
### x1 = RM0053Sebat_ParentSurvey.csv %>%
###   mutate(non_na_count = rowSums(!is.na(select(., past_smoke_cig.x, past_smoke_cig.y))))
### sum(x1$non_na_count)
### x2 = RM0053Sebat_2021_Parent_Survey.xlsx %>%
###   mutate(non_na_count = rowSums(!is.na(select(., past_smoke_cig.x, past_smoke_cig.y))))
### sum(x2$non_na_count)
### x3 = SSC_RESEARCH_MATCH_SURVEY %>% 
###   mutate(non_na_count = rowSums(!is.na(select(., past_smoke_cig_father, past_smoke_cig_mother))))
### sum(x3$non_na_count)
### x4 = SSC_SUBSTANCE_ABUSE_HISTORY_SURVEY  %>%
###   mutate(non_na_count = rowSums(!is.na(select(., past_smoke_cig_father_old_survey, past_smoke_cig_mother_old_survey))))
### sum(x4$non_na_count)
### x5 = PregnancyExposureSurvey %>%
###   mutate(non_na_count = rowSums(!is.na(select(., mom_smoking_prior, dad_smoking_prior))))
### sum(x5$non_na_count)
### # Get complete families
### sum(complete.cases(REACH_SURVEY)) # 46
### sum(complete.cases(SSC_SUBSTANCE_ABUSE_HISTORY_SURVEY)) # 2593
### sum(complete.cases(SSC_RESEARCH_MATCH_SURVEY)) # 187
### sum(complete.cases(RM0053Sebat_ParentSurvey.csv)) # 87
### sum(complete.cases(RM0053Sebat_2021_Parent_Survey.xlsx)) # 154


###################################################################################################
# Wrangling DNM data
###################################################################################################

# REACH
reach_jg.preds1.rare.exclude_segdup_str_umap.sorted.bed = fread("dnmData/reach-jg.preds1.rare.exclude-segdup-str-umap.sorted.bed") %>%
  separate(V4, into = c("chrom", "pos", "idk", "ref", "alt", "IID"), sep = ':', extra = "drop") %>%
  mutate(snp = (nchar(ref) == 1 & nchar(alt) == 1)) %>%
  mutate(sex_chrom = grepl("X|Y", chrom))

# SPARK
dnm_train0_wgs_snvs_indels_msc_preds_filtered.bed = fread("dnmData/dnm_train0_wgs_snvs_indels_msc_preds_filtered.bed") %>%
  rename("chrom" = "V1", "start" = "V2", "end" = "V3") %>%
  separate(V4, into = c("ref", "alt", "IID"), sep = ':', extra = "drop") %>%
  mutate(snp = (nchar(ref) == 1 & nchar(alt) == 1)) %>%
  mutate(sex_chrom = grepl("X|Y", chrom))

# SSC
ssc.preds1.rare.exclude_segdup_str_umap.bed = fread("dnmData/ssc.preds1.rare.exclude-segdup-str-umap.bed") %>%
  separate(V4, into = c("chrom", "pos", "ref", "alt", "IID"), sep = ':', extra = "drop") %>%
  mutate(snp = (nchar(ref) == 1 & nchar(alt) == 1)) %>%
  mutate(sex_chrom = grepl("X|Y", chrom))

reachSnvCountsAutosomal = reach_jg.preds1.rare.exclude_segdup_str_umap.sorted.bed %>%
  filter(snp == TRUE) %>%
  filter(sex_chrom == FALSE) %>%
  count(IID) %>%
  mutate(cohort = "REACH")
sparkSnvCountsAutosomal = dnm_train0_wgs_snvs_indels_msc_preds_filtered.bed %>%
  filter(snp == TRUE) %>%
  filter(sex_chrom == FALSE) %>%
  count(IID) %>%
  mutate(cohort = "SPARK")
sscSnvCountsAutosomal = ssc.preds1.rare.exclude_segdup_str_umap.bed %>%
  filter(snp == TRUE) %>%
  filter(sex_chrom == FALSE) %>%
  count(IID) %>%
  mutate(cohort = "SSC")

DNM_AUTOSOMAL_SNV_COUNTS = bind_rows(reachSnvCountsAutosomal,
                                     sparkSnvCountsAutosomal,
                                     sscSnvCountsAutosomal) %>%
  rename("Sample_ID" = "IID", "AUTOSOMAL_SNV_DNM_COUNT" = "n", "Cohort" = "cohort")
  
###################################################################################################
# Wrangling parental ages at birth of offspring
###################################################################################################

# REACH
REACH_MASTER_09.13.18.xlsx_master_table.tsv_DOB = REACH_MASTER_09.13.18.xlsx_master_table.tsv %>%
  select(Sample_ID, Subject_ID, Family_ID, DOB, Mother_ID, Father_ID, Rel_Proband) %>%
  filter(DOB != "NO SHOW") %>%
  mutate(DOB = as.Date(DOB, format = "%m/%d/%Y") )

REACH_AGES = REACH_MASTER_09.13.18.xlsx_master_table.tsv_DOB %>%
  left_join(REACH_MASTER_09.13.18.xlsx_master_table.tsv_DOB %>% select(Subject_ID, DOB, Sample_ID) %>% rename("Mother_ID" = "Subject_ID", "Mother_DOB" = "DOB", "Sample_ID_Mother" = "Sample_ID") , by = "Mother_ID") %>%
  left_join(REACH_MASTER_09.13.18.xlsx_master_table.tsv_DOB %>% select(Subject_ID, DOB, Sample_ID) %>% rename("Father_ID" = "Subject_ID", "Father_DOB" = "DOB", "Sample_ID_Father" = "Sample_ID") , by = "Father_ID") %>%
  filter(!is.na(Mother_DOB) & !is.na(Father_DOB)) %>%
  mutate(
    Mother_Age_At_Birth = as.numeric(difftime(DOB, Mother_DOB, units = "days")) / 365.25,
    Father_Age_At_Birth = as.numeric(difftime(DOB, Father_DOB, units = "days")) / 365.25
  ) %>%
  select(Sample_ID, Sample_ID_Father, Sample_ID_Mother, Father_Age_At_Birth, Mother_Age_At_Birth) %>%
  rename("Father_ID" = "Sample_ID_Father", "Mother_ID" = "Sample_ID_Mother")

# SPARK
individuals_registration_2022_12_12.csv_ages = fread("resources/SPARK_collection_v9_2022-12-12/individuals_registration_2022-12-12.csv") %>%
  select(subject_sp_id, family_sf_id, biomother_sp_id, biofather_sp_id, age_at_registration_months) # age_at_registration_years)

SPARK_AGES = individuals_registration_2022_12_12.csv_ages %>%
  left_join(individuals_registration_2022_12_12.csv_ages %>% select(subject_sp_id, age_at_registration_months) %>% mutate(Sample_ID_Mother = subject_sp_id) %>% rename("biomother_sp_id" = "subject_sp_id", "mother_age_at_registration_months" = "age_at_registration_months"), by = "biomother_sp_id") %>%
  left_join(individuals_registration_2022_12_12.csv_ages %>% select(subject_sp_id, age_at_registration_months) %>% mutate(Sample_ID_Father = subject_sp_id) %>% rename("biofather_sp_id" = "subject_sp_id", "father_age_at_registration_months" = "age_at_registration_months"), by = "biofather_sp_id") %>%
  filter(!is.na(mother_age_at_registration_months) & !is.na(father_age_at_registration_months)) %>%
  mutate(
    Mother_Age_At_Birth = (mother_age_at_registration_months - age_at_registration_months) / 12.0,
    Father_Age_At_Birth = (father_age_at_registration_months - age_at_registration_months) / 12.0
  ) %>%
  rename("Sample_ID" = "subject_sp_id") %>%
  select(Sample_ID, Sample_ID_Father, Sample_ID_Mother, Father_Age_At_Birth, Mother_Age_At_Birth) %>%
  rename("Father_ID" = "Sample_ID_Father", "Mother_ID" = "Sample_ID_Mother")


# SSC
SSC_Age_of_Subjects_and_ParentalAgeAtBirth.csv = fread("resources/SSC_Age_of_Subjects_&_ParentalAgeAtBirth.csv") %>%
  select(family, `id()`, father, mother, birth, father_birth, mother_birth, subject_age, father_age, mother_age, father_age_birth, mother_age_birth) %>%
  select(`id()`, father, mother, birth, father_birth, mother_birth, subject_age, father_age, mother_age, father_age_birth, mother_age_birth) %>%
  rename("subject_id" = "id()") %>%
  filter(!(father_birth == "." & mother_birth == ".")) %>%
  mutate(
    Mother_Age_At_Birth = mother_age_birth / 12.0,
    Father_Age_At_Birth = father_age_birth / 12.0
  )

SSC_AGES = SSC_Age_of_Subjects_and_ParentalAgeAtBirth.csv %>%
  left_join(nygc_sfari_id_map.csv %>% rename("subject_id" = "SFARI ID", "Sample_ID" = "Repository Id"), by = "subject_id") %>%
  left_join(nygc_sfari_id_map.csv %>% rename("mother" = "SFARI ID", "Sample_ID_Mother" = "Repository Id") %>% select(mother, Sample_ID_Mother), by = "mother") %>%
  left_join(nygc_sfari_id_map.csv %>% rename("father" = "SFARI ID", "Sample_ID_Father" = "Repository Id") %>% select(father, Sample_ID_Father), by = "father") %>%
  select(Sample_ID, Sample_ID_Father, Sample_ID_Mother, Father_Age_At_Birth, Mother_Age_At_Birth) %>%
  rename("Father_ID" = "Sample_ID_Father", "Mother_ID" = "Sample_ID_Mother")

PARENTAL_AGES = bind_rows(REACH_AGES, SPARK_AGES, SSC_AGES)

###################################################################################################
# Combine DNM data with PARENTAL_AGES
###################################################################################################

dnms.parentalAges = DNM_AUTOSOMAL_SNV_COUNTS %>%
  left_join(PARENTAL_AGES, by = "Sample_ID") %>%
  mutate(Cohort = as.factor(Cohort))

###################################################################################################
# Combine DNMs, parental ages, and PCs
###################################################################################################

reach_spark_ssc_geno0.05_samp0.05_founders.pc = fread("../misc/reach_spark_ssc_geno0.05_samp0.05_founders.pc")
reach_spark_ssc_geno0.05_samp0.05_nonfounders.proj = fread("../misc/reach_spark_ssc_geno0.05_samp0.05_nonfounders.proj")
cohort_pcs = bind_rows(reach_spark_ssc_geno0.05_samp0.05_founders.pc,
                       reach_spark_ssc_geno0.05_samp0.05_nonfounders.proj) %>%
  select(-FID)

dnmSnvCounts.parentalAges.PCs = dnms.parentalAges %>%
  left_join(cohort_pcs, by = c("Sample_ID" = "IID")) %>%
  left_join(cohort_pcs %>% rename("Father_ID" = "IID"), by = "Father_ID", suffix = c("", "_Father")) %>%
  left_join(cohort_pcs %>% rename("Mother_ID" = "IID"), by = "Mother_ID", suffix = c("", "_Mother"))


###################################################################################################
# Combine DNMs, parental ages, and PCs with the smoking survey data
###################################################################################################

smoking.dnmSnvCounts.parentalAges.PCs = COMBINED_SURVEY_DATA %>% 
  filter(!is.na(Father_ID) | !is.na(Mother_ID)) %>%
  left_join(dnmSnvCounts.parentalAges.PCs %>% select(-Cohort), by = c("Father_ID", "Mother_ID")) %>%
  select(Family_ID, Cohort, Survey_ID, in_ssc_research_match_survey, in_ssc_substance_abuse_survey,
         Sample_ID,
         Father_ID, Mother_ID, Father_Age_At_Birth, Mother_Age_At_Birth,
         past_smoke_cig_father, currently_smoke_cig_father, past_smoke_cig_father_substance_abuse_survey, currently_smoke_cig_father_substance_abuse_survey, past_smoke_cig_father_research_match, currently_smoke_cig_father_research_match,
         past_smoke_cig_mother, currently_smoke_cig_mother, past_smoke_cig_mother_substance_abuse_survey, currently_smoke_cig_mother_substance_abuse_survey, past_smoke_cig_mother_research_match, currently_smoke_cig_mother_research_match,
         contains("cig_freq_age"),
         contains("PC"),
         AUTOSOMAL_SNV_DNM_COUNT,
  ) %>%
  rename("Autosomal_SNV_DNMs" = "AUTOSOMAL_SNV_DNM_COUNT") %>%
  mutate(across(c(Father_Age_At_Birth, Mother_Age_At_Birth), as.numeric))



###################################################################################################
# Calculate pack years
###################################################################################################

# Functions
calculate_pack_years_range <- function(parental_age_at_birth_of_child,
                                       mean_cig_freq_interval,
                                       age_start, age_end) {
  if (is.na(parental_age_at_birth_of_child)) { return(NA) }
  if (parental_age_at_birth_of_child < age_start) { return(0) }
  else if ( (age_start < parental_age_at_birth_of_child) & (parental_age_at_birth_of_child <= age_end) ) {
    return((age_end - parental_age_at_birth_of_child + 1) * mean_cig_freq_interval)
  }
  else {
    return((age_end - age_start + 1) * mean_cig_freq_interval)
  }
}

calculate_pack_years <- function(parental_age_at_birth_of_child,
                                 mean_cig_freq_age_1,
                                 mean_cig_freq_age_2,
                                 mean_cig_freq_age_3,
                                 mean_cig_freq_age_4,
                                 mean_cig_freq_age_5,
                                 mean_cig_freq_age_6,
                                 mean_cig_freq_age_7,
                                 mean_cig_freq_age_8,
                                 mean_cig_freq_age_9) {
  pack_years = 0 
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_1, 13, 18)
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_2, 19, 25)
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_3, 26, 30)
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_4, 31, 35)
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_5, 36, 40)
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_6, 41, 45)
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_7, 46, 50)
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_8, 51, 55)
  pack_years = pack_years + calculate_pack_years_range(parental_age_at_birth_of_child, mean_cig_freq_age_9, 56, 60)
  return(pack_years/20.0)
}

smoking.dnmSnvCounts.parentalAges.PCs$Father_Pack_Years = mapply(
    calculate_pack_years, 
    smoking.dnmSnvCounts.parentalAges.PCs$Father_Age_At_Birth,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_1_father,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_2_father,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_3_father,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_4_father,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_5_father,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_6_father,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_7_father,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_8_father,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_9_father
    )

smoking.dnmSnvCounts.parentalAges.PCs$Mother_Pack_Years = mapply(
    calculate_pack_years, 
    smoking.dnmSnvCounts.parentalAges.PCs$Mother_Age_At_Birth,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_1_mother,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_2_mother,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_3_mother,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_4_mother,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_5_mother,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_6_mother,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_7_mother,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_8_mother,
    smoking.dnmSnvCounts.parentalAges.PCs$mean_cig_freq_age_9_mother
    )

smoking.dnmSnvCounts.parentalAges.PCs.packYears = smoking.dnmSnvCounts.parentalAges.PCs %>%
  mutate(Father_Pack_Years = ifelse(past_smoke_cig_father == 0 & currently_smoke_cig_father == 0, 0, Father_Pack_Years)) %>%
  mutate(Mother_Pack_Years = ifelse(past_smoke_cig_mother == 0 & currently_smoke_cig_mother == 0, 0, Mother_Pack_Years)) %>%
  mutate(Total_Pack_Years = Father_Pack_Years + Mother_Pack_Years) %>%
  mutate(Father_Ever_Smoke = ifelse(past_smoke_cig_father == 1 | currently_smoke_cig_father == 1, 1, 0)) %>%
  mutate(Mother_Ever_Smoke = ifelse(past_smoke_cig_mother == 1 | currently_smoke_cig_mother == 1, 1, 0))
  
DF_0 = smoking.dnmSnvCounts.parentalAges.PCs.packYears

###################################################################################################
# Assign ancestry
###################################################################################################

SPARK.iWES_v2.ancestry.2023_01.tsv = fread("resources/SPARK.iWES_v2.ancestry.2023_01.tsv") %>%
  select(spid, superclass) %>%
  filter(!grepl("UNKNOWN", superclass))

ancestry_training_data = dnmSnvCounts.parentalAges.PCs %>%
  select(Sample_ID, Father_ID, Mother_ID, contains("PC"), AUTOSOMAL_SNV_DNM_COUNT) %>%
  left_join(SPARK.iWES_v2.ancestry.2023_01.tsv, by = c("Sample_ID" = "spid")) %>%
  mutate(Ancestry = as.factor(superclass)) %>%
  select(Sample_ID, Ancestry, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10, AUTOSOMAL_SNV_DNM_COUNT) %>%
  filter(!is.na(Ancestry))
ancestry_model = multinom(Ancestry ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = ancestry_training_data)

ancestry_features_table_offspring = DF_0 %>%
  select(Sample_ID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10)
ancestry_predictions_offspring = predict(ancestry_model, newdata = ancestry_features_table_offspring %>% select(-Sample_ID))
ancestry_features_table_offspring = ancestry_features_table_offspring %>% 
  mutate(Ancestry_Predictions = ancestry_predictions_offspring)

ancestry_features_table_fathers = DF_0 %>%
  select(Father_ID, PC1_Father, PC2_Father, PC3_Father, PC4_Father, PC5_Father, PC6_Father, PC7_Father, PC8_Father, PC9_Father, PC10_Father) %>%
  rename_with(~ str_replace(., "_Father$", ""))
ancestry_predictions_fathers = predict(ancestry_model, newdata = ancestry_features_table_fathers %>% select(-Father_ID))
ancestry_features_table_fathers = ancestry_features_table_fathers %>% 
  mutate(Ancestry_Predictions = ancestry_predictions_fathers)

ancestry_features_table_mothers = DF_0 %>%
  select(Mother_ID, PC1_Mother, PC2_Mother, PC3_Mother, PC4_Mother, PC5_Mother, PC6_Mother, PC7_Mother, PC8_Mother, PC9_Mother, PC10_Mother) %>%
  rename_with(~ str_replace(., "_Mother$", ""))
ancestry_predictions_mothers = predict(ancestry_model, newdata = ancestry_features_table_mothers %>% select(-Mother_ID))
ancestry_features_table_mothers = ancestry_features_table_mothers %>% 
  mutate(Ancestry_Predictions = ancestry_predictions_mothers)

ggplot(ancestry_features_table_offspring, aes(x = PC1, y = PC2, color = Ancestry_Predictions)) + 
  geom_point()
ggplot(ancestry_features_table_fathers, aes(x = PC1, y = PC2, color = Ancestry_Predictions)) + 
  geom_point()
ggplot(ancestry_features_table_mothers, aes(x = PC1, y = PC2, color = Ancestry_Predictions)) + 
  geom_point()

ancestry_offspring_predictions = ancestry_features_table_offspring %>%
  select(Sample_ID, Ancestry_Predictions) %>%
  rename("Ancestry_Prediction_Offspring" = "Ancestry_Predictions") %>%
  filter(!is.na(Sample_ID)) %>%
  distinct()
ancestry_father_predictions = ancestry_features_table_fathers %>%
  select(Father_ID, Ancestry_Predictions) %>%
  rename("Ancestry_Prediction_Father" = "Ancestry_Predictions") %>%
  filter(!is.na(Father_ID)) %>%
  distinct()
ancestry_mother_predictions = ancestry_features_table_mothers %>%
  select(Mother_ID, Ancestry_Predictions) %>%
  rename("Ancestry_Prediction_Mother" = "Ancestry_Predictions") %>%
  filter(!is.na(Mother_ID)) %>%
  distinct()

DF_1 = DF_0 %>%
  left_join(ancestry_offspring_predictions, by = "Sample_ID") %>%
  left_join(ancestry_father_predictions, by = "Father_ID") %>%
  left_join(ancestry_mother_predictions, by = "Mother_ID") 


###################################################################################################
# Parental age and DNM issues
###################################################################################################

ggplot(DF_1, aes(x = Father_Age_At_Birth, y = Mother_Age_At_Birth, color = Cohort)) + 
  geom_point()

parental_age_outliers = DF_1 %>%
  filter(Father_Age_At_Birth < 13 | Father_Age_At_Birth > 50 | Mother_Age_At_Birth < 13 | Mother_Age_At_Birth > 50) %>%
  filter(Mother_Age_At_Birth < 0) %>%
  select(Cohort, Family_ID, Survey_ID, Sample_ID, Father_ID, Mother_ID, Father_Age_At_Birth, Mother_Age_At_Birth) %>%
  mutate(parental_age_issue = 1)
# 1 mother sample (Family_ID F0250 and Mother_ID REACH000603) has age less than 0 (-6.622861)

dnm_outliers = DF_1 %>%
  select(Cohort, Family_ID, Sample_ID, Father_ID, Father_Age_At_Birth, Mother_Age_At_Birth, Autosomal_SNV_DNMs) %>%
  filter(Autosomal_SNV_DNMs > 100) %>% # 53
  filter(Autosomal_SNV_DNMs > 150) %>% # 13
  filter(Autosomal_SNV_DNMs > 300) %>% # 11
  filter(Autosomal_SNV_DNMs > 1000) %>% # 11
  filter(Autosomal_SNV_DNMs > 10000) # 10
# I'll use 150 DNMs as a filtering threshold (and note the rest as outliers)
dnm_outliers = DF_1 %>%
  select(Cohort, Family_ID, Sample_ID, Father_ID, Father_Age_At_Birth, Mother_Age_At_Birth, Autosomal_SNV_DNMs) %>%
  filter(Autosomal_SNV_DNMs > 100) %>% # 53
  filter(Autosomal_SNV_DNMs > 150) %>% 
  mutate(dnm_outlier = 1)


###################################################################################################
# Make discrepancy columns and finalize data.frame/data.table
###################################################################################################

DF_2 = DF_1 %>%
  left_join(parental_age_outliers %>% select(Family_ID, parental_age_issue), by = "Family_ID") %>%
  left_join(dnm_outliers %>% select(Sample_ID, dnm_outlier), by = "Sample_ID") %>%
  left_join(ssc_father_smoking_discrepancies %>% select(Family_ID, ssc_father_smoking_discrepancy), by = "Family_ID") %>%
  left_join(ssc_mother_smoking_discrepancies %>% select(Family_ID, ssc_mother_smoking_discrepancy), by = "Family_ID") %>%
  left_join(spark_survey_01_sex_discrepancies %>% select(SFID, survey_sex_discrepancy) %>% rename("spark_survey_sex_discrepancy_01" = "survey_sex_discrepancy"), by = c("Family_ID" = "SFID")) %>%
  left_join(spark_survey_02_sex_discrepancies %>% select(SFID, survey_sex_discrepancy) %>% rename("spark_survey_sex_discrepancy_02" = "survey_sex_discrepancy"), by = c("Family_ID" = "SFID")) %>%
  left_join(ssc_research_match_survey_sex_discrepancies %>% select(Family_ID, survey_sex_discrepancy) %>% rename("ssc_survey_sex_discrepancy" = "survey_sex_discrepancy"), by = "Family_ID") %>%
  mutate(Parental_Age_Issue = parental_age_issue) %>%
  mutate(Missing_Parental_Age = ifelse(is.na(Father_Age_At_Birth) | is.na(Mother_Age_At_Birth), 1, 0)) %>%
  mutate(DNM_Outlier = dnm_outlier) %>%
  mutate(Father_Smoking_Discrepancy = ssc_father_smoking_discrepancy) %>%
  mutate(Mother_Smoking_Discrepancy = ssc_mother_smoking_discrepancy) %>%
  mutate(Survey_Sex_Discrepancy = coalesce(spark_survey_sex_discrepancy_01, spark_survey_sex_discrepancy_02, ssc_survey_sex_discrepancy)) %>%
  mutate(Missing_ID = ifelse(is.na(Father_ID) | is.na(Mother_ID) | is.na(Sample_ID), 1, 0)) %>%
  select(-parental_age_issue, -dnm_outlier, -ssc_father_smoking_discrepancy, -ssc_mother_smoking_discrepancy,
         -spark_survey_sex_discrepancy_01, -spark_survey_sex_discrepancy_02, -ssc_survey_sex_discrepancy) %>%
  mutate(across(c(Parental_Age_Issue, DNM_Outlier, Father_Smoking_Discrepancy, Mother_Smoking_Discrepancy, Survey_Sex_Discrepancy), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(Problematic_Row = Parental_Age_Issue + Missing_Parental_Age + DNM_Outlier + Father_Smoking_Discrepancy + Mother_Smoking_Discrepancy + Survey_Sex_Discrepancy + Missing_ID) %>%
  mutate(Family_ID = as.factor(Family_ID)) %>%
  mutate(Cohort = as.factor(Cohort)) %>%
  mutate(Survey_ID = as.factor(Survey_ID))

# Get the families that are NA for both father and mother IDs
COMBINED_SURVEY_DATA_ID_NAs = COMBINED_SURVEY_DATA %>%
  filter(!(!is.na(Father_ID) | !is.na(Mother_ID))) %>%
  mutate(Missing_Family = 1) %>%
  mutate(Cohort = ifelse(grepl("F0162", Family_ID), "REACH", Cohort))

# Merge with family rows that have neither Father_ID nor Mother_ID...
DF_3 = bind_rows(DF_2, COMBINED_SURVEY_DATA_ID_NAs) %>%
  rename("Past_Smoke_Father" = "past_smoke_cig_father", "Currently_Smoke_Father" = "currently_smoke_cig_father") %>%
  rename("Past_Smoke_Mother" = "past_smoke_cig_mother", "Currently_Smoke_Mother" = "currently_smoke_cig_mother") %>%
  rename("Ancestry_Offspring" = "Ancestry_Prediction_Offspring") %>%
  rename("Ancestry_Father" = "Ancestry_Prediction_Father") %>%
  rename("Ancestry_Mother" = "Ancestry_Prediction_Mother") %>%
  mutate(Missing_Family = ifelse(is.na(Missing_Family), 0, 1)) %>%
  mutate(Complete_Data_Pack_Years = ifelse(
    !is.na(Father_Pack_Years) & !is.na(Mother_Pack_Years) &
    !is.na(Father_Age_At_Birth) & !is.na(Mother_Age_At_Birth) &
    !is.na(Autosomal_SNV_DNMs) & 
    !is.na(PC1) &
    (DNM_Outlier == 0), 1, 0
  )
  ) %>%
  mutate(Complete_Data_Smoking_Status = ifelse(
    !is.na(Past_Smoke_Father) & !is.na(Currently_Smoke_Father) &
    !is.na(Past_Smoke_Mother) & !is.na(Currently_Smoke_Mother) &
    !is.na(Father_Age_At_Birth) & !is.na(Mother_Age_At_Birth) &
    !is.na(Autosomal_SNV_DNMs) & 
    !is.na(PC1) &
    (DNM_Outlier == 0), 1, 0
  )
  )

df_output_unclean = DF_3
df_output_clean = DF_3 %>%
  select(Cohort, Survey_ID, Family_ID,
         Sample_ID, Father_ID, Mother_ID,
         Father_Age_At_Birth, Mother_Age_At_Birth,
         Past_Smoke_Father, Currently_Smoke_Father, Father_Ever_Smoke,
         Past_Smoke_Mother, Currently_Smoke_Mother, Mother_Ever_Smoke,
         Father_Pack_Years, Mother_Pack_Years, Total_Pack_Years,
         Autosomal_SNV_DNMs,
         Ancestry_Offspring, Ancestry_Father, Ancestry_Mother,
         contains("PC"),
         Parental_Age_Issue, Missing_Parental_Age, DNM_Outlier, Father_Smoking_Discrepancy, Mother_Smoking_Discrepancy, Survey_Sex_Discrepancy, Missing_ID, Missing_Family, Problematic_Row)


###################################################################################################
# Basic plots and analysis
###################################################################################################

# Ancestry
ggplot(df_output_clean, aes(x = PC1, y = PC2, color = Ancestry_Offspring)) + 
  geom_point()
ggplot(df_output_clean, aes(x = PC1_Father, y = PC2_Father, color = Ancestry_Father)) + 
  geom_point()
ggplot(df_output_clean, aes(x = PC1_Mother, y = PC2_Mother, color = Ancestry_Mother)) + 
  geom_point()

# DNMs and parental age
ggplot(df_output_clean %>% filter(DNM_Outlier == 0) %>% filter(Parental_Age_Issue == 0), aes(x = Father_Age_At_Birth, y = Autosomal_SNV_DNMs, color = Cohort)) + 
  geom_point()
ggplot(df_output_clean %>% filter(DNM_Outlier == 0) %>% filter(Parental_Age_Issue == 0), aes(x = Mother_Age_At_Birth, y = Autosomal_SNV_DNMs, color = Cohort)) + 
  geom_point()

# Total_Pack_Years (and a variety of filters)
summary(
  lm(Autosomal_SNV_DNMs ~ Total_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(Problematic_Row == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Total_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Total_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Father_Smoking_Discrepancy == 0 & Mother_Smoking_Discrepancy == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Total_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Survey_Sex_Discrepancy == 0))
)

# Father_Pack_Years and Mother_Pack_Years
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Pack_Years + Mother_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(Problematic_Row == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Pack_Years + Mother_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Pack_Years + Mother_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Father_Smoking_Discrepancy == 0 & Mother_Smoking_Discrepancy == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Pack_Years + Mother_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Survey_Sex_Discrepancy == 0))
)

# Father_Pack_Years
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(Problematic_Row == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Father_Smoking_Discrepancy == 0 & Mother_Smoking_Discrepancy == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Survey_Sex_Discrepancy == 0))
)

# Mother_Pack_Years
summary(
  lm(Autosomal_SNV_DNMs ~ Mother_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(Problematic_Row == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Mother_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Mother_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Mother_Smoking_Discrepancy == 0 & Mother_Smoking_Discrepancy == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Mother_Pack_Years + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Survey_Sex_Discrepancy == 0))
)

# Father_Ever_Smoke and Mother_Ever_Smoke
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Ever_Smoke + Mother_Ever_Smoke + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(Problematic_Row == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Ever_Smoke + Mother_Ever_Smoke + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Ever_Smoke + Mother_Ever_Smoke + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Mother_Smoking_Discrepancy == 0 & Mother_Smoking_Discrepancy == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Ever_Smoke + Mother_Ever_Smoke + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Survey_Sex_Discrepancy == 0))
)
# Father_Ever_Smoke is significant in this model. (As well as PC1 more generally in many of the models.)

# Father_Ever_Smoke and Mother_Ever_Smoke
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Ever_Smoke + Mother_Ever_Smoke + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       Ancestry_Offspring, data = df_output_clean %>% filter(Problematic_Row == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Ever_Smoke + Mother_Ever_Smoke + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       Ancestry_Offspring, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Ever_Smoke + Mother_Ever_Smoke + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       Ancestry_Offspring, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Mother_Smoking_Discrepancy == 0 & Mother_Smoking_Discrepancy == 0))
)
summary(
  lm(Autosomal_SNV_DNMs ~ Father_Ever_Smoke + Mother_Ever_Smoke + 
       Father_Age_At_Birth + Mother_Age_At_Birth + Cohort +
       Ancestry_Offspring, data = df_output_clean %>% filter(DNM_Outlier == 0 & Parental_Age_Issue == 0 & Survey_Sex_Discrepancy == 0))
)
# Father_Ever_Smoke is significant in this model. (As well as PC1 more generally in many of the models.)

###################################################################################################
# Output data
###################################################################################################
#write.table(df_output_clean, file = "output/Smoking_Exposure_Table_20241213.tsv", quote = FALSE, row.names = FALSE, sep = '\t')
write.table(df_output_clean, file = "output/Smoking_Exposure_Table_20241217.tsv", quote = FALSE, row.names = FALSE, sep = '\t')

###################################################################################################
# Basic statistics (again)
###################################################################################################

df_output_unclean_reach = df_output_unclean %>%
  filter(Cohort == "REACH") %>%
  select(Family_ID, Sample_ID, Father_ID, Mother_ID, Past_Smoke_Father, Currently_Smoke_Father, Past_Smoke_Mother, Currently_Smoke_Mother) %>%
  mutate(Father_Has_Data = ifelse(!is.na(Past_Smoke_Father) & !is.na(Currently_Smoke_Father), 1, 0)) %>%
  mutate(Mother_Has_Data = ifelse(!is.na(Past_Smoke_Mother) & !is.na(Currently_Smoke_Mother), 1, 0))
reach_mothers_with_data = df_output_unclean_reach %>%
  filter(Mother_Has_Data == 1) %>%
  filter(!is.na(Mother_ID)) %>%
  distinct(Mother_ID)
reach_fathers_with_data = df_output_unclean_reach %>%
  filter(Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID)) %>%
  distinct(Father_ID)
reach_parents_with_data = df_output_unclean_reach %>%
  filter(Mother_Has_Data == 1 & Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID) & !is.na(Mother_ID)) %>%
  distinct(Family_ID)

df_output_unclean_ssc_sa = df_output_unclean %>%
  filter(in_ssc_substance_abuse_survey == 1) %>%
  select(Family_ID, in_ssc_substance_abuse_survey, in_ssc_research_match_survey, Sample_ID, Father_ID, Mother_ID, Past_Smoke_Father, Currently_Smoke_Father,
         past_smoke_cig_father_substance_abuse_survey, currently_smoke_cig_father_substance_abuse_survey,
         past_smoke_cig_mother_substance_abuse_survey, currently_smoke_cig_mother_substance_abuse_survey) %>%
  mutate(Father_Has_Data = ifelse(!is.na(past_smoke_cig_father_substance_abuse_survey) & !is.na(currently_smoke_cig_father_substance_abuse_survey), 1, 0)) %>%
  mutate(Mother_Has_Data = ifelse(!is.na(past_smoke_cig_mother_substance_abuse_survey) & !is.na(currently_smoke_cig_mother_substance_abuse_survey), 1, 0))
ssc_sa_mothers_with_data = df_output_unclean_ssc_sa %>%
  filter(Mother_Has_Data == 1) %>%
  filter(!is.na(Mother_ID)) %>%
  distinct(Mother_ID)
ssc_sa_fathers_with_data = df_output_unclean_ssc_sa %>%
  filter(Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID)) %>%
  distinct(Father_ID)
ssc_sa_parents_with_data = df_output_unclean_ssc_sa %>%
  filter(Mother_Has_Data == 1 & Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID) & !is.na(Mother_ID)) %>%
  distinct(Family_ID)


df_output_unclean_ssc_rm = df_output_unclean %>%
  filter(in_ssc_research_match_survey == 1) %>%
  select(Family_ID, in_ssc_substance_abuse_survey, in_ssc_research_match_survey, Sample_ID, Father_ID, Mother_ID, Past_Smoke_Father, Currently_Smoke_Father,
         past_smoke_cig_father_research_match, currently_smoke_cig_father_research_match,
         past_smoke_cig_mother_research_match, currently_smoke_cig_mother_research_match) %>%
  mutate(Father_Has_Data = ifelse(!is.na(past_smoke_cig_father_research_match) & !is.na(currently_smoke_cig_father_research_match), 1, 0)) %>%
  mutate(Mother_Has_Data = ifelse(!is.na(past_smoke_cig_mother_research_match) & !is.na(currently_smoke_cig_mother_research_match), 1, 0))
ssc_rm_mothers_with_data = df_output_unclean_ssc_rm %>%
  filter(Mother_Has_Data == 1) %>%
  filter(!is.na(Mother_ID)) %>%
  distinct(Mother_ID)
ssc_rm_fathers_with_data = df_output_unclean_ssc_rm %>%
  filter(Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID)) %>%
  distinct(Father_ID)
ssc_rm_parents_with_data = df_output_unclean_ssc_rm %>%
  filter(Mother_Has_Data == 1 & Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID) & !is.na(Mother_ID)) %>%
  distinct(Family_ID)


df_output_unclean_spark_1 = df_output_unclean %>%
  filter(Survey_ID == "RM0053Sebat") %>%
  select(Family_ID, Sample_ID, Father_ID, Mother_ID, Past_Smoke_Father, Currently_Smoke_Father, Past_Smoke_Mother, Currently_Smoke_Mother) %>%
  mutate(Father_Has_Data = ifelse(!is.na(Past_Smoke_Father) & !is.na(Currently_Smoke_Father), 1, 0)) %>%
  mutate(Mother_Has_Data = ifelse(!is.na(Past_Smoke_Mother) & !is.na(Currently_Smoke_Mother), 1, 0))
spark_mothers_with_data_1 = df_output_unclean_spark_1 %>%
  filter(Mother_Has_Data == 1) %>%
  filter(!is.na(Mother_ID)) %>%
  distinct(Mother_ID)
spark_fathers_with_data_1 = df_output_unclean_spark_1 %>%
  filter(Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID)) %>%
  distinct(Father_ID)
spark_parents_with_data_1 = df_output_unclean_spark_1 %>%
  filter(Mother_Has_Data == 1 & Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID) & !is.na(Mother_ID)) %>%
  distinct(Family_ID)

df_output_unclean_spark_2 = df_output_unclean %>%
  filter(Survey_ID == "RM0053Sebat_2021") %>%
  select(Family_ID, Sample_ID, Father_ID, Mother_ID, Past_Smoke_Father, Currently_Smoke_Father, Past_Smoke_Mother, Currently_Smoke_Mother) %>%
  mutate(Father_Has_Data = ifelse(!is.na(Past_Smoke_Father) & !is.na(Currently_Smoke_Father), 1, 0)) %>%
  mutate(Mother_Has_Data = ifelse(!is.na(Past_Smoke_Mother) & !is.na(Currently_Smoke_Mother), 1, 0))
spark_mothers_with_data_2 = df_output_unclean_spark_2 %>%
  filter(Mother_Has_Data == 1) %>%
  filter(!is.na(Mother_ID)) %>%
  distinct(Mother_ID)
spark_fathers_with_data_2 = df_output_unclean_spark_2 %>%
  filter(Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID)) %>%
  distinct(Father_ID)
spark_parents_with_data_2 = df_output_unclean_spark_2 %>%
  filter(Mother_Has_Data == 1 & Father_Has_Data == 1) %>%
  filter(!is.na(Father_ID) & !is.na(Mother_ID)) %>%
  distinct(Family_ID)

