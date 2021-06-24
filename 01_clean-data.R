############################################################
#                                                          #
#                        Clean data                        #
#                                                          #
############################################################

# Please note: DHS data are freely available for projects registered on the
# DHS Program system, but cannot be distributed by a third-party.
# DHS Program website: https://dhsprogram.com

#####################
#   Load packages   #
#####################
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(foreign)

###################
#   Import data   #
###################
biomarker <- read.spss('ZA_2016_DHS/ZAPR71SV/ZAPR71FL.SAV',
                       to.data.frame = TRUE)

women <- read.spss('ZA_2016_DHS/ZAAH71SV/ZAAHW71FL.SAV',
                   to.data.frame = TRUE)

men <- read.spss('ZA_2016_DHS/ZAAH71SV/ZAAHM71FL.SAV',
                 to.data.frame = TRUE)

############################
#   Clean biomarker data   #
############################
# Select and rename columns of required data
biomarker_reduced <- biomarker %>%
    # Select columns
    select(HV001, # Cluster number
           HV002, # Household number
           HVIDX, # Respondent line number
           SH228A, # Second reading systolic (women)
           SH228B, # Second reading diastolic (women)
           SH232A, # Third reading systolic (women)
           SH232B, # Third reading diastolic (women)
           SH328A, # Second reading systolic (men)
           SH328B, # Second reading diastolic (men)
           SH332A, # Third reading systolic (men)
           SH332B, # Third reading diastolic (men)
           SHWHBA1C, # HBA1c (women). Note: 3 decimals implicit
           SHMHBA1C, # HBA1c (men). Note: 3 decimals implicit
           starts_with('SH277'), # Female medication check-list
           starts_with('SH377')) %>%  # Male medication check-list
    # Rename variables
    ## SBP/DBP = systolic/diastolic blood pressure,
    rename(V001 = HV001,
           V002 = HV002,
           V003 = HVIDX,
           SBP_women_2 = SH228A,
           DBP_women_2 = SH228B,
           SBP_women_3 = SH232A,
           DBP_women_3 = SH232B,
           SBP_men_2 = SH328A,
           DBP_men_2 = SH328B,
           SBP_men_3 = SH332A,
           DBP_men_3 = SH332B,
           HBA1C_women = SHWHBA1C,
           HBA1C_men = SHMHBA1C)

#-- Clean hypertension data --#
biomarker_BP <- biomarker_reduced %>%
    # Select columns
    select(-starts_with('HBA1C'), -starts_with('SH277'), -starts_with('SH377')) %>%
    # Convert all BP columns to numeric format
    # Conversion will convert all non-numeric codes ('Other' and 'Technical problems') to <NA>
    mutate(across(.cols = starts_with('SBP'), ~as.numeric(as.character(.x))),
           across(.cols = starts_with('DBP'), ~as.numeric(as.character(.x)))) %>%
    # Calculate the average BP over the 2nd and 3rd measurements
    rowwise() %>%
    mutate(SBP_women = mean(c_across(cols = c(4, 6)), na.rm = TRUE),
           DBP_women = mean(c_across(cols = c(5, 7)), na.rm = TRUE),
           SBP_men = mean(c_across(cols = c(8, 10)), na.rm = TRUE),
           DBP_men = mean(c_across(cols = c(9, 11)), na.rm = TRUE)) %>%
    ungroup() %>%
    # Remove original columns
    select(-contains('_2'), -contains('_3')) %>%
    # Combine men and women BP columns
    ## Systolic blood pressure
    mutate(SBP = case_when(
        is.nan(SBP_women) ~ SBP_men,
        TRUE ~ SBP_women
    )) %>%
    ## Diastolic blood pressure
    mutate(DBP = case_when(
        is.nan(DBP_women) ~ DBP_men,
        TRUE ~ DBP_women
    )) %>%
    # Remove SBP/DBP 'men' and 'women' columns. They are no longer needed.
    select(-SBP_women, -DBP_women, -SBP_men, -DBP_men) %>%
    # Calculate whether hypertension is present based on SBP and DBP
    mutate(Hypertension_measured = case_when(
        SBP >= 140 | DBP >= 90 ~ 'Yes',
        SBP < 140 | DBP < 90 ~ 'No'
    )) %>%
    mutate(Hypertension_measured = factor(Hypertension_measured)) %>%
    # Get rid of pesky <NaN> introduced into the SBP and DBP
    # columns during the cleaning process
    mutate(SBP = ifelse(is.nan(SBP),
                        yes = NA,
                        no = SBP),
           DBP = ifelse(is.nan(DBP),
                        yes = NA,
                        no = DBP))

#-- Clean HBA1c data --#
biomarker_HBA1c <- biomarker_reduced %>%
    # Select columns
    select(starts_with('V0'), starts_with('HB')) %>%
    # Convert all HBA1c columns to numeric format
    # Conversion will convert non-numeric code ("inconclusive") to <NA>
    mutate(across(starts_with('HBA'), ~ as.numeric(as.character(.x)))) %>%
    # Combine the men and women columns
    mutate(HBA1C = case_when(
        is.na(HBA1C_women) ~ HBA1C_men,
        TRUE ~ HBA1C_women
    )) %>%
    # Remove HBA1c 'men' and 'women' columns. They are no longer needed.
    select(-contains('_')) %>%
    # VERY IMPORTANT STEP
    # Correct HBA1C data to 3 decimals
    mutate(HBA1C = HBA1C / 1000) %>%
    # VERY IMPORTANT STEP
    # Correct HBA1c based on the use of a dried blood spot instead
    # of fresh venous blood (HBA1c from the dried sample - 0.228) / 0.9866
    mutate(HBA1C = (HBA1C - 0.228) / 0.9866,
           HBA1C = round(HBA1C, 3)) %>%
    # Calculate whether diabetes is present based on HBA1c
    mutate(Diabetes_measured = case_when(
        HBA1C >= 6.5 ~ 'Yes',
        HBA1C < 6.5 ~ 'No'
    )) %>%
    mutate(Diabetes_measured = factor(Diabetes_measured))

#-- Clean medication data --#
biomarker_rx <- biomarker_reduced %>%
    # Select columns
    select(starts_with('V0'), starts_with('SH277'), starts_with('SH377')) %>%
    # Rename medication assessment columns
    rename(Rx_assessed_women = SH277,
           Rx_assessed_men = SH377) %>%
    # Convert Rx_assessed_* to character
    mutate(across(.cols = starts_with('Rx_assessed_'), ~ as.character(.x))) %>%
    # Combine Rx_assessed_* columns
    mutate(Rx_assessed = ifelse(is.na(Rx_assessed_women),
                                yes = Rx_assessed_men,
                                no = Rx_assessed_women),
           Rx_assessed = factor(Rx_assessed)) %>%
    # Remove men and womens Rx_assessed columns
    select(-starts_with('Rx_assessed_')) %>%
    # Unite SH277A-L and SH377A-L
    unite(col = 'Rx',
          matches('77[A-L]')) %>%
    # Check for anti-hypertensives medications
    # WHO ATC codes: C02, C03, C07, C08, C09
    mutate(Rx_hypertension = str_detect(Rx,
                                        pattern = c('C02|C03|C07|C08|C09'))) %>%
    # Check for anti-diabetic medications
    # WHO ATC code: A10
    mutate(Rx_diabetes = str_detect(Rx,
                                    pattern = 'A10')) %>%
    # Remove Rx column
    select(-Rx) %>%
    # Fill in <NA> in Rx_* when Rx_assessed is <NA>
    mutate(Rx_hypertension = ifelse(is.na(Rx_assessed),
                                    yes = NA,
                                    no = Rx_hypertension),
           Rx_diabetes = ifelse(is.na(Rx_assessed),
                                yes = NA,
                                no = Rx_diabetes))

# Join biomarker_BP, biomarker_HBA1c, and biomarker_rx
biomarker_clean <- biomarker_BP %>%
    left_join(biomarker_HBA1c) %>%
    left_join(biomarker_rx)

########################
#   Clean men's data   #
########################
# Select and rename columns of required data
men_reduced <- men %>%
    # Select columns
    select(MV001, # Cluster number
           MV002, # Household number
           MV003, # Respondent line number
           MV021, # Primary sampling unit (PSU)
           MV022, # Sampling strata
           SMWEIGHT, # Sample weight Note: 6 decimals implicit
           MV012, # Age in years
           SM1108A, # Diagnosed with high blood pressure
           SM1111, # If diagnosed, then did you receive medication for hypertension
           SM1108F, # Diagnosed with diabetes
           SM1121) %>% # If diagnosed, then did you receive medication for diabetes
    # Rename variables
    rename(V001 = MV001,
           V002 = MV002,
           V003 = MV003,
           V021 = MV021,
           V022 = MV022,
           SWEIGHT = SMWEIGHT,
           Age_years = MV012,
           Hypertension_question =  SM1108A,
           Hypertension_treatment_question = SM1111,
           Diabetes_question = SM1108F,
           Diabetes_treatment_question = SM1121) %>%
    # Add sex column
    mutate(Sex = 'Male')

# Clean the reduced data
men_clean <- men_reduced %>%
    # VERY IMPORTANT STEP
    # Correct SWEIGHT to 6 decimals
    mutate(SWEIGHT = SWEIGHT / 1000000) %>%
    # Calculate age categories
    mutate(Age_categories = case_when(
        Age_years >= 15 & Age_years < 25 ~ '15-24',
        Age_years >= 25 & Age_years < 35 ~ '25-34',
        Age_years >= 35 & Age_years < 45 ~ '35-44',
        Age_years >= 45 & Age_years < 55 ~ '45-54',
        Age_years >= 55 & Age_years < 65 ~ '55-64',
        Age_years >= 65 ~ '65+'
    )) %>%
    # Hypertension_question: Recode "Don't know" values to "No",
    mutate(Hypertension_question = as.character(Hypertension_question)) %>%
    mutate(Hypertension_question = case_when(
        Hypertension_question == 'No' |
            Hypertension_question == "Don't know" ~ 'No',
        Hypertension_question == 'Yes' ~ 'Yes'
    )) %>%
    mutate(Hypertension_question = factor(Hypertension_question)) %>%
    # Diabetes_question: Recode "Don't know" values to "No",
    mutate(Diabetes_question = as.character(Diabetes_question)) %>%
    mutate(Diabetes_question = case_when(
        Diabetes_question == 'No' |
            Diabetes_question == "Don't know" ~ 'No',
        Diabetes_question == 'Yes' ~ 'Yes'
    )) %>%
    mutate(Diabetes_question = factor(Diabetes_question))

##########################
#   Clean women's data   #
##########################
# Select and rename columns of required data
women_reduced <- women %>%
    # Select columns
    select(V001, # Cluster number
           V002, # Household number
           V003, # Respondent line number
           V021, # Primary sampling unit (PSU)
           V022, # Sampling strata
           SWEIGHT, # Sample weight Note: 6 decimals implicit
           V012, # Age in years
           S1413A, # Diagnosed with high blood pressure
           S1416, # If diagnosed, then did you receive medication for hypertension
           S1413F, # Diagnosed with diabetes
           S1426) %>% # If diagnosed, then did you receive medication for diabetes
    # Rename variables
    rename(Age_years = V012,
           Hypertension_question =  S1413A,
           Hypertension_treatment_question = S1416,
           Diabetes_question = S1413F,
           Diabetes_treatment_question = S1426) %>%
    # Add sex column
    mutate(Sex = 'Female')

# Clean the reduced data
women_clean <- women_reduced %>%
    # VERY IMPORTANT STEP
    # Correct SWEIGHT to 6 decimals
    mutate(SWEIGHT = SWEIGHT / 1000000) %>%
    # Calculate age categories
    mutate(Age_categories = case_when(
        Age_years >= 15 & Age_years < 25 ~ '15-24',
        Age_years >= 25 & Age_years < 35 ~ '25-34',
        Age_years >= 35 & Age_years < 45 ~ '35-44',
        Age_years >= 45 & Age_years < 55 ~ '45-54',
        Age_years >= 55 & Age_years < 65 ~ '55-64',
        Age_years >= 65 ~ '65+'
    )) %>%
    # Hypertension_question: Recode "Don't know" values to "No",
    mutate(Hypertension_question = as.character(Hypertension_question)) %>%
    mutate(Hypertension_question = case_when(
        Hypertension_question == 'No' |
            Hypertension_question == "Don't know" ~ 'No',
        Hypertension_question == 'Yes' ~ 'Yes'
    )) %>%
    mutate(Hypertension_question = factor(Hypertension_question)) %>%
    # Diabetes_question: Recode "Don't know" values to "No",
    mutate(Diabetes_question = as.character(Diabetes_question)) %>%
    mutate(Diabetes_question = case_when(
        Diabetes_question == 'No' |
            Diabetes_question == "Don't know" ~ 'No',
        Diabetes_question == 'Yes' ~ 'Yes'
    )) %>%
    mutate(Diabetes_question = factor(Diabetes_question))

##############################################
#   Vertical join: women_clean + men_clean   #
##############################################
sex_clean <- bind_rows(women_clean, men_clean)

####################################################
#   Horizontal join: biomarker_clean + sex_clean   #
####################################################
analysis_set <- sex_clean %>%
    left_join(biomarker_clean) %>%
    # Select and order columns
    select(V021, V022, SWEIGHT,
           Sex, Age_years, Age_categories, Rx_assessed,
           Hypertension_question, Hypertension_treatment_question,
           Rx_hypertension, Hypertension_measured, SBP, DBP,
           Diabetes_question, Diabetes_treatment_question,
           Rx_diabetes, Diabetes_measured, HBA1C)

#######################
#   Save clean data   #
#######################
write_csv(x = analysis_set,
          file = 'data-clean/analysis-set.csv')

write_rds(x = analysis_set,
          file = 'data-clean/analysis-set.rds')
