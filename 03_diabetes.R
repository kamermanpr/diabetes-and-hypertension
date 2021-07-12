############################################################
#                                                          #
#                         Diabetes                         #
#                                                          #
############################################################

#####################
#   Load packages   #
#####################
library(survey)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(knitr)
library(ggplot2)
library(ggthemes)
library(patchwork)

####################
#   ggplot theme   #
####################
theme_set(new = theme_minimal(base_size = 16) +
              theme(panel.grid = element_blank(),
                    axis.text = element_text(colour = '#000000'),
                    axis.line = element_line(colour = '#000000',
                                             size = 0.4),
                    axis.ticks = element_line(colour = '#000000',
                                              size = 0.4)))

###################
#   Import data   #
###################
data <- readRDS('data-clean/analysis-set.rds')

############################
#   Create design object   #
############################
design_obj <- svydesign(ids = ~V021, # Primary sampling units
                        strata = ~V023, # Strata
                        weights = ~SWEIGHT, # Design weights
                        data = data)

############################
#   Exploratory analysis   #
############################
#-- Answers to the questionnaire --#
data %>%
    group_by(Diabetes_question) %>%
    summarise(count = n()) %>%
    mutate(total = sum(count)) %>%
    kable()

data %>%
    group_by(Sex, Diabetes_question) %>%
    summarise(count = n()) %>%
    mutate(total = sum(count)) %>%
    ungroup() %>%
    mutate(overll_n = sum(count)) %>%
    kable()

#-- No stratification --#
# HbA1c
## Calculate mean
diabetes <- svymean(~HBA1C,
                    design = design_obj,
                    na.rm = TRUE)
round(diabetes, 1)

## Calculate 95% CI for the mean
round(confint(diabetes), 1)

## Calculate quantiles
round(svyquantile(~HBA1C,
                  design = design_obj,
                  na.rm = TRUE,
                  quantiles = c(0, 0.25, 0.50, 0.75, 1),
                  keep.var = FALSE)[, 1:5], 1)

## Calculate crude number of individuals
data %>%
    filter(!is.na(HBA1C)) %>%
    select(HBA1C) %>%
    summarise(n = n()) %>%
    kable()

## Plot the data
png(filename = 'figures/10_HBA1C_full-cohort.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(HBA1C ~ 1,
           design = design_obj,
           all.outliers = FALSE,
           ylab = 'Percent haemoglobin (%)',
           main = 'HbA1c',
           ylim = c(0, 25),
           xaxt = 'n',
           las = 2,
           cex.main = 2,
           cex.axis = 1.2,
           cex.lab = 1.5)

axis(side = 1,
     at = 1,
     tick = FALSE,
     label = 'Full cohort',
     cex.axis = 1.2)

abline(h = 6.5,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

#-- Stratified by sex --#
# HbA1c
## Calculate mean and 95% CI
diabetes_sex <- svyby(~HBA1C,
                      by = ~Sex,
                      FUN = svymean,
                      design = design_obj,
                      vartype = 'ci',
                      na.rm = TRUE)

round(diabetes_sex[, 2:4], 1)

## Calculate quantiles
round(svyby(~HBA1C,
            by = ~Sex,
            FUN = svyquantile,
            design = design_obj,
            na.rm = TRUE,
            quantiles = c(0, 0.25, 0.50, 0.75, 1),
            keep.var = FALSE)[, 2:6], 1)

## Calculate crude number of individuals
data %>%
    select(HBA1C, Sex) %>%
    filter(complete.cases(.)) %>%
    group_by(Sex) %>%
    summarise(n = n()) %>%
    kable()

## T-test
svyttest(HBA1C ~ Sex, design = design_obj)

## Plot the data
png(filename = 'figures/11_HBA1C_sex.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(HBA1C ~ Sex,
           design = design_obj,
           all.outliers = FALSE,
           ylab = 'Percent haemoglobin',
           main = 'HbA1c (by sex)',
           ylim = c(0, 25),
           xaxt = 'n',
           las = 2,
           cex.main = 2,
           cex.axis = 1.2,
           cex.lab = 1.5)

axis(1,
     at = c(1, 2),
     labels = c('Female', 'Male'),
     cex.axis = 1.2)

abline(h = 6.5,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

#-- Stratified by age group --#
# HbA1c
## Calculate mean and 95% CI
diabetes_age <- svyby(~HBA1C,
                      by = ~Age_categories,
                      FUN = svymean,
                      design = design_obj,
                      vartype = 'ci',
                      na.rm = TRUE)

round(diabetes_age[, 2:4], 1)

## Calculate quantiles
round(svyby(~HBA1C,
            by = ~Age_categories,
            FUN = svyquantile,
            design = design_obj,
            na.rm = TRUE,
            quantiles = c(0, 0.25, 0.50, 0.75, 1),
            keep.var = FALSE)[, 2:6], 1)

## Calculate crude number of individuals
data %>%
    select(HBA1C, Age_categories) %>%
    filter(complete.cases(.)) %>%
    group_by(Age_categories) %>%
    summarise(n = n()) %>%
    kable()

## OLS for age in years
summary(svyglm(HBA1C ~ Age_years,
               design = design_obj))

## ANOVA for age catgories
summary(svyglm(HBA1C ~ Age_categories,
               design = design_obj))

## Plot the data
png(filename = 'figures/12_HBA1C_age-category.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(HBA1C ~ Age_categories,
           design = design_obj,
           all.outliers = FALSE,
           ylab = 'Percent haemoglobin (%)',
           xlab = 'Years',
           main = 'HbA1c (by age group)',
           ylim = c(0, 25),
           xaxt = 'n',
           las = 2,
           cex.main = 2,
           cex.axis = 1.2,
           cex.lab = 1.5)

axis(1,
     at = c(1, 2, 3, 4, 5, 6),
     labels = c('15-24', '25-34', '35-44', '45-54', '55-64', '65+'),
     cex.axis = 1.2)

abline(h = 6.5,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

######################################
#   Diabetes: measured vs reported   #
######################################

#-- No stratification --#
# Question
## Crude sample size
data %>%
    filter(!is.na(Diabetes_question)) %>%
    group_by(Diabetes_question) %>%
    summarise(n = n()) %>%
    mutate(total = sum(n))

## Calculate proportion
DB_question <- svymean(~Diabetes_question,
                       design = design_obj,
                       na.rm = TRUE)

## Convert to svystat object to dataframe
DB_question_df <- DB_question %>%
    as.data.frame(.)

## Calculate CI from svystat to dataframe
DB_question_ci <- confint(DB_question) %>%
    as.data.frame(.)

## Column bind, clean-up, and convert to a percentage
DB_question_perc <- bind_cols(DB_question_df, DB_question_ci) %>%
    rownames_to_column(var = 'outcome') %>%
    mutate(across(where(is.numeric), ~ 100 * round(.x, 3)))

DB_question_perc$outcome <- c('No', 'Yes')
DB_question_perc$group <- 'Question'

# Measured
## Crude sample size
data %>%
    filter(!is.na(Diabetes_measured)) %>%
    group_by(Diabetes_measured) %>%
    summarise(n = n()) %>%
    mutate(total = sum(n)) %>%
    kable()

## Calculate proportion
DB_measured <- svymean(~Diabetes_measured,
                       design = design_obj,
                       na.rm = TRUE)

## Convert to svystat object to dataframe
DB_measured_df <- DB_measured %>%
    as.data.frame(.)

## Calculate CI from svystat to dataframe
DB_measured_ci <- confint(DB_measured) %>%
    as.data.frame(.)

## Column bind, clean-up, and convert to a percentage
DB_measured_perc <- bind_cols(DB_measured_df, DB_measured_ci) %>%
    rownames_to_column(var = 'outcome') %>%
    mutate(across(where(is.numeric), ~ 100 * round(.x, 3)))

DB_measured_perc$outcome <- c('No', 'Yes')
DB_measured_perc$group <- 'Measured'

DB_combined <- bind_rows(DB_question_perc, DB_measured_perc) %>%
    filter(outcome == 'Yes')

DB_combined %>%
    kable()

# Percent change
DB_combined %>%
    select(group, mean) %>%
    mutate(mean = mean / 100) %>%
    pivot_wider(names_from = group,
                values_from = mean) %>%
    mutate(percent_points = 100 * (Measured - Question),
           percent_change = 100 * ((Measured - Question) / Question)) %>%
    select(-Question, -Measured) %>%
    kable(digits = 1)

# Plot data
plot_DB <- ggplot(data = DB_combined) +
    aes(x = group,
        y = mean,
        colour = group,
        fill = group,
        ymin = `2.5 %`,
        ymax = `97.5 %`) +
    geom_col(alpha = 0.5) +
    geom_errorbar(width = 0.3,
                  size = 1) +
    labs(y = 'Percent with diabetes (%)',
         x = 'Method of diagnosis') +
    scale_colour_tableau() +
    scale_fill_tableau() +
    scale_y_continuous(limits = c(0, 15),
                       expand = c(0, 0)) +
    scale_x_discrete(label = c('HbA1c\nmeasured',
                               'Recalled being\ndiagnosed')) +
    theme(legend.position = 'none')

ggsave(filename = 'figures/13_diabetes_full-cohort.png',
       plot = plot_DB,
       width = 6,
       height = 5.5)

#-- Stratified by sex --#
# Question
## Crude sample size
data %>%
    filter(!is.na(Diabetes_question)) %>%
    filter(!is.na(Sex)) %>%
    group_by(Sex, Diabetes_question) %>%
    summarise(n = n()) %>%
    group_by(Sex) %>%
    mutate(total_by_sex = sum(n)) %>%
    ungroup() %>%
    mutate(grand_total = sum(n)) %>%
    kable()

## Calculate proportion and convert to a percentage
DB_question_sex <- svyby(~Diabetes_question,
                         by = ~Sex,
                         FUN = svymean,
                         design = design_obj,
                         vartype = 'ci',
                         na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Sex, ends_with('Yes')) %>%
    set_names(nm = c('sex', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Question',
           diabetes = 'Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(DB_question_sex) <- NULL

# Measured
## Crude sample size
data %>%
    filter(!is.na(Diabetes_measured)) %>%
    filter(!is.na(Sex)) %>%
    group_by(Sex, Diabetes_measured) %>%
    summarise(n = n()) %>%
    group_by(Sex) %>%
    mutate(total_by_sex = sum(n)) %>%
    ungroup() %>%
    mutate(grand_total = sum(n)) %>%
    kable()

## Calculate proportion and convert to a percentage
DB_measured_sex <- svyby(~Diabetes_measured,
                         by = ~Sex,
                         FUN = svymean,
                         design = design_obj,
                         vartype = 'ci',
                         na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Sex, ends_with('Yes')) %>%
    set_names(nm = c('sex', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Measured',
           diabetes = 'Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(DB_measured_sex) <- NULL

DB_combined_sex <- bind_rows(DB_question_sex, DB_measured_sex)

DB_combined_sex %>%
    kable(digits = 1)

# Percent change
DB_combined_sex %>%
    select(sex, estimate, group) %>%
    mutate(estimate = estimate / 100) %>%
    group_by(sex) %>%
    nest() %>%
    mutate(percent_change = map(.x = data,
                                ~ .x %>%
                                    pivot_wider(names_from = group,
                                                values_from = estimate) %>%
                                    mutate(percent_points = 100 * (Measured - Question),
                                           percent_change = 100 * ((Measured - Question) / Question)))) %>%
    unnest(c(sex, percent_change)) %>%
    select(-data, -Question, -Measured) %>%
    kable(digits = 1)

# Plot data
plot_DB_sex <- ggplot(data = DB_combined_sex) +
    aes(x = sex,
        y = estimate,
        colour = group,
        fill = group,
        ymin = ci_lower,
        ymax = ci_upper) +
    geom_col(position = position_dodge(0.9),
             alpha = 0.5) +
    geom_errorbar(position = position_dodge(0.9),
                  width = 0.3,
                  size = 1,
                  show.legend = FALSE) +
    labs(y = 'Percent with diabetes (%)',
         x = 'Sex') +
    scale_colour_tableau(name = 'Method of diagnosis',
                         label = c('HbA1c measured',
                                   'Recalled being diagnosed')) +
    scale_fill_tableau(name = 'Method of diagnosis',
                       label = c('HbA1c measured',
                                 'Recalled being diagnosed')) +
    scale_y_continuous(limits = c(0, 15),
                       expand = c(0, 0)) +
    scale_x_discrete() +
    theme(legend.position = c(0.78, 0.93))

ggsave(filename = 'figures/14_diabetes_sex.png',
       plot = plot_DB_sex,
       width = 7,
       height = 6)

#-- Stratified by age --#
# Question
## Crude sample size
data %>%
    filter(!is.na(Diabetes_question)) %>%
    filter(!is.na(Age_categories)) %>%
    group_by(Age_categories, Diabetes_question) %>%
    summarise(n = n()) %>%
    group_by(Age_categories) %>%
    mutate(total_by_age = sum(n)) %>%
    ungroup() %>%
    mutate(grand_total = sum(n)) %>%
    kable()

## Calculate proportion and convert to a percentage
DB_question_age <- svyby(~Diabetes_question,
                         by = ~Age_categories,
                         FUN = svymean,
                         design = design_obj,
                         vartype = 'ci',
                         na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Age_categories, ends_with('Yes')) %>%
    set_names(nm = c('age_categories', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Question',
           diabetes = 'Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(DB_question_age) <- NULL

# Measured
## Crude sample size
data %>%
    filter(!is.na(Diabetes_measured)) %>%
    filter(!is.na(Age_categories)) %>%
    group_by(Age_categories, Diabetes_measured) %>%
    summarise(n = n()) %>%
    group_by(Age_categories) %>%
    mutate(total_by_age = sum(n)) %>%
    ungroup() %>%
    mutate(grand_total = sum(n)) %>%
    kable()

## Calculate proportion and convert to a percentage
DB_measured_age <- svyby(~Diabetes_measured,
                         by = ~Age_categories,
                         FUN = svymean,
                         design = design_obj,
                         vartype = 'ci',
                         na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Age_categories, ends_with('Yes')) %>%
    set_names(nm = c('age_categories', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Measured',
           diabetes = 'Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(DB_measured_age) <- NULL

DB_combined_age <- bind_rows(DB_question_age, DB_measured_age)

DB_combined_age %>%
    kable(digits = 1)

# Percent change
DB_combined_age %>%
    select(age_categories, estimate, group) %>%
    mutate(estimate = estimate / 100) %>%
    group_by(age_categories) %>%
    nest() %>%
    mutate(percent_change = map(.x = data,
                                ~ .x %>%
                                    pivot_wider(names_from = group,
                                                values_from = estimate) %>%
                                    mutate(percent_point = 100 * (Measured - Question),
                                           percent_change = 100 * ((Measured - Question) / Question)))) %>%
    unnest(c(age_categories, percent_change)) %>%
    select(-data, -Question, -Measured) %>%
    kable(digits = 1)

# Plot data
plot_DB_age <- ggplot(data = DB_combined_age) +
    aes(x = age_categories,
        y = estimate,
        colour = group,
        fill = group,
        ymin = ci_lower,
        ymax = ci_upper) +
    geom_col(position = position_dodge(0.9),
             alpha = 0.5) +
    geom_errorbar(position = position_dodge(0.9),
                  width = 0.3,
                  size = 1,
                  show.legend = FALSE) +
    labs(y = 'Percent with diabetes (%)',
         x = 'Age group (Years)') +
    scale_colour_tableau(name = 'Method of diagnosis',
                         label = c('HbA1c measured',
                                   'Recalled being diagnosed')) +
    scale_fill_tableau(name = 'Method of diagnosis',
                       label = c('HbA1c measured',
                                 'Recalled being diagnosed')) +
    scale_y_continuous(limits = c(0, 50),
                       expand = c(0, 0)) +
    scale_x_discrete() +
    theme(legend.position = c(0.2, 0.93))

ggsave(filename = 'figures/15_diabetes_age.png',
       plot = plot_DB_age,
       width = 8,
       height = 6)

#-- Who has diabetes (measured) vs who recalled receiving a diagnosis --#
# Crude numbers
data %>%
    select(Diabetes_question, Diabetes_measured) %>%
    filter(complete.cases(.)) %>%
    group_by(Diabetes_measured, Diabetes_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Grand_total = sum(Count))

# Analysis
measured_vs_question_yes <- svyby(~Diabetes_question,
                                  by = ~Diabetes_measured,
                                  FUN = svymean,
                                  design = design_obj,
                                  vartype = 'ci',
                                  na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Diabetes_measured, ends_with('Yes')) %>%
    set_names(nm = c('Diabetes_measured', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Question: Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(measured_vs_question_yes) <- NULL

measured_vs_question_no <- svyby(~Diabetes_question,
                                 by = ~Diabetes_measured,
                                 FUN = svymean,
                                 design = design_obj,
                                 vartype = 'ci',
                                 na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Diabetes_measured, ends_with('No')) %>%
    set_names(nm = c('Diabetes_measured', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Question: No') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(measured_vs_question_no) <- NULL

rbind(measured_vs_question_no, measured_vs_question_yes) %>%
    kable()

# X-tabulate data taking design into account
tab_measured_vs_question <- svytable(~Diabetes_question +
                                         Diabetes_measured,
                                     design = design_obj)

# Check proportions match those generated in previous steps (Analysis)
prop.table(tab_measured_vs_question, margin = 2)

#-- Sample size for Rx_medicines_seen and Diabetes_treatment_question --#
# Rx_medicines_seen
data %>%
    select(Rx_medicines_seen) %>%
    filter(complete.cases(.)) %>%
    group_by(Rx_medicines_seen) %>%
    summarise(Count = n())

# Diabetes_treatment_question
data %>%
    select(Diabetes_treatment_question) %>%
    filter(complete.cases(.)) %>%
    group_by(Diabetes_treatment_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count))

#-- Sample size of participants: Rx_medicines_seen with Rx_diabetes data --#
data %>%
    select(Rx_medicines_seen, Rx_diabetes) %>%
    filter(complete.cases(.)) %>%
    group_by(Rx_medicines_seen, Rx_diabetes) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    mutate(Percent = 100 * (Count / Total))

#-- Sample size of participants: Rx_medicines_seen with Diabetes_treatment_question data --#
data %>%
    select(Rx_medicines_seen, Diabetes_treatment_question) %>%
    filter(complete.cases(.)) %>%
    group_by(Rx_medicines_seen, Diabetes_treatment_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    mutate(Percent = 100 * (Count / Total))

#-- Rx for diabetes (prescription data) in those with/without diabetes (measured and question) --#
data %>%
    select(Rx_medicines_seen,
           Diabetes_question,
           Diabetes_measured,
           Rx_diabetes) %>%
    filter(complete.cases(.)) %>%
    group_by(Diabetes_question, Diabetes_measured, Rx_diabetes) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Grand_total = sum(Count))

tab_rx <- svytable(~Diabetes_measured + Diabetes_question +
                       Rx_diabetes + Rx_medicines_seen,
                   design = design_obj)

ftable(tab_rx) %>%
    as.data.frame() %>%
    group_by(Rx_medicines_seen, Diabetes_measured,
             Diabetes_question) %>%
    mutate(Sub_total = sum(Freq)) %>%
    arrange(Sub_total) %>%
    mutate(Percent_sub_total = 100 * (Freq/Sub_total)) %>%
    kable()

svyby(~ Rx_diabetes,
      by = ~ Diabetes_measured + Diabetes_question,
      design = design_obj,
      FUN = svymean,
      na.rm = TRUE,
      vartype = 'ci') %>%
    select(1:2, ends_with('TRUE')) %>%
    mutate(across(.cols = ends_with('TRUE'), ~ round(100 * .x, 1)))

#-- Qx for diabetes (question data) in those with/without diabetes (measured and question) --#
data %>%
    select(Diabetes_question,
           Diabetes_measured,
           Diabetes_treatment_question) %>%
    filter(complete.cases(.)) %>%
    group_by(Diabetes_question, Diabetes_measured,
             Diabetes_treatment_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Grand_total = sum(Count))

tab_rx_question <- svytable(~Diabetes_question + Diabetes_measured +
                                Diabetes_treatment_question,
                            design = design_obj)

ftable(tab_rx_question) %>%
    as.data.frame() %>%
    group_by(Diabetes_measured, Diabetes_question) %>%
    mutate(Sub_total = sum(Freq)) %>%
    arrange(Sub_total) %>%
    mutate(Percent_sub_total = 100 * (Freq/Sub_total)) %>%
    kable()

#-- Rx_diabetes vs diabetes_treatment_question --#
data %>%
    select(Rx_medicines_seen,
           Diabetes_treatment_question,
           Rx_diabetes) %>%
    filter(complete.cases(.)) %>%
    group_by(Rx_medicines_seen, Rx_diabetes, Diabetes_treatment_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Grand_total = sum(Count))

tab_rx_vs_question <- svytable(~Rx_diabetes + Diabetes_treatment_question +
                                   Rx_medicines_seen,
                               design = design_obj)

ftable(tab_rx_vs_question) %>%
    as.data.frame() %>%
    group_by(Rx_diabetes) %>%
    mutate(Sub_total = sum(Freq)) %>%
    arrange(Sub_total) %>%
    mutate(Percent_sub_total = 100 * (Freq/Sub_total)) %>%
    kable()

########################
#   Publication plot   #
########################
# Overall
plot_DB_stack <- ggplot(data = DB_combined) +
    aes(x = group,
        y = mean,
        colour = group,
        fill = group,
        ymin = `2.5 %`,
        ymax = `97.5 %`) +
    geom_col(alpha = 0.5) +
    geom_errorbar(width = 0.3,
                  size = 1,
                  show.legend = FALSE) +
    labs(title = 'Overall',
         y = NULL,
         x = NULL) +
    scale_colour_tableau(name = 'Method of diagnosis',
                         label = c('HbA1c measured',
                                   'Recalled being diagnosed')) +
    scale_fill_tableau(name = 'Method of diagnosis',
                       label = c('HbA1c measured',
                                 'Recalled being diagnosed')) +
    scale_y_continuous(limits = c(0, 40),
                       expand = c(0, 0)) +
    scale_x_discrete(label = c('Measured',
                               'Recall')) +
    theme_minimal(base_size = 24) +
    theme(legend.position = c(0.78, 0.93),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16),
          plot.title = element_text(size = 24),
          panel.grid = element_blank(),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(colour = '#000000',
                                   size = 0.6),
          axis.ticks = element_line(colour = '#000000',
                                    size = 0.6))

# Sex
plot_DB_sex_stack <- ggplot(data = DB_combined_sex) +
    aes(x = sex,
        y = estimate,
        colour = group,
        fill = group,
        ymin = ci_lower,
        ymax = ci_upper) +
    geom_col(position = position_dodge(0.9),
             alpha = 0.5) +
    geom_errorbar(position = position_dodge(0.9),
                  width = 0.3,
                  size = 1,
                  show.legend = FALSE) +
    labs(title = 'Sex',
         y = 'Percent with diabetes (%)',
         x = NULL) +
    scale_colour_tableau() +
    scale_fill_tableau() +
    scale_y_continuous(limits = c(0, 40),
                       expand = c(0, 0)) +
    scale_x_discrete() +
    theme_minimal(base_size = 24) +
    theme(legend.position = 'none',
          plot.title = element_text(size = 24),
          panel.grid = element_blank(),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(colour = '#000000',
                                   size = 0.6),
          axis.ticks = element_line(colour = '#000000',
                                    size = 0.6))

# Age
plot_DB_age_stack <- ggplot(data = DB_combined_age) +
    aes(x = age_categories,
        y = estimate,
        colour = group,
        fill = group,
        ymin = ci_lower,
        ymax = ci_upper) +
    geom_col(position = position_dodge(0.9),
             alpha = 0.5) +
    geom_errorbar(position = position_dodge(0.9),
                  width = 0.3,
                  size = 1,
                  show.legend = FALSE) +
    labs(title = 'Age group (years)',
         y = NULL,
         x = NULL) +
    scale_colour_tableau() +
    scale_fill_tableau() +
    scale_y_continuous(limits = c(0, 40),
                       expand = c(0, 0)) +
    scale_x_discrete() +
    theme_minimal(base_size = 24) +
    theme(legend.position = 'none',
          plot.title = element_text(size = 24),
          panel.grid = element_blank(),
          axis.text = element_text(colour = '#000000'),
          axis.line = element_line(colour = '#000000',
                                   size = 0.6),
          axis.ticks = element_line(colour = '#000000',
                                    size = 0.6))

# Stacking
plot_stacked <- plot_DB_stack +
    plot_DB_sex_stack +
    plot_DB_age_stack +
    plot_layout(ncol = 1)

ggsave(filename = 'figures/figure-3.png',
       plot = plot_stacked,
       height = 16,
       width = 8)

# Diagnosed vs measured
png(filename = 'figures/figure-4_original.png',
    res = 300,
    width = 2400,
    height = 2400)
mosaicplot(t(tab_measured_vs_question),
           main = NULL,
           color = c('grey30', 'grey90'),
           cex.axis = 1.2)
dev.off()
