############################################################
#                                                          #
#                       Hypertension                       #
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
                        strata = ~V022, # Strata
                        weights = ~SWEIGHT, # Design weights
                        data = data)

############################
#   Exploratory analysis   #
############################
#-- Answers to the questionnaire --#
data %>%
    group_by(Hypertension_question) %>%
    summarise(count = n()) %>%
    mutate(total = sum(count)) %>%
    kable()

data %>%
    group_by(Sex, Hypertension_question) %>%
    summarise(count = n()) %>%
    mutate(total = sum(count)) %>%
    ungroup() %>%
    mutate(overll_n = sum(count)) %>%
    kable()

#-- No stratification --#
# Systolic
## Calculate mean
systolic <- svymean(~SBP,
                    design = design_obj,
                    na.rm = TRUE)
round(systolic)

## Calculate 95% CI for the mean
round(confint(systolic))

## Calculate quantiles
round(svyquantile(~SBP,
                  design = design_obj,
                  na.rm = TRUE,
                  quantiles = c(0, 0.25, 0.50, 0.75, 1),
                  keep.var = FALSE)[, 1:5])

## Calculate crude number of individuals
data %>%
    filter(!is.na(SBP)) %>%
    select(SBP) %>%
    summarise(n = n()) %>%
    kable()

## Plot the data
png(filename = 'figures/01_systolic_full-cohort.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(SBP ~ 1,
           design = design_obj,
           all.outliers = FALSE,
           ylab = 'Pressure (mm Hg)',
           main = 'Systolic',
           ylim = c(40, 220),
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

abline(h = 140,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

# Diastolic
## Calculate the mean
diastolic <- svymean(~DBP,
                     design = design_obj,
                     na.rm = TRUE)
round(diastolic)

## calculate the 95% CI for the mean
round(confint(diastolic))

## Calculate quantiles
round(svyquantile(~DBP,
                  design = design_obj,
                  na.rm = TRUE,
                  quantiles = c(0, 0.25, 0.50, 0.75, 1),
                  keep.var = FALSE)[, 1:5])

## Calculate crude number of individuals
data %>%
    filter(!is.na(DBP)) %>%
    select(DBP) %>%
    summarise(n = n()) %>%
    kable()

## Plot the data
png(filename = 'figures/02_diastolic_full-cohort.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(DBP ~ 1,
           design = design_obj,
           all.outliers = FALSE,
           ylab = 'Pressure (mm Hg)',
           main = 'Diastolic',
           ylim = c(40, 120),
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

abline(h = 90,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

#-- Stratified by sex --#
# Systolic
## Calculate mean and 95% CI
systolic_sex <- svyby(~SBP,
                      by = ~Sex,
                      FUN = svymean,
                      design = design_obj,
                      vartype = 'ci',
                      na.rm = TRUE)

round(systolic_sex[, 2:4])

## Calculate quantiles
round(svyby(~SBP,
            by = ~Sex,
            FUN = svyquantile,
            design = design_obj,
            na.rm = TRUE,
            quantiles = c(0, 0.25, 0.50, 0.75, 1),
            keep.var = FALSE)[, 2:6])

## Calculate crude number of individuals
data %>%
    select(SBP, Sex) %>%
    filter(complete.cases(.)) %>%
    group_by(Sex) %>%
    summarise(n = n()) %>%
    kable()

## T-test
svyttest(SBP ~ Sex, design = design_obj)

## Plot the data
png(filename = 'figures/03_systolic_sex.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(SBP ~ Sex,
           design = design_obj,
           all.outliers = TRUE,
           ylab = 'Pressure (mm Hg)',
           main = 'Systolic (by sex)',
           ylim = c(40, 220),
           xaxt = 'n',
           las = 2,
           cex.main = 2,
           cex.axis = 1.2,
           cex.lab = 1.5)

axis(1,
     at = c(1, 2),
     labels = c('Female', 'Male'),
     cex.axis = 1.2)

abline(h = 140,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

# Diastolic
## Calculate mean and 95% CI
diastolic_sex <- svyby(~DBP,
                       by = ~Sex,
                       FUN = svymean,
                       design = design_obj,
                       vartype = 'ci',
                       na.rm = TRUE)

round(diastolic_sex[, 2:4])

## Calculate quantiles
round(svyby(~DBP,
            by = ~Sex,
            FUN = svyquantile,
            design = design_obj,
            na.rm = TRUE,
            quantiles = c(0, 0.25, 0.50, 0.75, 1),
            keep.var = FALSE)[, 2:6])

## Calculate crude number of individuals
data %>%
    select(DBP, Sex) %>%
    filter(complete.cases(.)) %>%
    group_by(Sex) %>%
    summarise(n = n()) %>%
    kable()

## T-test
svyttest(DBP ~ Sex, design = design_obj)

## Plot the data
png(filename = 'figures/04_diastolic_sex.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(DBP ~ Sex,
           design = design_obj,
           all.outliers = TRUE,
           ylab = 'Pressure (mm Hg)',
           main = 'Diastolic (by sex)',
           ylim = c(40, 120),
           xaxt = 'n',
           las = 2,
           cex.main = 2,
           cex.axis = 1.2,
           cex.lab = 1.5)

axis(1,
     at = c(1, 2),
     labels = c('Female', 'Male'),
     cex.axis = 1.2)

abline(h = 90,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

#-- Stratified by age group --#
# Systolic
## Calculate mean and 95% CI
systolic_age <- svyby(~SBP,
                      by = ~Age_categories,
                      FUN = svymean,
                      design = design_obj,
                      vartype = 'ci',
                      na.rm = TRUE)

round(systolic_age[, 2:4])

## Calculate quantiles
round(svyby(~SBP,
            by = ~Age_categories,
            FUN = svyquantile,
            design = design_obj,
            na.rm = TRUE,
            quantiles = c(0, 0.25, 0.50, 0.75, 1),
            keep.var = FALSE)[, 2:6])

## Calculate crude number of individuals
data %>%
    select(SBP, Age_categories) %>%
    filter(complete.cases(.)) %>%
    group_by(Age_categories) %>%
    summarise(n = n()) %>%
    kable()

## OLS for age in years
summary(svyglm(SBP ~ Age_years,
               design = design_obj))

## ANOVA for age catgories
summary(svyglm(SBP ~ Age_categories,
               design = design_obj))

## Plot the data
png(filename = 'figures/05_systolic_age-category.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(SBP ~ Age_categories,
           design = design_obj,
           all.outliers = TRUE,
           ylab = 'Pressure (mm Hg)',
           xlab = 'Years',
           main = 'Systolic (by age group)',
           ylim = c(40, 220),
           xaxt = 'n',
           las = 2,
           cex.main = 2,
           cex.axis = 1.2,
           cex.lab = 1.5)

axis(1,
     at = c(1, 2, 3, 4, 5, 6),
     labels = c('15-24', '25-34', '35-44', '45-54', '55-64', '65+'),
     cex.axis = 1.2)

abline(h = 140,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

# Diastolic
## Calculate mean and 95% CI
round(svyby(~DBP,
            by = ~Age_categories,
            FUN = svyquantile,
            design = design_obj,
            na.rm = TRUE,
            quantiles = c(0, 0.25, 0.50, 0.75, 1),
            keep.var = FALSE)[, 2:6])

## Calculate quantiles
round(svyby(~DBP,
            by = ~Age_categories,
            FUN = svyquantile,
            design = design_obj,
            na.rm = TRUE,
            quantiles = c(0, 0.25, 0.50, 0.75, 1),
            keep.var = FALSE)[, 2:6])

## Calculate crude number of individuals
data %>%
    select(DBP, Age_categories) %>%
    filter(complete.cases(.)) %>%
    group_by(Age_categories) %>%
    summarise(n = n()) %>%
    kable()

## OLS for age in years
summary(svyglm(DBP ~ Age_years,
               design = design_obj))

## ANOVA for age categories
summary(svyglm(DBP ~ Age_categories,
               design = design_obj))

## Plot the data
png(filename = 'figures/06_diastolic_age-category.png',
    res = 300, width = 2400, height = 2400)

par(bty = 'n',
    mar = c(5, 5, 5, 5))

svyboxplot(DBP ~ Age_categories,
           design = design_obj,
           all.outliers = TRUE,
           ylab = 'Pressure (mm Hg)',
           xlab = 'Years',
           main = 'Diastolic (by age group)',
           ylim = c(40, 120),
           xaxt = 'n',
           las = 2,
           cex.main = 2,
           cex.axis = 1.2,
           cex.lab = 1.5)

axis(1,
     at = c(1, 2, 3, 4, 5, 6),
     labels = c('15-24', '25-34', '35-44', '45-54', '55-64', '65+'),
     cex.axis = 1.2)

abline(h = 90,
       col = 'red',
       lwd = 2,
       lty = 3)

dev.off()

##########################################
#   Hypertension: measured vs reported   #
##########################################

#-- No stratification --#
# Question
## Crude sample size
data %>%
    filter(!is.na(Hypertension_question)) %>%
    group_by(Hypertension_question) %>%
    summarise(n = n()) %>%
    mutate(total = sum(n))

## Calculate proportion
HT_question <- svymean(~Hypertension_question,
                       design = design_obj,
                       na.rm = TRUE)

## Convert to svystat object to dataframe
HT_question_df <- HT_question %>%
    as.data.frame(.)

## Calculate CI from svystat to dataframe
HT_question_ci <- confint(HT_question) %>%
    as.data.frame(.)

## Column bind, clean-up, and convert to a percentage
HT_question_perc <- bind_cols(HT_question_df, HT_question_ci) %>%
    rownames_to_column(var = 'outcome') %>%
    mutate(across(where(is.numeric), ~ 100 * round(.x, 3)))

HT_question_perc$outcome <- c('No', 'Yes')
HT_question_perc$group <- 'Question'

# Measured
## Crude sample size
data %>%
    filter(!is.na(Hypertension_measured)) %>%
    group_by(Hypertension_measured) %>%
    summarise(n = n()) %>%
    mutate(total = sum(n)) %>%
    kable()

## Calculate proportion
HT_measured <- svymean(~Hypertension_measured,
                       design = design_obj,
                       na.rm = TRUE)

## Convert to svystat object to dataframe
HT_measured_df <- HT_measured %>%
    as.data.frame(.)

## Calculate CI from svystat to dataframe
HT_measured_ci <- confint(HT_measured) %>%
    as.data.frame(.)

## Column bind, clean-up, and convert to a percentage
HT_measured_perc <- bind_cols(HT_measured_df, HT_measured_ci) %>%
    rownames_to_column(var = 'outcome') %>%
    mutate(across(where(is.numeric), ~ 100 * round(.x, 3)))

HT_measured_perc$outcome <- c('No', 'Yes')
HT_measured_perc$group <- 'Measured'

HT_combined <- bind_rows(HT_question_perc, HT_measured_perc) %>%
    filter(outcome == 'Yes')

HT_combined %>%
    kable()

# Percent change
HT_combined %>%
    select(group, mean) %>%
    mutate(mean = mean / 100) %>%
    pivot_wider(names_from = group,
                values_from = mean) %>%
    mutate(percent_points = 100 * (Measured - Question),
           percent_change = 100 * ((Measured - Question) / Question)) %>%
    select(-Question, -Measured) %>%
    kable(digits = 1)

# Plot data
plot_HT <- ggplot(data = HT_combined) +
    aes(x = group,
        y = mean,
        colour = group,
        fill = group,
        ymin = `2.5 %`,
        ymax = `97.5 %`) +
    geom_col(alpha = 0.5) +
    geom_errorbar(width = 0.3,
                  size = 1) +
    labs(y = 'Percent with hypertension (%)',
         x = 'Method of diagnosis') +
    scale_colour_tableau() +
    scale_fill_tableau() +
    scale_y_continuous(limits = c(0, 50),
                       expand = c(0, 0)) +
    scale_x_discrete(label = c('Blood pressure\nmeasured',
                               'Recalled being\ndiagnosed')) +
    theme(legend.position = 'none')

ggsave(filename = 'figures/07_hypertension_full-cohort.png',
       plot = plot_HT,
       width = 6,
       height = 5.5)

#-- Stratified by sex --#
# Question
## Crude sample size
data %>%
    filter(!is.na(Hypertension_question)) %>%
    filter(!is.na(Sex)) %>%
    group_by(Sex, Hypertension_question) %>%
    summarise(n = n()) %>%
    group_by(Sex) %>%
    mutate(total_by_sex = sum(n)) %>%
    ungroup() %>%
    mutate(grand_total = sum(n)) %>%
    kable()

## Calculate proportion and convert to a percentage
HT_question_sex <- svyby(~Hypertension_question,
                         by = ~Sex,
                         FUN = svymean,
                         design = design_obj,
                         vartype = 'ci',
                         na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Sex, ends_with('Yes')) %>%
    set_names(nm = c('sex', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Question',
           hypertension = 'Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(HT_question_sex) <- NULL

# Measured
## Crude sample size
data %>%
    filter(!is.na(Hypertension_measured)) %>%
    filter(!is.na(Sex)) %>%
    group_by(Sex, Hypertension_measured) %>%
    summarise(n = n()) %>%
    group_by(Sex) %>%
    mutate(total_by_sex = sum(n)) %>%
    ungroup() %>%
    mutate(grand_total = sum(n)) %>%
    kable()

## Calculate proportion and convert to a percentage
HT_measured_sex <- svyby(~Hypertension_measured,
                         by = ~Sex,
                         FUN = svymean,
                         design = design_obj,
                         vartype = 'ci',
                         na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Sex, ends_with('Yes')) %>%
    set_names(nm = c('sex', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Measured',
           hypertension = 'Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(HT_measured_sex) <- NULL

HT_combined_sex <- bind_rows(HT_question_sex, HT_measured_sex)

HT_combined_sex %>%
    kable(digits = 1)

# Percent change
HT_combined_sex %>%
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
plot_HT_sex <- ggplot(data = HT_combined_sex) +
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
    labs(y = 'Percent with hypertension (%)',
         x = 'Sex') +
    scale_colour_tableau(name = 'Method of diagnosis',
                         label = c('Blood pressure measured',
                                   'Recalled being diagnosed')) +
    scale_fill_tableau(name = 'Method of diagnosis',
                       label = c('Blood pressure measured',
                                 'Recalled being diagnosed')) +
    scale_y_continuous(limits = c(0, 100),
                       expand = c(0, 0)) +
    scale_x_discrete() +
    theme(legend.position = c(0.78, 0.93))

ggsave(filename = 'figures/08_hypertension_sex.png',
       plot = plot_HT_sex,
       width = 7,
       height = 6)

#-- Stratified by age --#
# Question
## Crude sample size
data %>%
    filter(!is.na(Hypertension_question)) %>%
    filter(!is.na(Age_categories)) %>%
    group_by(Age_categories, Hypertension_question) %>%
    summarise(n = n()) %>%
    group_by(Age_categories) %>%
    mutate(total_by_age = sum(n)) %>%
    ungroup() %>%
    mutate(grand_total = sum(n)) %>%
    kable()

## Calculate proportion and convert to a percentage
HT_question_age <- svyby(~Hypertension_question,
                         by = ~Age_categories,
                         FUN = svymean,
                         design = design_obj,
                         vartype = 'ci',
                         na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Age_categories, ends_with('Yes')) %>%
    set_names(nm = c('age_categories', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Question',
           hypertension = 'Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(HT_question_age) <- NULL

# Measured
## Crude sample size
data %>%
    filter(!is.na(Hypertension_measured)) %>%
    filter(!is.na(Age_categories)) %>%
    group_by(Age_categories, Hypertension_measured) %>%
    summarise(n = n()) %>%
    group_by(Age_categories) %>%
    mutate(total_by_age = sum(n)) %>%
    ungroup() %>%
    mutate(grand_total = sum(n)) %>%
    kable()

## Calculate proportion and convert to a percentage
HT_measured_age <- svyby(~Hypertension_measured,
                         by = ~Age_categories,
                         FUN = svymean,
                         design = design_obj,
                         vartype = 'ci',
                         na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Age_categories, ends_with('Yes')) %>%
    set_names(nm = c('age_categories', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Measured',
           hypertension = 'Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(HT_measured_age) <- NULL

HT_combined_age <- bind_rows(HT_question_age, HT_measured_age)

HT_combined_age %>%
    kable(digits = 1)

# Percent change
HT_combined_age %>%
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
plot_HT_age <- ggplot(data = HT_combined_age) +
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
    labs(y = 'Percent with hypertension (%)',
         x = 'Age group (Years)') +
    scale_colour_tableau(name = 'Method of diagnosis',
                         label = c('Blood pressure measured',
                                   'Recalled being diagnosed')) +
    scale_fill_tableau(name = 'Method of diagnosis',
                       label = c('Blood pressure measured',
                                 'Recalled being diagnosed')) +
    scale_y_continuous(limits = c(0, 100),
                       expand = c(0, 0)) +
    scale_x_discrete() +
    theme(legend.position = c(0.2, 0.93))

ggsave(filename = 'figures/09_hypertension_age.png',
       plot = plot_HT_age,
       width = 8,
       height = 6)

#-- Who has hypertension (measured) vs who recalled receiving a diagnosis --#
# Crude numbers
data %>%
    select(Hypertension_question, Hypertension_measured) %>%
    filter(complete.cases(.)) %>%
    group_by(Hypertension_measured, Hypertension_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Grand_total = sum(Count))

# Analysis
measured_vs_question_yes <- svyby(~Hypertension_question,
                                  by = ~Hypertension_measured,
                                  FUN = svymean,
                                  design = design_obj,
                                  vartype = 'ci',
                                  na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Hypertension_measured, ends_with('Yes')) %>%
    set_names(nm = c('Hypertension_measured', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Question: Yes') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(measured_vs_question_yes) <- NULL

measured_vs_question_no <- svyby(~Hypertension_question,
                                 by = ~Hypertension_measured,
                                 FUN = svymean,
                                 design = design_obj,
                                 vartype = 'ci',
                                 na.rm = TRUE) %>%
    as.data.frame() %>%
    select(Hypertension_measured, ends_with('No')) %>%
    set_names(nm = c('Hypertension_measured', 'estimate', 'ci_lower', 'ci_upper')) %>%
    mutate(group = 'Question: No') %>%
    mutate(across(where(is.numeric), ~100 * round(.x, 3)))

rownames(measured_vs_question_no) <- NULL

rbind(measured_vs_question_no, measured_vs_question_yes) %>%
    kable()

# X-tabulate data taking design into account
tab_measured_vs_question <- svytable(~Hypertension_question +
                                          Hypertension_measured,
                                     design = design_obj)

# Check proportions match those generated in previous steps (Analysis)
prop.table(tab_measured_vs_question, margin = 2)

#-- Sample size for Rx_medicines_seen and Hypertension_treatment_question --#
# Rx_medicines_seen
data %>%
    select(Rx_medicines_seen) %>%
    filter(complete.cases(.)) %>%
    group_by(Rx_medicines_seen) %>%
    summarise(Count = n())

# Hypertension_treatment_question
data %>%
    select(Hypertension_treatment_question) %>%
    filter(complete.cases(.)) %>%
    group_by(Hypertension_treatment_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count))

#-- Sample size of participants: Rx_medicines_seen with Rx_hypertension data --#
data %>%
    select(Rx_medicines_seen, Rx_hypertension) %>%
    filter(complete.cases(.)) %>%
    group_by(Rx_medicines_seen, Rx_hypertension) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    mutate(Percent = 100 * (Count / Total))

#-- Sample size of participants: Rx_medicines_seen with Hypertension_treatment_question data --#
data %>%
    select(Rx_medicines_seen, Hypertension_treatment_question) %>%
    filter(complete.cases(.)) %>%
    group_by(Rx_medicines_seen, Hypertension_treatment_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    mutate(Percent = 100 * (Count / Total))

#-- Rx for hypertension (prescription data) in those with/without hypertension (measured and question) --#
data %>%
    select(Rx_medicines_seen,
           Hypertension_question,
           Hypertension_measured,
           Rx_hypertension) %>%
    filter(complete.cases(.)) %>%
    group_by(Hypertension_question, Hypertension_measured, Rx_hypertension) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Grand_total = sum(Count))

tab_rx <- svytable(~Hypertension_measured + Hypertension_question +
                        Rx_hypertension + Rx_medicines_seen,
                   design = design_obj)

ftable(tab_rx) %>%
    as.data.frame() %>%
    group_by(Rx_medicines_seen, Hypertension_measured,
             Hypertension_question) %>%
    mutate(Sub_total = sum(Freq)) %>%
    arrange(Sub_total) %>%
    mutate(Percent_sub_total = 100 * (Freq/Sub_total)) %>%
    kable()

#-- Qx for hypertension (question data) in those with/without hypertension (measured and question) --#
data %>%
    select(Hypertension_question,
           Hypertension_measured,
           Hypertension_treatment_question) %>%
    filter(complete.cases(.)) %>%
    group_by(Hypertension_question, Hypertension_measured,
             Hypertension_treatment_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Grand_total = sum(Count))

tab_rx_question <- svytable(~Hypertension_question + Hypertension_measured +
                                Hypertension_treatment_question,
                            design = design_obj)

ftable(tab_rx_question) %>%
    as.data.frame() %>%
    group_by(Hypertension_measured, Hypertension_question) %>%
    mutate(Sub_total = sum(Freq)) %>%
    arrange(Sub_total) %>%
    mutate(Percent_sub_total = 100 * (Freq/Sub_total)) %>%
    kable()

#-- Rx_hypertension vs hypertension_treatment_question --#
data %>%
    select(Rx_medicines_seen,
           Hypertension_treatment_question,
           Rx_hypertension) %>%
    filter(complete.cases(.)) %>%
    group_by(Rx_medicines_seen, Rx_hypertension, Hypertension_treatment_question) %>%
    summarise(Count = n()) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Grand_total = sum(Count))

tab_rx_vs_question <- svytable(~Rx_hypertension + Hypertension_treatment_question +
                                   Rx_medicines_seen,
                               design = design_obj)

ftable(tab_rx_vs_question) %>%
    as.data.frame() %>%
    group_by(Rx_hypertension) %>%
    mutate(Sub_total = sum(Freq)) %>%
    arrange(Sub_total) %>%
    mutate(Percent_sub_total = 100 * (Freq/Sub_total)) %>%
    kable()

########################
#   Publication plot   #
########################
# Overall
plot_HT_stack <- ggplot(data = HT_combined) +
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
                         label = c('Blood pressure measured',
                                   'Recalled being diagnosed')) +
    scale_fill_tableau(name = 'Method of diagnosis',
                       label = c('Blood pressure measured',
                                 'Recalled being diagnosed')) +
    scale_y_continuous(limits = c(0, 100),
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
plot_HT_sex_stack <- ggplot(data = HT_combined_sex) +
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
         y = 'Percent with hypertension (%)',
         x = NULL) +
    scale_colour_tableau() +
    scale_fill_tableau() +
    scale_y_continuous(limits = c(0, 100),
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
plot_HT_age_stack <- ggplot(data = HT_combined_age) +
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
    scale_y_continuous(limits = c(0, 100),
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
plot_stacked <- plot_HT_stack +
    plot_HT_sex_stack +
    plot_HT_age_stack +
    plot_layout(ncol = 1)

ggsave(filename = 'figures/figure-1.png',
       plot = plot_stacked,
       height = 16,
       width = 8)

# Diagnosed vs measured
png(filename = 'figures/figure-3_original.png',
    res = 300,
    width = 2400,
    height = 2400)
mosaicplot(t(tab_measured_vs_question),
           main = NULL,
           color = c('grey30', 'grey90'),
           cex.axis = 1.2)
dev.off()
