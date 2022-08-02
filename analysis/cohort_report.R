pacman::p_load(tidyverse, data.table)
theme_set(theme_bw())
args = commandArgs(trailingOnly=TRUE)
cohort_date = args[1]

# read cohort data
cohort_df <- read.csv(here::here("output", gsub("%s", cohort_date, "input_%s.csv")), header = TRUE)

# ---------------------------
# COUNT REPEAT AND NON REPEAT PRESCRIBING
cohort_df <- cohort_df %>% mutate(repeat_amr = ifelse(amr_6_months >= 3, 1, 0)) %>% mutate(non_repeat_amr = ifelse(amr_6_months < 3 & amr_6_months > 0, 1, 0))  # append repeat_amr and non_repeat_amr
n_prescribing_df <- cohort_df %>% summarise(n_total = n(), n_repeat = sum(repeat_amr), n_non_repeat = sum(non_repeat_amr))
n_prescribing_df <- n_prescribing_df %>% mutate(perc_repeat = (n_repeat / n_total) * 100, perc_non_repeat = (n_non_repeat / n_total) * 100)
write.csv(n_prescribing_df, here::here("output", gsub("%s", cohort_date, "n_prescribing_%s.csv")), row.names = FALSE)


# ---------------------------
# COUNT REPEAT PRESCRIBING CLINICAL CONDITIONS
# clinical conditions (indications and comorbidities) are binary variables ("has_" columns); recode to 0 unless patient has repeat_amr
cohort_df <- cohort_df %>%
  mutate(across(
    .cols = starts_with("has_"),
    .fns = ~ ifelse(repeat_amr == 1, .x, 0)
  ))

# append has_any_clinical_condition
cohort_df <- cohort_df %>% rowwise() %>% mutate(has_any_clinical_condition = sum(c_across(starts_with('has_')))) %>% ungroup() %>% mutate(has_any_clinical_condition = ifelse(has_any_clinical_condition > 0, 1, 0))

# sum clinical conditions and calculate proportion relative to sum_repeat_amr (i.e. repeat antibiotic prescription patients a given clinical condition assignment, as proportion of total repeat prescription patients)
n_indications <- cohort_df %>%
  summarise(across(
    .cols = c(repeat_amr, starts_with("has_")),  # summarise across repeat_amr and indications
    .fns = sum,
    .names = "sum_{.col}")
  ) %>% mutate(across(everything(), .fns = ~ ifelse(.x <5, NA, .x))) %>% mutate(across(  # small count suppression
    .cols = starts_with("sum_has"),  # mutate across summed indications...
    .fns = ~ .x / sum_repeat_amr,  # ...divide by sum of repeat_amr
    .names = "prop_{.col}")
  )
write.csv(n_indications, here::here("output", gsub("%s", cohort_date, "n_indications_%s.csv")), row.names = FALSE)

# matrix of co-occurrence between clinical conditions
co_occurrence_matrix <- as.data.frame(crossprod(as.matrix(cohort_df %>% select(starts_with('has_')) %>% select(-has_any_clinical_condition))))
co_occurrence_matrix <- co_occurrence_matrix %>% mutate(across(everything(), .fns = ~ ifelse(.x <5, NA, .x)))  # small count suppression
write.table(co_occurrence_matrix, here::here("output", gsub("%s", cohort_date, "n_indications_co_occurrence_%s.csv")), sep = ',', row.names = colnames(co_occurrence_matrix), col.names = NA)


# ---------------------------
# COUNT REPEAT PRESCRIBING ACROSS CLASSES
# sum repeat prescribing across antibiotic classes and across clinical conditions
# note, antibiotic columns ("_6_months") give number of prescriptions in a 6 month lookback period
has_cols <- colnames(cohort_df)[colnames(cohort_df) %>% str_starts('has_')]
has_cols <- c(NA, has_cols)
n_repeat_amr_subsets <- list()
for (has_col in has_cols) {
  if (is.na(has_col)) {
    # count patients receiving antibiotics by class, across all patients
    n_repeat_amr_all_patients <- cohort_df %>% select(-amr_6_months) %>% mutate(across(  # remove amr_6_months column since repeat_amr has already been calculated; want to get repeat cols for amr classes; then sum repeat_ cols
      .cols = ends_with("_6_months"),  # mutate across antibiotic class columns
      .fns = ~ ifelse(.x >=3, 1, 0),  # convert to binary
      .names = "repeat_{.col}")
    ) %>% summarise(across(
      .cols = starts_with("repeat_"),  # sum across repeat_ columns (repeat_amr and repeat_ antibiotic class columns)
      .fns = sum,
      .names = "sum_{.col}")
    ) %>% rename_with(~sub(x = ., pattern = "_6_months", ""))  # remove unecessary suffix from column names
    n_repeat_amr_subsets[['all patients']] <- n_repeat_amr_all_patients
  } else {
    # count patients receiving antibiotics by class, across patient subsets (patients with a given clinical condition)
    subset_df <- cohort_df[cohort_df[,has_col] > 0,]
    n_repeat_amr_subset <- subset_df %>% select(-amr_6_months) %>% mutate(across(  # remove amr_6_months column since repeat_amr has already been calculated; want to get repeat cols for amr classes; then sum repeat_ cols
      .cols = ends_with("_6_months"),  # mutate across antibiotic class columns
      .fns = ~ ifelse(.x >=3, 1, 0),  # convert to binary
      .names = "repeat_{.col}")
    ) %>% summarise(across(
      .cols = starts_with("repeat_"),  # sum across repeat_ columns (repeat_amr and repeat_ antibiotic class columns)
      .fns = sum,
      .names = "sum_{.col}")
    ) %>% rename_with(~sub(x = ., pattern = "_6_months", ""))  # remove unecessary suffix from column names
    n_repeat_amr_subsets[[has_col]] <- n_repeat_amr_subset
  }
}

# combine counts across clinical condition patient subgroups
n_repeat_amr_combined <- as.data.frame(rbindlist(n_repeat_amr_subsets, idcol = 'clinical condition'))
n_repeat_amr_combined <- n_repeat_amr_combined %>% mutate(across(starts_with('sum_'), .fns = ~ ifelse(.x <5, NA, .x)))  # small count suppression
write.csv(n_repeat_amr_combined, here::here("output", gsub("%s", cohort_date, "n_repeat_amr_%s.csv")), row.names = FALSE)


# ---------------------------
# DEMOGRAPHIC STATISTICS
# categorise age
cohort_df <- cohort_df %>% mutate(age_cat = case_when(age >= 18 & age < 30 ~ '18-29',
                                         age >= 30  & age < 40 ~ '30-39',
                                         age >= 40  & age < 50 ~ '40-49',
                                         age >= 50  & age < 60 ~ '50-59',
                                         age >= 60  & age < 70 ~ '60-69',
                                         age >= 70  & age < 80 ~ '70-79',
                                         age >= 80  & age <= 120 ~ '80-120')) %>% mutate(age_cat = as.factor(age_cat))

# tabular statistics by repeat and non-repeat amr
f_tabulate_by <- function(grouped_df) {
  # function to calculate statistics on grouped dataframe; counts of repeat and non_repeat prescribing and incidence proportion per 100,000 population
  tabular_df <- grouped_df %>% summarise(population = n(), n_repeat_amr = sum(repeat_amr), n_non_repeat_amr = sum(non_repeat_amr), .groups = 'drop') %>% mutate(across(c(population, n_repeat_amr, n_non_repeat_amr), .fns = ~ ifelse(.x <5, NA, .x))) %>% mutate(across(starts_with('n_'), .fns = ~ (.x / population) * 100000, .names = 'incidence_prop_{.col}'))
  return(tabular_df)
}


# age-sex
age_sex_table <- f_tabulate_by(cohort_df %>% group_by(age_cat, sex))
age_sex_table <- age_sex_table %>% unite("age_sex", age_cat:sex, remove = TRUE) %>% rename('characteristic' = age_sex)

# region and stp
region_table <- f_tabulate_by(cohort_df %>% group_by(region)) %>% rename('characteristic' = region)
stp_table <- f_tabulate_by(cohort_df %>% group_by(stp)) %>% rename('characteristic' = stp)

# care home status
care_home_table <- f_tabulate_by(cohort_df %>% group_by(care_home)) %>% rename('characteristic' = care_home)

# imd
imd_table <- f_tabulate_by(cohort_df %>% group_by(imd)) %>% rename('characteristic' = imd) %>% mutate(characteristic = as.character(characteristic))

# ethnicity
ethnicity_table <- f_tabulate_by(cohort_df %>% group_by(ethnicity)) %>% rename('characteristic' = ethnicity)

# combine tables and save
demographics_table <- bind_rows(list('age_sex' = age_sex_table, 'region' = region_table, 'stp' = stp_table, 'care_home' = care_home_table, 'imd' = imd_table, 'ethnicity' = ethnicity_table), .id = "id")
write.csv(demographics_table, here::here("output", gsub("%s", cohort_date, "demographics_table_%s.csv")), row.names = FALSE)
