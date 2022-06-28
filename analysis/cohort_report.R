pacman::p_load(tidyverse)
theme_set(theme_bw())
args = commandArgs(trailingOnly=TRUE)
cohort_date = args[1]

# read cohort data
cohort_df <- read.csv(here::here("output", gsub("%s", cohort_date, "input_%s.csv")), header = TRUE)

# ---------------------------
# COUNT REPEAT AND NON REPEAT PRESCRIBING
cohort_df <- cohort_df %>% mutate(repeat_amr = ifelse(amr_6_months >= 3, 1, 0)) %>% mutate(non_repeat_amr = ifelse(amr_6_months < 3 & amr_6_months > 0, 1, 0))  # append repeat_amr and non_repeat_amr
n_prescribing_df <- cohort_df %>% summarise(n_total = n(), n_repeat = sum(repeat_amr), n_non_repeat = sum(non_repeat_amr))
write.csv(n_prescribing_df, here::here("output", gsub("%s", cohort_date, "n_prescribing_%s.csv")), col.names = TRUE, row.names = FALSE)


# ---------------------------
# COUNT REPEAT PRESCRIBING INDICATIONS
# recode inications ("had_" columns); set indication (binary) to 0 unless patient has repeat_amr
cohort_df <- cohort_df %>%
  mutate(across(
    .cols = starts_with("had_"),
    .fns = ~ ifelse(repeat_amr == 1, .x, 0)
  ))

# sum indications and calculate proportion relative to repeat_amr
n_indications <- cohort_df %>%
  summarise(across(
    .cols = c(repeat_amr, starts_with("had_")),  # summarise across repeat_amr and indications
    .fns = sum,
    .names = "sum_{.col}")
  ) %>% mutate(across(
    .cols = starts_with("sum_had"),  # mutate across summed indications...
    .fns = ~ .x / sum_repeat_amr,  # ...divide by sum of repeat_amr
    .names = "prop_{.col}")
  )

write.csv(n_indications, here::here("output", gsub("%s", cohort_date, "n_indications_%s.csv")), col.names = TRUE, row.names = FALSE)


# ---------------------------
# COUNT REPEAT PRESCRIBING ACROSS CLASSES
# sum repeat prescribing across antibiotic classes
n_repeat_amr <- cohort_df %>% select(-amr_6_months) %>% mutate(across(  # remove amr_6_months column since repeat_amr has already been calculated; want to get repeat cols for amr classes; then sum repeat_ cols
    .cols = ends_with("_6_months"),  # mutate across antibiotic class columns
    .fns = ~ ifelse(.x >=3, 1, 0),
    .names = "repeat_{.col}")
  ) %>% summarise(across(
    .cols = starts_with("repeat_"),  # sum across repeat_ columns (repeat_amr and repeat_ antibiotic class columns)
    .fns = sum,
    .names = "sum_{.col}")
  ) %>% rename_with(~sub(x = ., pattern = "_6_months", ""))  # remove unecessary suffix from column names

write.csv(n_repeat_amr, here::here("output", gsub("%s", cohort_date, "n_repeat_amr_%s.csv")), col.names = TRUE, row.names = FALSE)


# ---------------------------
# DEMOGRAPHIC PYRAMIDS
# categorise age
cohort_df <- cohort_df %>% mutate(age_cat = case_when(age >= 18 & age < 30 ~ '18-29',
                                         age >= 30  & age < 40 ~ '30-39',
                                         age >= 40  & age < 50 ~ '40-49',
                                         age >= 50  & age < 60 ~ '50-59',
                                         age >= 60  & age < 70 ~ '60-69',
                                         age >= 70  & age < 80 ~ '70-79',
                                         age >= 80  & age <= 120 ~ '80-120')) %>% mutate(age_cat = as.factor(age_cat))

# ensure missing/other sex excluded
age_sex_df <- cohort_df %>% filter(sex %in% c('F','M'))

# plot demographic pyramids for repeat_amr and non_repeat_amr patients
plot_age_sex <- function(amr_colname = 'repeat_amr') {
  age_sex_df <- age_sex_df[age_sex_df[amr_colname] == 1, ]
  n_age_sex <- age_sex_df %>% nrow()
  
  # count age-sex and make female counts negative
  age_sex_df <- age_sex_df %>% count(age_cat, sex) %>% complete(age_cat = levels(age_cat), sex = c("F", "M"), fill = list(n = 0))
  age_sex_df <- age_sex_df %>% mutate(n = ifelse(sex == 'F', n * -1, n))
  
  # plot age-sex pyramid
  p_age_sex <- ggplot() + geom_col(data = age_sex_df, mapping = aes(x = age_cat, y = n, fill = sex), colour = "white", width = 1) +   # white around each bar; bar width = 1 means no gap between bars
    coord_flip() +  # flip the X and Y axes to make pyramid vertical
    theme_bw() + scale_y_continuous(label = as_labeller(function(x) abs(as.numeric(x)))) +
    labs(x = "Age group", y = "Number of patients by age/sex", fill = NULL, caption = str_c('Total N = ',n_age_sex))
  return(p_age_sex)
}

p_age_sex_repeat <- plot_age_sex(amr_colname = 'repeat_amr')
p_age_sex_non_repeat <- plot_age_sex(amr_colname = 'non_repeat_amr')
ggsave(plot = p_age_sex_repeat, filename = gsub("%s", cohort_date, "demographic_pyramid_repeat_amr_%s.png"), path = here::here("output"), device = "png")
ggsave(plot = p_age_sex_non_repeat, filename = gsub("%s", cohort_date, "demographic_pyramid_non_repeat_amr_%s.png"), path = here::here("output"), device = "png")
