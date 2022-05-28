pacman::p_load(tidyverse)
theme_set(theme_bw())
args = commandArgs(trailingOnly=TRUE)
cohort_date = args[1]

# read cohort data
cohort_df <- read.csv(here::here("output", gsub("%s", cohort_date, "input_%s.csv")), header = TRUE)

# filter for LT-repeat and count population
n_total <- nrow(cohort_df)
cohort_df <- cohort_df %>% filter(amr_6_months >= 3)
n_lt_repeat <- nrow(cohort_df)
n_lt_repeat_df <- data.frame('n_total' = n_total, 'n_lt_repeat' = n_lt_repeat)
write.csv(n_lt_repeat_df, here::here("output", gsub("%s", cohort_date, "n_lt_repeat_%s.csv")), col.names = TRUE, row.names = FALSE)

# categorise age
cohort_df <- cohort_df %>% mutate(age_cat = case_when(age >= 18 & age < 30 ~ '18-29',
                                         age >= 30  & age < 40 ~ '30-39',
                                         age >= 40  & age < 50 ~ '40-49',
                                         age >= 50  & age < 60 ~ '50-59',
                                         age >= 60  & age < 70 ~ '60-69',
                                         age >= 70  & age < 80 ~ '70-79',
                                         age >= 80  & age <= 120 ~ '80-120')) %>% mutate(age_cat = as.factor(age_cat))

# exclude missing/other sex
age_sex_df <- cohort_df %>% filter(sex %in% c('F','M'))
n_age_sex <- age_sex_df %>% nrow()

# plot demographic pyramid
# count age-sex and make female counts negative
age_sex_df <- age_sex_df %>% count(age_cat, sex) %>% complete(age_cat = levels(age_cat), sex = c("F", "M"), fill = list(n = 0))
age_sex_df <- age_sex_df %>% mutate(n = ifelse(sex == 'F', n * -1, n))

# plot age-sex pyramid
p_age_sex <- ggplot() + geom_col(data = age_sex_df, mapping = aes(x = age_cat, y = n, fill = sex), colour = "white", width = 1) +   # white around each bar; bar width = 1 means no gap between bars
  coord_flip() +  # flip the X and Y axes to make pyramid vertical
  theme_bw() + scale_y_continuous(label = as_labeller(function(x) abs(as.numeric(x)))) +
  labs(x = "Age group", y = "Number of patients by age/sex", fill = NULL, caption = str_c('Total N = ',n_age_sex))
ggsave(plot = p_age_sex, filename = gsub("%s", cohort_date, "demographic_pyramid_%s.png"), path = here::here("output"), device = "png")