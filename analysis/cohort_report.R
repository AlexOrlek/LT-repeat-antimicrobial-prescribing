pacman::p_load('tidyverse')
theme_set(theme_bw())
args = commandArgs(trailingOnly=TRUE)
cohort_date = args[1]

# read cohort data
cohort_df <- read.csv(here::here("output", gsub("%s", cohort_date, "input_%s.csv")), header = TRUE)
cohort_df <- cohort_df %>% mutate(amr_6_months_binary = ifelse(amr_6_months > 0 , 1, 0))
#cohort_df %>% head()

# proportion LT-repeat
lt_repeat_prop <- as.data.frame(table(cohort_df$amr_6_months_binary)) %>% rename(LT_repeat = Var1)
write.csv(lt_repeat_prop, here::here("output", gsub("%s", cohort_date, "lt_repeat_prop_%s.csv")), col.names = TRUE, row.names = FALSE)

# plot age distribution
p_age <- ggplot(cohort_df, aes(x = age)) + geom_histogram()
ggsave(plot = p_age, filename = gsub("%s", cohort_date, "age_distribution_%s.pdf"), path = here::here("output"), device = "pdf")
