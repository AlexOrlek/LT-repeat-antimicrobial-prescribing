pacman::p_load('tidyverse')
theme_set(theme_bw())


cohort_df <- read.csv("output/input.csv", header = TRUE)
#cohort_df %>% head()

# plot age distribution
p_age <- ggplot(cohort_df, aes(x = age)) + geom_histogram()
ggsave(p_age, filename = "output/age_distribution.pdf", device = "pdf")


