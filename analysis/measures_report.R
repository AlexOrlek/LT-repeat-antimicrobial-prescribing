pacman::p_load(tidyverse, scales, lubridate)

ym_short_labeller <- function(x) {
  labels <- as.character(x)
  years <- lubridate::year(labels); months <- lubridate::month(labels, label = TRUE)
  years <- str_c(years,'-')
  years[duplicated(years)] <- ''
  labels <- str_c(years,months,sep='')
  return(labels)
}

# load measures output for repeat and non repeat antibiotic prescribing
measures_repeat_amr <- read.csv(here::here("output/measures", "measure_repeat_prescribing.csv"), header = TRUE)
measures_repeat_amr <- measures_repeat_amr %>% mutate(date = as.Date(date))

measures_non_repeat_amr <- read.csv(here::here("output/measures", "measure_non_repeat_prescribing.csv"), header = TRUE)
measures_non_repeat_amr <- measures_non_repeat_amr %>% mutate(date = as.Date(date))

# join datasets
measures <- measures_repeat_amr %>% inner_join(measures_non_repeat_amr, by = 'date', suffix = c('_repeat','_non_repeat'))
measures <- measures %>% select(date, 'Repeat prescribing' = value_repeat, 'Non-repeat prescribing' = value_non_repeat) %>% pivot_longer(!date, names_to = 'prescribing', values_to = 'proportion')

# plot
p_measures <- ggplot(measures, aes(x = date, y = proportion, group = prescribing, colour = prescribing)) + geom_line() + scale_x_date(expand = c(0,0), breaks = "3 month", minor_breaks = "month", labels = ym_short_labeller) + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs(x = 'Date', y = 'Proportion of patients prescribed repeat and non-repeat antibiotics', colour = '')

ggsave(plot = p_measures, filename = "measure_prescribing.png", path = here::here("output/measures"), device = "png")