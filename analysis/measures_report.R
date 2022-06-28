pacman::p_load(tidyverse, scales, lubridate)

ym_short_labeller <- function(x) {
  labels <- as.character(x)
  years <- lubridate::year(labels); months <- lubridate::month(labels, label = TRUE)
  years <- str_c(years,'-')
  years[duplicated(years)] <- ''
  labels <- str_c(years,months,sep='')
  return(labels)
}

measures <- read.csv(here::here("output/measures", "measure_prescribing.csv"), header = TRUE)
measures <- measures %>% mutate(date = as.Date(date))

p_measures <- ggplot(measures, aes(x = date, y = value)) + geom_line(col='blue') + scale_x_date(expand = c(0,0), breaks = "3 month", minor_breaks = "month", labels = ym_short_labeller) + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + labs(x = 'Date', y = 'Proportion patients prescribed long-term repeat antimicrobials')

ggsave(plot = p_measures, filename = "measure_prescribing.png", path = here::here("output/measures"), device = "png")