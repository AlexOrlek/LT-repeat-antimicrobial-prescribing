pacman::p_load(tidyverse, scales, lubridate, patchwork)
theme_set(theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

paired_blues <- c("#A6CEE3", "#1F78B4")  # light blue, dark blue
paired_reds <- c("#FB9A99", "#E31A1C")  # light red, dark red

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
measures_repeat_amr <- measures_repeat_amr %>% mutate(date = as.Date(date)) %>% rename(patients_amr = repeat_amr)

measures_non_repeat_amr <- read.csv(here::here("output/measures", "measure_non_repeat_prescribing.csv"), header = TRUE)
measures_non_repeat_amr <- measures_non_repeat_amr %>% mutate(date = as.Date(date)) %>% rename(patients_amr = non_repeat_amr)

# join datasets
measures <- measures_repeat_amr %>% inner_join(measures_non_repeat_amr, by = 'date', suffix = c('_repeat','_non_repeat')) %>% relocate(date,.before = patients_amr_repeat) %>% rename(prop_repeat = value_repeat, prop_non_repeat = value_non_repeat)
stopifnot(all(measures$population_repeat == measures$population_non_repeat))

# ---------------------------
# PROPORTION
# plot proportion of patients on repeat and non-repeat
measures_prop <- measures %>% select(date, 'Non-repeat' = prop_non_repeat, 'Repeat' = prop_repeat) %>% pivot_longer(!date, names_to = 'Prescribing mode', values_to = 'proportion')
measures_prop$`Prescribing mode` <- factor(measures_prop$`Prescribing mode`, levels = c('Non-repeat', 'Repeat'))
measures_prop <- measures_prop %>% mutate(proportion = proportion * 100)  # express proportion as percentage

p_prop <- ggplot(measures_prop, aes(x = date, y = proportion, group = `Prescribing mode`, colour = `Prescribing mode`, linetype = `Prescribing mode`)) + geom_line() + scale_x_date(expand = c(0,0), breaks = "1 month", minor_breaks = "1 month", labels = ym_short_labeller) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = 'top') + labs(x = 'Date', y = 'Percent of patients prescribed antibiotics', colour = 'Prescribing mode') + scale_colour_manual(values = c(paired_blues[2], paired_reds[2])) + scale_linetype_manual(name = 'Prescribing mode', values = c('dashed', 'solid'))

ggsave(plot = p_prop, filename = "measure_prescribing.png", path = here::here("output/measures/plots"), dpi = 600, width = 5, height = 4)
ggsave(plot = p_prop, filename = "measure_prescribing.pdf", path = here::here("output/measures/plots"), width = 5, height = 4)

# ---------------------------
# PERCENT CHANGE FROM BASELINE
# calculate monthly percent change from baseline
measures_perc_change <- measures %>% select(date, starts_with("patients_")) %>%
  mutate(amr_repeat_perc_change = ((patients_amr_repeat / first(patients_amr_repeat))-1) * 100) %>%
  mutate(amr_non_repeat_perc_change = ((patients_amr_non_repeat / first(patients_amr_non_repeat))-1) * 100)

measures_perc_change <- measures_perc_change %>% select(date, 'Non-repeat' = amr_non_repeat_perc_change, 'Repeat' = amr_repeat_perc_change) %>% pivot_longer(!date, names_to = 'Prescribing mode', values_to = 'percent change')
measures_perc_change$`Prescribing mode` <- factor(measures_perc_change$`Prescribing mode`, levels = c('Non-repeat', 'Repeat'))

# record max percent decrease/increase
max_perc_decrease <- measures_perc_change %>% group_by(`Prescribing mode`) %>% filter(`percent change` == min(`percent change`)) %>% arrange(match(`Prescribing mode`, c('Non-repeat', 'Repeat')))
max_perc_increase <- measures_perc_change %>% group_by(`Prescribing mode`) %>% filter(`percent change` == max(`percent change`)) %>% arrange(match(`Prescribing mode`, c('Non-repeat', 'Repeat')))
max_perc_change <- bind_rows(list('Max decrease' = max_perc_decrease, 'Max increase' = max_perc_increase), .id = 'id')
write.csv(max_perc_change, file = here::here("output/measures/tables/max_percent_change.csv"), row.names = FALSE)

# plot percent change
p_perc_change <- ggplot(measures_perc_change, aes(x = date, y = `percent change`, group = `Prescribing mode`, colour = `Prescribing mode`, linetype = `Prescribing mode`)) + geom_line() + scale_x_date(expand = c(0,0), breaks = "3 month", minor_breaks = "1 month", labels = ym_short_labeller) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = 'top') + labs(x = 'Date', y = 'Percent change in patients\nprescribed antibiotics (vs Jan 2020)', colour = 'Prescribing mode') + scale_colour_manual(values = c(paired_blues[2], paired_reds[2])) + scale_linetype_manual(name = 'Prescribing mode', values = c('dashed', 'solid'))

ggsave(plot = p_perc_change, filename = "measure_change_prescribing.png", path = here::here("output/measures/plots"), dpi = 600, width = 5, height = 4)
ggsave(plot = p_perc_change, filename = "measure_change_prescribing.pdf", path = here::here("output/measures/plots"), width = 5, height = 4)

# COMBINE PLOTS
p_prop_final <- p_prop + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(), legend.position = 'top', panel.grid.minor.x = element_line(), panel.grid.major.x = element_line()) + labs(x = '') + geom_vline(xintercept = as.Date(c('2020-03-23', '2021-07-19')), linetype = 'dotted', colour = 'black')
p_perc_change_final <- p_perc_change + theme(legend.position = 'none', panel.grid.minor.x = element_line(), panel.grid.major.x = element_line()) + geom_vline(xintercept = as.Date(c('2020-03-23', '2021-07-19')), linetype = 'dotted', colour = 'black')

p_combined <- p_prop_final / p_perc_change_final + plot_annotation(tag_levels = 'A')

ggsave(plot = p_combined, filename = "measure_change_prescribing_combined.png", path = here::here("output/measures/plots"), dpi = 600, width = 4.5, height = 7)
ggsave(plot = p_combined, filename = "measure_change_prescribing_combined.pdf", path = here::here("output/measures/plots"), width = 4.5, height = 7)

