pacman::p_load(plyr, tidyverse, data.table, scales, lubridate, epitools, janitor, patchwork, forestploter, glue, grid, cowplot)
theme_set(theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

paired_blues <- c("#A6CEE3", "#1F78B4")  # light blue, dark blue
paired_reds <- c("#FB9A99", "#E31A1C")  # light red, dark red

# functions
append_risk <- function(x) {
  # store characteristics
  characteristics <- unlist(x[, 'characteristic'], use.names = FALSE)
  # calculate risk ratios
  amr_mat <- as.matrix(x[,c('n_not_prescribed', 'n_prescribed')])
  rownames(amr_mat) <- characteristics
  risk_output <- riskratio.wald(amr_mat)
  risk_output_measure <- as.data.frame(risk_output$measure) %>% tibble::rownames_to_column("characteristic")
  risk_output_pvalue <- as.data.frame(risk_output$p.value) %>% tibble::rownames_to_column("characteristic")
  risk_df <- risk_output_measure %>% inner_join(risk_output_pvalue, by = 'characteristic')
  return(risk_df)
}

table_1_format <- function(table_df) {
  # input: table with variable and characteristic (=variable factor levels) columns
  # relocate variable names to first row, followed by factor levels and other columns
  table_df_split <- split(table_df, as.factor(table_df$variable))
  for (var in names(table_df_split)) {
    table_df_split[[var]] <- table_df_split[[var]] %>% add_row(characteristic = var, .before = 1)
  }
  table_df_table_1 <- rbindlist(table_df_split) %>% as_tibble() %>% select(-variable)
}

var_labeller <- function(x) {
  x <- str_replace(x, 'age_female', 'Age (female patients)')
  x <- str_replace(x, 'age_male', 'Age (male patients)')
  x <- str_replace(x, 'care_home', 'Residence')
  x <- str_replace(x, 'ethnicity', 'Ethnicity')
  x <- str_replace(x, 'imd', 'IMD quintile')
  x <- str_replace(x, 'region', 'England region')
}

refs <- c('18-29', 'Private home', 'British', '1', 'London')

plot_forest <- function(forest_table) {
  # plot RRs as forest plot
  forest_table$` ` <- paste(rep(" ", nrow(forest_table)), collapse = " ")  # ci_column (mandatory field - leaving blank)
  p <- forest(forest_table[,c('characteristic', ' ')],
              est = list(forest_table$`log_estimate_Jan 2020, prescribed non-repeat`,
                         forest_table$`log_estimate_Jan 2021, prescribed non-repeat`,
                         forest_table$`log_estimate_Jan 2020, prescribed repeat`,
                         forest_table$`log_estimate_Jan 2021, prescribed repeat`),
              lower = list(forest_table$`log_lower_Jan 2020, prescribed non-repeat`,
                           forest_table$`log_lower_Jan 2021, prescribed non-repeat`,
                           forest_table$`log_lower_Jan 2020, prescribed repeat`,
                           forest_table$`log_lower_Jan 2021, prescribed repeat`), 
              upper = list(forest_table$`log_upper_Jan 2020, prescribed non-repeat`,
                           forest_table$`log_upper_Jan 2021, prescribed non-repeat`,
                           forest_table$`log_upper_Jan 2020, prescribed repeat`,
                           forest_table$`log_upper_Jan 2021, prescribed repeat`),
              ci_column = 2,
              ref_line = 0,
              nudge_y = 0.2,
              xlim = c(-2.5, 2.5),
              xlab = 'log Risk ratio',
              theme = tm)
  
  # increase width
  new_widths <- convertWidth(p$widths, "mm", valueOnly = TRUE)
  new_widths <- new_widths*c(1,1.1,1.5,1.1,1)
  new_widths <- unit(new_widths, "mm")
  p$widths <- new_widths
  
  # decrease row height
  p$heights <- rep(unit(8, "mm"), nrow(p))  # change row height
  varname_rows <- which(str_detect(forest_table$characteristic, '^    ', negate = TRUE) == TRUE)
  
  # additional edits
  p <- edit_plot(p, row = varname_rows, gp = gpar(fontface = "bold"))  # make group variable names bold
  p <- edit_plot(p, row = varname_rows, which = "background", gp = gpar(fill = "palegoldenrod"))  # change background colour of group variable names
  p <- edit_plot(p, part = 'header', gp = gpar(col = "white"))  # hide plot header
  return(list('plot' = p, 'widths' = new_widths))
}


plot_pc <- function(pct_change_df, var, ylab, xmin, xmax, vline_non_repeat, vline_repeat) {
  var_df <- pct_change_df %>% filter(variable == {var})
  # plot percent change
  p <- ggplot(var_df, aes(x = incidence_percent_change, y = characteristic, group = prescribing_mode, colour = prescribing_mode, shape = prescribing_mode)) + geom_vline(xintercept = c(0, vline_non_repeat, vline_repeat), linetype=c('dashed', 'dashed', 'dashed'), colour=c('black', paired_blues[2], paired_reds[2]), linewidth = 0.3) + geom_point(size = 2) + scale_x_continuous(breaks = seq(from = xmin, to = xmax, by = 5), limits = c(xmin, xmax)) + scale_colour_manual(values = c(paired_blues[2], paired_reds[2]), labels = c('prescribed non-repeat','prescribed repeat'), name = 'Patient group') + scale_shape_manual(values = c(23, 21), labels = c('prescribed non-repeat','prescribed repeat'), name = 'Patient group')
  # customise
  p <- p + labs(x = 'Percent change in patients prescribed antibiotics\n(Jan 2021 vs Jan 2020)', y = ylab) + theme(plot.margin = unit(c(5.5,5.5,5.5,25), "pt")) #, legend.title = element_text(face = "bold", size = 11), legend.title.align = 0.5
  p <- p + theme(panel.grid.major.y = element_line(colour = 'light grey', linetype = 'dashed'))
  # change order of y axis categorical
  p <- p + scale_y_discrete(limits = rev(var_order_list[[var]]))
  return(p)
}


# ---------------------------
# LOAD AND CLEAN DATA; CALCULATE RISK RATIOS
# read and combine cohort data
cohort_df_2021 <- read.csv(here::here("output", "demographics_table_2021-01-01.csv"), header = TRUE)
cohort_df_2021$year <- '2021'
cohort_df_2020 <- read.csv(here::here("output", "demographics_table_2020-01-01.csv"), header = TRUE)
cohort_df_2020$year <- '2020'

cohort_df <- bind_rows(cohort_df_2020, cohort_df_2021)
cohort_df <- cohort_df %>% mutate(characteristic = ifelse(is.na(characteristic) | characteristic=='', 'Missing', characteristic)) %>% rename(variable = id)

# add anti count column
cohort_df <- cohort_df %>% mutate(n_repeat_amr_anti = population - n_repeat_amr, n_non_repeat_amr_anti = population - n_non_repeat_amr)

# pivot longer
cohort_df_repeat <- cohort_df %>% select(year, variable, characteristic, population, starts_with('n_repeat_amr')) %>% mutate(prescribing_mode = 'repeat_amr') %>% rename(n_prescribed = n_repeat_amr, n_not_prescribed = n_repeat_amr_anti)

cohort_df_non_repeat <- cohort_df %>% select(year, variable, characteristic, population, starts_with('n_non_repeat_amr')) %>% mutate(prescribing_mode = 'non_repeat_amr') %>% rename(n_prescribed = n_non_repeat_amr, n_not_prescribed = n_non_repeat_amr_anti)

cohort_df_long <- bind_rows(cohort_df_repeat,cohort_df_non_repeat)


# recode care home labels
cohort_df_long <- cohort_df_long %>% mutate(characteristic = recode(characteristic, 'CareHome' = 'Care home', 'CareOrNursingHome' = 'Care or nursing home', 'NursingHome' = 'Nursing home', 'PrivateHome' = 'Private home'))

# recode imd (0 -> Missing)
cohort_df_long <- cohort_df_long %>% mutate(characteristic = ifelse(characteristic == '0' & variable == 'imd', 'Missing', characteristic))

# recode age_sex to age_male, age_female
cohort_df_long_age_sex <- cohort_df_long %>% filter(variable == 'age_sex')
cohort_df_long_age_sex <- cohort_df_long_age_sex %>% mutate(variable = ifelse(str_ends(characteristic, '_F'), 'age_female', 'age_male')) %>% mutate(characteristic = str_replace(characteristic, '_[FM]', ''))


cohort_df_clean <- bind_rows((cohort_df_long %>% filter(!variable == 'age_sex')), cohort_df_long_age_sex) %>% arrange(year, prescribing_mode, variable, characteristic)


# re-order variable characteristics (reference top row, Missing bottom row)
var_order_list <- list()
age_female_order <- c("18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-120")
age_male_order <- age_female_order
care_home_order <- c("Private home", "Care home", "Care or nursing home", "Nursing home", "Missing")
ethnicity_order <- c("British",  "African", "Any other Asian background", "Any other Black background", "Any other Mixed background", "Any other White background", "Any other ethnic group", "Bangladeshi", "Caribbean", "Chinese", "Indian", "Irish", "Pakistani", "White and Asian", "White and Black African", "White and Black Caribbean", "Missing")
imd_order <- c('1','2','3','4','5','Missing')
region_order <- c("London", "East", "East Midlands", "North East", "North West", "South East", "South West", "West Midlands", "Yorkshire and The Humber", "Missing")

var_order_list[['age_female']] <- age_female_order
var_order_list[['age_male']] <- age_male_order
var_order_list[['care_home']] <- care_home_order
var_order_list[['ethnicity']] <- ethnicity_order
var_order_list[['imd']] <- imd_order
var_order_list[['region']] <- region_order

cohort_df_clean_split <- split(cohort_df_clean, cohort_df_clean$variable)
for (var in names(cohort_df_clean_split)) {
  var_order <- var_order_list[[var]]
  cohort_df_clean_split[[var]] <- cohort_df_clean_split[[var]] %>% arrange(year, prescribing_mode, variable, match(characteristic, var_order))
}
cohort_df_clean <- rbindlist(cohort_df_clean_split) %>% as.data.frame()


# append incidence_per_1000
cohort_df_clean <- cohort_df_clean %>% mutate(incidence_per_1000 = (n_prescribed / population) * 1000)


# calculate risk ratios and CIs per group (defined by year, prescribing mode, variable)
cohort_df_split <- split(cohort_df_clean, list(cohort_df_clean$year, cohort_df_clean$prescribing_mode, cohort_df_clean$variable))  # interaction of values in list used for the grouping (https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/split)

rr_list <- lapply(cohort_df_split, append_risk)  # calculate RR vs reference; first row of each split is the reference factor level
rr_df <- as.data.frame(data.table::rbindlist(rr_list, idcol = TRUE)) %>% separate(col='.id', into = c('year','prescribing_mode','variable'), sep = '\\.')

# combine count data with calculated RRs and CIs and append log RRs and CIs
rr_df_combined <- rr_df %>% inner_join(cohort_df_clean, by = c('year', 'prescribing_mode', 'variable', 'characteristic')) %>% arrange(year, prescribing_mode, variable)
rr_df_combined <- rr_df_combined %>% mutate(log_estimate = log(estimate), log_lower = log(lower), log_upper = log(upper)) %>% relocate(log_estimate, .after = estimate) %>% relocate(log_lower, .after = lower) %>% relocate(log_upper, .after = upper)


# append percentage change in prescribing incidence
rr_df_combined <- rr_df_combined %>% group_by(prescribing_mode, variable, characteristic) %>% mutate(incidence_percent_change = ifelse(year == 2020, NA, (((incidence_per_1000[year == 2021] - incidence_per_1000[year == 2020])/incidence_per_1000[year == 2020]) * 100))) %>% ungroup()


# ---------------------------
# TABULAR OUTPUT
# save table output (raw data)
write.csv(rr_df_combined, file = 'output/tables/demographics_table_risk_ratios.csv', row.names = FALSE, na = '')
#rr_df_combined <- read.csv('output/tables/demographics_table_risk_ratios.csv')

# save population totals, by year, prescribing mode, all/female/male
demographic_summary_df <- rr_df_combined %>% filter(variable %in% c('age_female', 'age_male', 'care_home')) %>% group_by(year, prescribing_mode, variable) %>% summarise(n = sum(population), n_prescribed = sum(n_prescribed)) %>% arrange(prescribing_mode, year, variable) %>% mutate(strata = recode(variable, 'age_female' = 'female', 'age_male' = 'male', 'care_home' = 'all')) %>% select(-variable) %>% relocate(strata, .before = n) %>% mutate(n_neat = formatC(n, big.mark = ','), n_prescribed_neat = formatC(n_prescribed, big.mark = ','))
write.csv(demographic_summary_df, file = 'output/tables/demographics_table_summary.csv', row.names = FALSE, na = '')

# ---------------------------
# re-format for final repeat/non-repeat RR tables
rr_df_combined_clean_RR <- rr_df_combined %>% select(year, prescribing_mode, variable, characteristic, estimate, lower, upper, log_estimate, log_lower, log_upper, chi.square) %>% mutate_at(vars(c(estimate, log_estimate)), ~ifelse(characteristic %in% refs, NA, .)) %>% mutate(characteristic = ifelse(characteristic %in% refs, glue('{characteristic} (reference)'), characteristic))

rr_df_combined_clean_RR <- rr_df_combined_clean_RR %>% mutate(across(estimate:log_upper, ~ifelse(is.na(.x), NA, formatC(.x, format = 'f', digits = 2)))) %>% mutate(chi.square = case_when(is.na(chi.square) ~ NA, chi.square < 0.0001 ~ "****", TRUE ~ formatC(chi.square, format = 'f', digits = 4))) %>% mutate(characteristic = glue('    {characteristic}')) %>% mutate(variable = var_labeller(variable))

# output repeat
rr_df_combined_clean_RR_repeat <- rr_df_combined_clean_RR %>% filter(prescribing_mode == 'repeat_amr')
rr_df_combined_clean_RR_repeat <- rr_df_combined_clean_RR_repeat %>% filter(year == '2020') %>% inner_join((rr_df_combined_clean_RR_repeat %>% filter(year == '2021')), by = c('prescribing_mode', 'variable', 'characteristic'), suffix = c('_2020', '_2021'))
rr_df_combined_clean_RR_repeat <- rr_df_combined_clean_RR_repeat %>% select(-c(year_2020, year_2021, prescribing_mode))
rr_df_combined_clean_RR_repeat <- rr_df_combined_clean_RR_repeat %>% 
  mutate(estimate_2020 = ifelse(is.na(estimate_2020), NA, glue('{estimate_2020} [{lower_2020}, {upper_2020}]')),
         estimate_2021 = ifelse(is.na(estimate_2021), NA, glue('{estimate_2021} [{lower_2021}, {upper_2021}]')),
         log_estimate_2020 = ifelse(is.na(log_estimate_2020), NA, glue('{log_estimate_2020} [{log_lower_2020}, {log_upper_2020}]')),
         log_estimate_2021 = ifelse(is.na(log_estimate_2021), NA, glue('{log_estimate_2021} [{log_lower_2021}, {log_upper_2021}]'))) %>%
  select(-c(lower_2020, upper_2020, lower_2021, upper_2021, log_lower_2020, log_upper_2020, log_lower_2021, log_upper_2021))

rr_df_combined_clean_RR_repeat_final <- table_1_format(rr_df_combined_clean_RR_repeat)

write.csv(rr_df_combined_clean_RR_repeat_final, file = 'output/tables/demographics_table_risk_ratios_clean_repeat.csv', row.names = FALSE, na = '')

# output non-repeat
rr_df_combined_clean_RR_non_repeat <- rr_df_combined_clean_RR %>% filter(prescribing_mode == 'non_repeat_amr')
rr_df_combined_clean_RR_non_repeat <- rr_df_combined_clean_RR_non_repeat %>% filter(year == '2020') %>% inner_join((rr_df_combined_clean_RR_non_repeat %>% filter(year == '2021')), by = c('prescribing_mode', 'variable', 'characteristic'), suffix = c('_2020', '_2021'))
rr_df_combined_clean_RR_non_repeat <- rr_df_combined_clean_RR_non_repeat %>% select(-c(year_2020, year_2021, prescribing_mode))
rr_df_combined_clean_RR_non_repeat <- rr_df_combined_clean_RR_non_repeat %>% 
  mutate(estimate_2020 = ifelse(is.na(estimate_2020), NA, glue('{estimate_2020} [{lower_2020}, {upper_2020}]')),
         estimate_2021 = ifelse(is.na(estimate_2021), NA, glue('{estimate_2021} [{lower_2021}, {upper_2021}]')),
         log_estimate_2020 = ifelse(is.na(log_estimate_2020), NA, glue('{log_estimate_2020} [{log_lower_2020}, {log_upper_2020}]')),
         log_estimate_2021 = ifelse(is.na(log_estimate_2021), NA, glue('{log_estimate_2021} [{log_lower_2021}, {log_upper_2021}]'))) %>%
  select(-c(lower_2020, upper_2020, lower_2021, upper_2021, log_lower_2020, log_upper_2020, log_lower_2021, log_upper_2021))

rr_df_combined_clean_RR_non_repeat_final <- table_1_format(rr_df_combined_clean_RR_non_repeat)

write.csv(rr_df_combined_clean_RR_non_repeat_final, file = 'output/tables/demographics_table_risk_ratios_clean_non_repeat.csv', row.names = FALSE, na = '')


# ---------------------------
# re-format for final repeat/non-repeat demographic tables
#rr_df_combined %>% glimpse()
rr_df_combined_clean <- rr_df_combined %>% select(year, prescribing_mode, variable, characteristic, population, n_prescribed, incidence_per_1000, incidence_percent_change)

rr_df_combined_clean <- rr_df_combined_clean %>% mutate(variable2 = ifelse(str_starts(variable, 'age_'), 'age', variable)) %>%
  group_by(year, prescribing_mode, variable2) %>%
  mutate(population_perc = formatC((population / sum(population))*100, format = 'f', digits = 2)) %>%
  mutate(prescribed_perc = formatC((n_prescribed / sum(n_prescribed))*100, format = 'f', digits = 2)) %>% ungroup() %>%
  
  group_by(year, prescribing_mode, variable) %>%
  mutate(population_sex_perc = ifelse(variable2 == 'age', formatC((population / sum(population))*100, format = 'f', digits = 2), NA)) %>%
  mutate(prescribed_sex_perc = ifelse(variable2 == 'age', formatC((n_prescribed / sum(n_prescribed))*100, format = 'f', digits = 2), NA)) %>% ungroup() %>%
  
  mutate(population = formatC(population, big.mark = ','), n_prescribed = formatC(n_prescribed, big.mark = ',')) %>%
  mutate(population = ifelse(is.na(population_sex_perc), glue('{population} ({population_perc})'), glue('{population} ({population_perc}) ({population_sex_perc})'))) %>%
  mutate(n_prescribed = ifelse(is.na(prescribed_sex_perc), glue('{n_prescribed} ({prescribed_perc})'), glue('{n_prescribed} ({prescribed_perc}) ({prescribed_sex_perc})'))) %>%
  
  mutate(incidence_per_1000 = formatC(incidence_per_1000, format = "f", digits = 2), incidence_percent_change = formatC(incidence_percent_change, format = "f", digits = 2)) %>%
  select(-c(population_perc, population_sex_perc, prescribed_perc, prescribed_sex_perc, variable2)) %>%
  mutate(characteristic = glue('    {characteristic}')) %>% mutate(variable = var_labeller(variable))

# output repeat
rr_df_combined_clean_repeat <- rr_df_combined_clean %>% filter(prescribing_mode == 'repeat_amr')
rr_df_combined_clean_repeat <- rr_df_combined_clean_repeat %>% filter(year == '2020') %>% inner_join((rr_df_combined_clean_repeat %>% filter(year == '2021')), by = c('prescribing_mode', 'variable', 'characteristic'), suffix = c('_2020', '_2021'))
rr_df_combined_clean_repeat <- rr_df_combined_clean_repeat %>% select(-c(year_2020, year_2021, prescribing_mode, incidence_percent_change_2020))

rr_df_combined_clean_repeat_final <- table_1_format(rr_df_combined_clean_repeat)

write.csv(rr_df_combined_clean_repeat_final, file = 'output/tables/demographics_table_clean_repeat.csv', row.names = FALSE, na = '')

# output non-repeat
rr_df_combined_clean_non_repeat <- rr_df_combined_clean %>% filter(prescribing_mode == 'non_repeat_amr')
rr_df_combined_clean_non_repeat <- rr_df_combined_clean_non_repeat %>% filter(year == '2020') %>% inner_join((rr_df_combined_clean_non_repeat %>% filter(year == '2021')), by = c('prescribing_mode', 'variable', 'characteristic'), suffix = c('_2020', '_2021'))
rr_df_combined_clean_non_repeat <- rr_df_combined_clean_non_repeat %>% select(-c(year_2020, year_2021, prescribing_mode, incidence_percent_change_2020))

rr_df_combined_clean_non_repeat_final <- table_1_format(rr_df_combined_clean_non_repeat)

write.csv(rr_df_combined_clean_non_repeat_final, file = 'output/tables/demographics_table_clean_non_repeat.csv', row.names = FALSE, na = '')


# ---------------------------
# PLOTS

# ---------------------------
# forest plot of risk ratios

rr_df_combined_forest <- rr_df_combined %>% mutate(characteristic = ifelse(characteristic %in% refs, glue('{characteristic} (reference)'), characteristic)) %>% mutate(characteristic = glue('    {characteristic}')) %>% select(year, prescribing_mode, variable, characteristic, log_estimate, log_lower, log_upper) %>% mutate('Patient group' = ifelse(prescribing_mode == 'repeat_amr', glue('Jan {year}, prescribed repeat'), glue('Jan {year}, prescribed non-repeat'))) %>% select(-c(year, prescribing_mode)) %>% mutate(variable = var_labeller(variable))

rr_df_combined_forest_wide <- rr_df_combined_forest %>%
  group_by(`Patient group`) %>%
  mutate(id = row_number()) %>%
  tidyr::pivot_wider(names_from = `Patient group`, values_from = log_estimate:log_upper) %>%
  select(-id)
# for reason for id col, see http://ritsokiguess.site/docs/2020/07/09/another-tidying-problem/

rr_df_combined_forest_wide_set1 <- rr_df_combined_forest_wide %>% filter(!variable %in% c('IMD quintile', 'England region'))
rr_df_combined_forest_wide_set1 <- table_1_format(rr_df_combined_forest_wide_set1)
rr_df_combined_forest_wide_set2 <- rr_df_combined_forest_wide %>% filter(variable %in% c('IMD quintile', 'England region'))
rr_df_combined_forest_wide_set2 <- table_1_format(rr_df_combined_forest_wide_set2)


tm <- forest_theme(base_size = 13,
                   base_family = "sans",
                   refline_lty = "dashed",
                   ci_pch = c(23, 23, 21, 21),  # symbols: diamonds and circles
                   ci_col = c(paired_blues, paired_reds),
                   legend_name = "Patient group",
                   legend_value = c("Jan 2020, prescribed non-repeat", "Jan 2021, prescribed non-repeat", "Jan 2020, prescribed repeat", "Jan 2021, prescribed repeat"),
                   legend_position = "right") 

p1_out <- suppressWarnings(plot_forest(rr_df_combined_forest_wide_set1))
p1 <- p1_out$plot
p2 <- suppressWarnings(plot_forest(rr_df_combined_forest_wide_set2))$plot
p2$widths <- p1_out$widths

ggsave("output/plots/demographics_forestplot_main.png", plot = p1, dpi = 600, width = 13, height = 14)
ggsave("output/plots/demographics_forestplot_supplementary.png", plot = p2, dpi = 600, width = 13, height = 7)
ggsave("output/plots/demographics_forestplot_main.pdf", plot = p1, width = 13, height = 14)
ggsave("output/plots/demographics_forestplot_supplementary.pdf", plot = p2, width = 13, height = 7)

# make ggplot legend (option to manually replace default forestploter legend)
p_legend_plot <- ggplot(rr_df_combined_forest %>% filter(variable == "Age (female patients)"), aes(x = characteristic, y = log_estimate, colour = `Patient group`, fill = `Patient group`, shape = `Patient group`)) + geom_point() +
  scale_colour_manual(values = c(paired_blues, paired_reds)) +
  scale_fill_manual(values = c(paired_blues, paired_reds)) +
  scale_shape_manual(values = c(23, 23, 21, 21)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE), colour = guide_legend(nrow = 2, byrow = TRUE)) + theme(legend.position = "top", legend.title = element_text(size = 15), legend.text = element_text(size = 13))
p_legend <- get_legend(p_legend_plot)
ggsave("output/plots/demographics_forestplot_legend.png", plot = p_legend, dpi = 600, width = 8, height = 2)
ggsave("output/plots/demographics_forestplot_legend.pdf", plot = p_legend, width = 8, height = 2)

# ---------------------------
# plot percent change for repeat and non repeat in Jan 2021 cohort vs Jan 2020
rr_df_combined_2021 <- rr_df_combined %>% filter(year == 2021)
#rr_df_combined_2021 %>% glimpse()

# calculate total incidence per 1000 (for lines on plot); filter on an (arbitrary) variable and calculate total incidence
incidence_total_df <- rr_df_combined %>% filter(variable == 'care_home') %>% group_by(year, prescribing_mode) %>% summarise(population = sum(population), n_prescribed = sum(n_prescribed), .groups = 'drop') %>% mutate(incidence_per_1000 = (n_prescribed / population) * 1000)

repeat_incidence_pct_change <- incidence_total_df %>% filter(prescribing_mode == 'repeat_amr') %>% summarise(incidence_percent_change_repeat = ((incidence_per_1000[year == 2021] - incidence_per_1000[year == 2020])/incidence_per_1000[year == 2020])*100) %>% pull(incidence_percent_change_repeat)
non_repeat_incidence_pct_change <- incidence_total_df %>% filter(prescribing_mode == 'non_repeat_amr') %>% summarise(incidence_percent_change_non_repeat = ((incidence_per_1000[year == 2021] - incidence_per_1000[year == 2020])/incidence_per_1000[year == 2020])*100) %>% pull(incidence_percent_change_non_repeat)

# plot percent change
vars <- unique(rr_df_combined_2021$variable)
ylabs <- c("Age (female patients)", "Age (male patients)", "Residence", "Ethnicity", "IMD quintile", "England region")
xmin <- round_any(min(rr_df_combined_2021$incidence_percent_change), 5, f = floor)
xmax <- round_any(max(rr_df_combined_2021$incidence_percent_change), 5, f = ceiling)

plotlist_set1 <- list()
plotlist_set2 <- list()
for (i in 1:length(vars)) {
  var <- vars[i]
  ylab <- ylabs[i]
  if (var %in% c("imd", "region")) {
    p <- plot_pc(rr_df_combined_2021, var, ylab, xmin, xmax, non_repeat_incidence_pct_change, repeat_incidence_pct_change)
    if (var %in% c("imd")) {
      p <- p + theme(axis.title.x = element_blank())
    }
    plotlist_set2[[var]] <- p
  } else {
    p <- plot_pc(rr_df_combined_2021, var, ylab, xmin, xmax, non_repeat_incidence_pct_change, repeat_incidence_pct_change)
    if (var %in% c("age_female", "age_male", "care_home")) {
      p <- p + theme(axis.title.x = element_blank())
    }
    plotlist_set1[[var]] <- p
  }
}

# combine plots using patchwork
layout <- "
A
A
A
A
A
A
A
B
B
B
B
B
B
B
C
C
C
C
C
D
D
D
D
D
D
D
D
D
D
D
D
D
D
D
"
p_combined_set1 <- wrap_plots(plotlist_set1) + plot_annotation(tag_levels = NULL) + plot_layout(ncol = 1, widths = c(1,1), byrow = FALSE, design = layout, guides = 'collect') & theme(legend.position = 'top') & guides(colour=guide_legend(nrow=2,byrow=TRUE))

layout <- "
A
A
A
A
A
A
B
B
B
B
B
B
B
B
B
B
"
p_combined_set2 <- wrap_plots(plotlist_set2) + plot_annotation(tag_levels = NULL) + plot_layout(ncol = 1, widths = c(1,1), byrow = FALSE, design = layout, guides = 'collect') & theme(legend.position = 'top') & guides(colour=guide_legend(nrow=2,byrow=TRUE))


ggsave("output/plots/demographics_pct_change_combined_main.png", plot = p_combined_set1, dpi = 600, width = 6, height = 9)
ggsave("output/plots/demographics_pct_change_combined_supplementary.png", plot = p_combined_set2, dpi = 600, width = 6, height = 4.5)
ggsave("output/plots/demographics_pct_change_combined_main.pdf", plot = p_combined_set1, width = 6, height = 9)
ggsave("output/plots/demographics_pct_change_combined_supplementary.pdf", plot = p_combined_set2, width = 6, height = 4.5)
