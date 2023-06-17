pacman::p_load(tidyverse, data.table, scales, lubridate, janitor, glue, patchwork, Hmisc)
theme_set(theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

paired_blues <- c("#A6CEE3", "#1F78B4")  # light blue, dark blue
paired_reds <- c("#FB9A99", "#E31A1C")  # light red, dark red

dir.create(here::here("output/tables"), showWarnings = FALSE)
dir.create(here::here("output/plots"), showWarnings = FALSE)

# functions
clinical_condition_labeller <- function(x) {
  x <- str_replace(x, 'has_comorbidity_cancer_immunosuppression', 'Cancer immunosuppression')
  x <- str_replace(x, 'has_comorbidity_copd', 'COPD')
  x <- str_replace(x, 'has_comorbidity_sickle_cell', 'Sickle cell disease')
  x <- str_replace(x, 'has_comorbidity_splenectomy', 'Splenectomy')
  x <- str_replace(x, 'has_indication_acne', 'Acne')
  x <- str_replace(x, 'has_indication_copd_infection', 'COPD excacerbation &\nLower respiratory tract infection')
  x <- str_replace(x, 'has_indication_dental_infection', 'Dental infection')
  x <- str_replace(x, 'has_indication_otitis_media', 'Otitis media')
  x <- str_replace(x, 'has_indication_ssti', 'Skin & soft tissue infection')
  x <- str_replace(x, 'has_indication_uti', 'Urinary tract infection')
  x <- str_replace(x, 'has_any_clinical_condition', 'Any clinical condition')
  x <- str_replace(x, 'all patients', 'All patients')
}

amr_class_labeller <- function(x) {
  x <- str_replace(x, 'sum_', '')
  x <- str_replace(x, 'beta_lactams', 'beta-lactams')
  x <- str_replace_all(x, '_', ' ')
  x <- Hmisc::capitalize(x)
}


# read and combine clinical conditions/amr class data
# data is number of patients receiving repeat and non-repeat antibiotics; 1) all antibiotics (sum_amr) 2) broken down by antibiotic class. Patient counts are given for 1) all patients 2) subset of patient with a given clinical condition
repeat_amr_2020 <- read.csv(here::here("output", "n_repeat_amr_2020-01-01.csv"), header = TRUE)
repeat_amr_2020$year <- '2020'
repeat_amr_2020$prescribing_mode <- 'repeat'

non_repeat_amr_2020 <- read.csv(here::here("output", "n_non_repeat_amr_2020-01-01.csv"), header = TRUE)
non_repeat_amr_2020$year <- '2020'
non_repeat_amr_2020$prescribing_mode <- 'non-repeat'

repeat_amr_2021 <- read.csv(here::here("output", "n_repeat_amr_2021-01-01.csv"), header = TRUE)
repeat_amr_2021$year <- '2021'
repeat_amr_2021$prescribing_mode <- 'repeat'

non_repeat_amr_2021 <- read.csv(here::here("output", "n_non_repeat_amr_2021-01-01.csv"), header = TRUE)
non_repeat_amr_2021$year <- '2021'
non_repeat_amr_2021$prescribing_mode <- 'non-repeat'


amr_df <- bind_rows(repeat_amr_2020, non_repeat_amr_2020, repeat_amr_2021, non_repeat_amr_2021)
#amr_df %>% glimpse()
amr_df <- amr_df %>% replace(is.na(.), 0)

# save
write.csv(amr_df, file = here::here('output/tables/amr_clinical_conditions.csv'), row.names = FALSE)


# ---------------------------
# FREQUENCY OF CLINICAL CONDITIONS

# ---------------------------
# relative frequency of clinical conditions within each patient group

# calculate relative frequency
amr_df_relfreq <- amr_df %>% mutate(`Patient group` = glue('Jan {year}, prescribed {prescribing_mode}')) %>% group_by(`Patient group`) %>% mutate(percentage = (sum_amr / first(sum_amr)) * 100) %>% ungroup() %>% select(year, prescribing_mode, `Patient group`, clinical.condition, sum_amr, percentage) %>% filter(clinical.condition != 'all patients')

# append clinical condition type
amr_df_relfreq <- amr_df_relfreq %>% mutate(clinical.condition.type = case_when(clinical.condition == 'has_any_clinical_condition' ~ 'Any clinical condition',
                                                              str_starts(clinical.condition, 'has_comorbidity') ~ 'Comorbidities',
                                                              str_starts(clinical.condition, 'has_indication') ~ 'Indications'))

# order factor levels
amr_df_relfreq$`Patient group` <- factor(amr_df_relfreq$`Patient group`, levels = c("Jan 2020, prescribed non-repeat", "Jan 2021, prescribed non-repeat", "Jan 2020, prescribed repeat", "Jan 2021, prescribed repeat"))

clinical_condition_order <- amr_df_relfreq %>% filter(`Patient group` == 'Jan 2021, prescribed repeat') %>% arrange(percentage) %>% pull(clinical.condition)
amr_df_relfreq$clinical.condition <- factor(amr_df_relfreq$clinical.condition, levels = rev(clinical_condition_order))

# plot
condition_types <- sort(unique(amr_df_relfreq$clinical.condition.type)) 
ymaxs <- c(60, 25, 25)
annotate_x_positions <- c(NA, 2.5, 3.5)
plotlist_relfreq <- list()
for (i in 1:length(condition_types)) {
  condition_type <- condition_types[i]
  ylab <- 'Relative frequency of clinical\ncondition in prescribed patients\n(percentage)'
  ymax <- ymaxs[i]
  annotate_xposition <- annotate_x_positions[i]
  tag_lab <- ''
  if (condition_type == 'Any clinical condition') {
    tag_lab <- 'A'
  }
  amr_df_relfreq_cond_type <- amr_df_relfreq %>% filter(clinical.condition.type == condition_type)
  p <- ggplot(amr_df_relfreq_cond_type, aes(x = clinical.condition, y = percentage, fill = `Patient group`)) + geom_col(position = position_dodge(), width = 0.7) +
    scale_y_continuous(breaks = seq(from = 0, to = ymax, by = 5), limits = c(0, ymax), expand = c(0, 0)) +
    scale_fill_manual(values = c(paired_blues, paired_reds)) + labs(x = "", y = ylab, tag = tag_lab) +
    theme(plot.margin = unit(c(5.5,5.5,5.5,25), "pt"), axis.text.x = element_text(angle=45,vjust=1,hjust=1), plot.tag.position = "topleft") +
    guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
    scale_x_discrete(labels = as_labeller(clinical_condition_labeller)) +
    geom_text(aes(label = ifelse(percentage < 1, formatC(percentage, format = 'f', digits = 2), NA)), hjust = -0.5, vjust = 0.5, position = position_dodge(0.7), size = 2.25, angle = 90)
  if (condition_type %in% c("Comorbidities", "Indications")) {
    p <- p + annotate("text", label = condition_type, x = annotate_xposition, y = 24, size = 3.5) + theme(axis.title.y = element_blank()) +
      theme(plot.margin = unit(c(10,5.5,5.5,5.5), "pt"))
  }
  plotlist_relfreq[[condition_type]] <- p
}

layout <- "ABBBBCCCCCC"
p_cc_relfreq_combined <- wrap_plots(plotlist_relfreq) + plot_annotation(tag_levels = NULL) + plot_layout(nrow = 1, widths = c(1,1), byrow = TRUE, design = layout, guides = 'collect') & theme(legend.position = 'top')
gg_axis <- cowplot::get_plot_component(ggplot() + labs(x = "Clinical condition"), "xlab-b")
p_cc_relfreq_combined <- p_cc_relfreq_combined / gg_axis + plot_layout(heights = c(40, 1))

ggsave("clinical_conditions.png", plot = p_cc_relfreq_combined, path = here::here("output/plots"), dpi = 600, height = 6, width = 8)
ggsave("clinical_conditions.pdf", plot = p_cc_relfreq_combined, path = here::here("output/plots"), height = 6, width = 8)



# ---------------------------
# percent change 2021 vs 2020 for repeat and non-repeat

amr_df_pctchange_2021 <- amr_df_relfreq %>% group_by(prescribing_mode, clinical.condition) %>% mutate(percent_change = ifelse(year == 2020, NA, (((sum_amr[year == 2021] - sum_amr[year == 2020]) / sum_amr[year == 2020]))) * 100) %>% ungroup() %>% filter(year == 2021)
amr_df_pctchange_2021$prescribing_mode <- factor(amr_df_pctchange_2021$prescribing_mode, levels = c('non-repeat', 'repeat'))

plotlist_pctchange <- list()
for (i in 1:length(condition_types)) {
  condition_type <- condition_types[i]
  annotate_xposition <- annotate_x_positions[i]
  tag_lab <- ''
  ylab <- 'Percent change in prescribed\npatients with clinical condition\n(Jan 2021 vs Jan 2020)'
  if (condition_type == 'Any clinical condition') {
    tag_lab <- 'B'
  }
  amr_df_pctchange_2021_cond_type <- amr_df_pctchange_2021 %>% filter(clinical.condition.type == condition_type)
  p <- ggplot(amr_df_pctchange_2021_cond_type, aes(x = clinical.condition, y = percent_change, group = prescribing_mode, colour = prescribing_mode, shape = prescribing_mode)) + geom_hline(yintercept = 0, linetype='solid', colour='light grey', linewidth = 0.3) + geom_point(size = 2) +  labs(x = "", y = ylab, tag = tag_lab) +
    scale_colour_manual(values = c(paired_blues[2], paired_reds[2]), labels = c('prescribed non-repeat', 'prescribed repeat'), name = 'Patient group') +
    scale_shape_manual(values = c(23, 21), labels = c('prescribed non-repeat','prescribed repeat'), name = 'Patient group') +
    theme(panel.grid.major.x = element_line(colour = 'light grey', linetype = 'dashed'), plot.margin = unit(c(5.5,5.5,5.5,25), "pt"),
          axis.text.x = element_text(angle=45,vjust=1,hjust=1), plot.tag.position = "topleft") +
    scale_y_continuous(breaks = seq(from = -80, to = 20, by = 20), limits = c(-80, 20)) + scale_x_discrete(labels = as_labeller(clinical_condition_labeller))
  if (condition_type %in% c("Comorbidities", "Indications")) {
    p <- p + annotate("text", label = condition_type, x = annotate_xposition, y = 20, size = 3.5) + theme(axis.title.y = element_blank()) +
      theme(plot.margin = unit(c(10,5.5,5.5,5.5), "pt"))
  }
  plotlist_pctchange[[condition_type]] <- p
}

layout <- "ABBBBCCCCCC"
p_cc_pctchange <- wrap_plots(plotlist_pctchange) + plot_annotation(tag_levels = NULL) + plot_layout(nrow = 1, widths = c(1,1), byrow = TRUE, design = layout, guides = 'collect') & theme(legend.position = 'top')

gg_axis <- cowplot::get_plot_component(ggplot() + labs(x = "Clinical condition"), "xlab-b")
p_cc_pctchange_combined <- p_cc_pctchange / gg_axis + plot_layout(heights = c(40, 1))

ggsave("clinical_conditions_pct_change.png", plot = p_cc_pctchange_combined, path = here::here("output/plots"), dpi = 600, height = 6, width = 8)
ggsave("clinical_conditions_pct_change.pdf", plot = p_cc_pctchange_combined, path = here::here("output/plots"), height = 6, width = 8)


# ---------------------------
# combine relative frequency and percent change plots
# add plot_spacer between plots
#p_cc_combined <- wrap_plots(list(p_cc_relfreq_combined, plot_spacer(), p_cc_pctchange_combined), ncol = 1, heights = c(60,1,60))
p_cc_combined <- wrap_plots(list(p_cc_relfreq_combined, p_cc_pctchange_combined), ncol = 1)
ggsave("clinical_conditions_combined.png", plot = p_cc_combined, path = here::here("output/plots"), dpi = 600, height = 9.5, width = 7.5)
ggsave("clinical_conditions_combined.pdf", plot = p_cc_combined, path = here::here("output/plots"), height = 9.5, width = 7.5)


# ---------------------------
# FREQUENCY OF PRESCRIBING ACROSS AMR CLASSES - ALL PATIENTS

# ---------------------------
# relative frequency of antibiotic classes

amr_df_amr <- amr_df %>% mutate(`Patient group` = glue('Jan {year}, prescribed {prescribing_mode}')) %>% select(-sum_amr)
# order factor levels
amr_df_amr$`Patient group` <- factor(amr_df_amr$`Patient group`, levels = c("Jan 2020, prescribed non-repeat", "Jan 2021, prescribed non-repeat", "Jan 2020, prescribed repeat", "Jan 2021, prescribed repeat"))

# split into class and tetracycline subclass data
amr_df_class <- amr_df_amr %>% select(-c(sum_demeclocycline, sum_doxycycline, sum_lymecycline, sum_minocycline, sum_oxytetracycline, sum_tetracycline)) %>% pivot_longer(starts_with('sum_'), names_to = 'amr_class', values_to = 'count') %>% group_by(`Patient group`, clinical.condition) %>% mutate(percentage = (count / sum(count)) * 100) %>% ungroup()

amr_df_class_tetracyclines <- amr_df_amr %>% select(clinical.condition, year, prescribing_mode, `Patient group`, sum_demeclocycline, sum_doxycycline, sum_lymecycline, sum_minocycline, sum_oxytetracycline, sum_tetracycline) %>% pivot_longer(starts_with('sum_'), names_to = 'amr_class', values_to = 'count') %>% group_by(`Patient group`, clinical.condition) %>% mutate(percentage = (count / sum(count)) * 100) %>% ungroup()

# set class and tetracycline subclass order based on relative frequency of 2021 repeat prescribing
amr_repeat_order <- amr_df_class %>% filter(prescribing_mode == 'repeat', year == '2021', clinical.condition == 'all patients') %>% arrange(desc(percentage)) %>% pull(amr_class)
amr_df_class$amr_class <- factor(amr_df_class$amr_class, levels = amr_repeat_order)

amr_tetracyclines_repeat_order <- amr_df_class_tetracyclines %>% filter(prescribing_mode == 'repeat', year == '2021', clinical.condition == 'all patients') %>% arrange(desc(percentage)) %>% pull(amr_class)
amr_df_class_tetracyclines$amr_class <- factor(amr_df_class_tetracyclines$amr_class, levels = amr_tetracyclines_repeat_order)

# plot class breakdown for all patients (not broken down by clinical condition subsets)
p_class_rel_freq <- ggplot((amr_df_class %>% filter(clinical.condition == 'all patients')), aes(x = amr_class, y = percentage, fill = `Patient group`)) + geom_col(position = position_dodge(), width = 0.7) +
  scale_y_continuous(breaks = seq(from = 0, to = 45, by = 5), limits = c(0, 45), expand = c(0, 0)) +
  scale_fill_manual(values = c(paired_blues, paired_reds)) + labs(x = 'BNF antibiotic class', y = 'Relative frequency of prescribed\nantibiotic class (percentage)') +
  theme(plot.margin = unit(c(5.5,5.5,5.5,25), "pt"), axis.text.x = element_text(angle=45,vjust=1, hjust=1), legend.position = "top") +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  scale_x_discrete(labels = as_labeller(amr_class_labeller)) +
  geom_text(aes(label = ifelse(percentage < 1, formatC(percentage, format = 'f', digits = 2), NA)), hjust = -0.5, vjust = 0.5, position = position_dodge(0.7), size = 2.25, angle = 90)


ggsave("amr_class_rel_freq.png", plot = p_class_rel_freq, path = here::here("output/plots"), dpi = 600, height = 6, width = 8)
ggsave("amr_class_rel_freq.pdf", plot = p_class_rel_freq, path = here::here("output/plots"), height = 6, width = 8)


# ---------------------------
# frequency of antibiotic classes as a rate per 1000 rather than relative frequency

# first load population data
demographics_2020 <- read.csv(here::here("output", "demographics_table_2020-01-01.csv"), header = TRUE)
demographics_2020$year <- '2020'
demographics_2021 <- read.csv(here::here("output", "demographics_table_2021-01-01.csv"), header = TRUE)
demographics_2021$year <- '2021'
demographics_df <- bind_rows(demographics_2020, demographics_2021)
demographics_df <- demographics_df %>% filter(id == 'age_sex') %>% group_by(year) %>% summarise(population = sum(population), n_repeat_amr = sum(n_repeat_amr), n_non_repeat_amr = sum(n_non_repeat_amr))

pop_2020 <- demographics_df %>% filter(year == '2020') %>% pull(population)
pop_2021 <- demographics_df %>% filter(year == '2021') %>% pull(population)

# calculate per class rate per 100k
amr_df_class_subclass_rate <- amr_df %>% mutate(`Patient group` = glue('Jan {year}, prescribed {prescribing_mode}')) %>% select(-sum_amr) %>%
  filter(clinical.condition == 'all patients') %>%
  mutate(across(.cols = starts_with("sum_"), .fns = ~ ifelse(year == '2020', (.x / pop_2020)*100000, (.x / pop_2021)*100000), .names =  "prop_{.col}")) %>%
  select(-c(starts_with("sum_"), clinical.condition)) %>% rename_with(~str_replace(.x, '^prop_sum_', 'sum_'), .cols = starts_with('prop_sum_'))

# exclude subclass rates, pivot longer
amr_df_class_rate <- amr_df_class_subclass_rate %>% select(-c(sum_demeclocycline, sum_doxycycline, sum_lymecycline, sum_minocycline, sum_oxytetracycline, sum_tetracycline)) %>%
  pivot_longer(!c(`Patient group`, year, prescribing_mode), names_to = 'BNF antibiotic class', values_to = 'Prescribing per 100,000')

# order amr classes (most frequently repeat prescribed in 2020) and patient group
amr_class_order <- amr_df_class_rate %>% filter(`Patient group` == 'Jan 2021, prescribed repeat') %>% arrange(`Prescribing per 100,000`) %>% pull(`BNF antibiotic class`)
amr_df_class_rate$`BNF antibiotic class` <- factor(amr_df_class_rate$`BNF antibiotic class`, levels = rev(amr_class_order))

amr_df_class_rate$`Patient group` <- factor(amr_df_class_rate$`Patient group`, levels = c("Jan 2020, prescribed non-repeat", "Jan 2021, prescribed non-repeat", "Jan 2020, prescribed repeat", "Jan 2021, prescribed repeat"))
#amr_df_class_rate %>% filter(`Prescribing per 100,000` == 0) %>% arrange(`BNF antibiotic class`)  # no 0s

# table output
table_class_rate <- amr_df_class_rate %>% select(-c(year, prescribing_mode)) %>% pivot_wider(names_from = `Patient group`, values_from = `Prescribing per 100,000`) %>% mutate(`BNF antibiotic class` = amr_class_labeller(`BNF antibiotic class`)) %>% mutate(across(.cols = starts_with('Jan'), .fns = ~ formatC(.x, format = 'f', digits = 2, big.mark = ",")))
write.csv(table_class_rate, 'output/tables/amr_class_rate.csv', row.names = FALSE)

# plot
dodge_width <- 0.9
my_xmax <- max(as.numeric(as.factor(amr_df_class_rate$`BNF antibiotic class`)))
#min(amr_df_class_rate$`Prescribing per 100,000`)
#max(amr_df_class_rate$`Prescribing per 100,000`)

n_char <- length(unique(amr_df_class_rate$`BNF antibiotic class`))
p_class_rate <- ggplot(amr_df_class_rate, aes(x = `BNF antibiotic class`, y = `Prescribing per 100,000`, fill = `Patient group`, shape = `Patient group`)) +
  geom_point(size = 2, colour = "white", position = position_dodge(width = dodge_width)) +
  labs(y = 'Rate of prescribing per 100,000 patients') +
  scale_fill_manual(values = c(paired_blues, paired_reds)) +
  scale_shape_manual(values = c(23, 23, 21, 21)) +
  scale_x_discrete(labels = as_labeller(amr_class_labeller)) +
  theme(plot.margin = unit(c(5.5,5.5,5.5,25), "pt"), axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position = "top") +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  scale_y_log10(breaks = 10^(-1:4), labels = label_comma(accuracy = 1), expand = c(0, 0)) +
  annotate('rect', xmin = seq(from = 0.5, to = my_xmax), xmax = seq(from = 1.5, to = my_xmax + 0.5),
           ymin = 10^-1, ymax = 10^4, alpha = .1, fill = rep(c('white','grey'), n_char)[1:n_char])

ggsave("amr_class_rate.png", plot = p_class_rate, path = here::here("output/plots"), dpi = 600, height = 6, width = 8)
ggsave("amr_class_rate.pdf", plot = p_class_rate, path = here::here("output/plots"), height = 6, width = 8)


# ---------------------------
# percent change 2021 vs 2020 for repeat and non-repeat based on counts for all patients (not broken down by clinical condition subsets)
amr_df_class_2021 <- amr_df_class %>% filter(clinical.condition == 'all patients') %>% group_by(prescribing_mode, amr_class) %>% mutate(percent_change = ifelse(year == 2020, NA, (((count[year == 2021] - count[year == 2020]) / count[year == 2020]))) * 100) %>% ungroup() %>% filter(year == 2021)

# plot
p_amr_class_pct_change <- ggplot(amr_df_class_2021, aes(x = amr_class, y = percent_change, colour = prescribing_mode, shape = prescribing_mode)) + geom_hline(yintercept = 0, linetype='solid', colour='light grey', linewidth = 0.3) + geom_point(size = 2) +
  labs(x = 'BNF antibiotic class', y = 'Percent change in patients prescribed\nantibiotic class (Jan 2021 vs Jan 2020)') +
  scale_colour_manual(values = c(paired_blues[2], paired_reds[2]), labels = c('prescribed non-repeat','prescribed repeat'), name = 'Patient group') +
  scale_shape_manual(values = c(23, 21), labels = c('prescribed non-repeat','prescribed repeat'), name = 'Patient group') +
  scale_x_discrete(labels = as_labeller(amr_class_labeller)) +
  theme(plot.margin = unit(c(5.5,5.5,5.5,25), "pt"), legend.position = "top", axis.text.x = element_text(angle=45,vjust=1,hjust=1), panel.grid.major.x = element_line(colour = 'light grey', linetype = 'dashed'))

ggsave("amr_class_pct_change.png", plot = p_amr_class_pct_change, path = here::here("output/plots"), dpi = 600, height = 6, width = 8)
ggsave("amr_class_pct_change.pdf", plot = p_amr_class_pct_change, path = here::here("output/plots"), height = 6, width = 8)


# ---------------------------
# combine frequency (rate per 100k) and percent change plots
p_amr_class_combined <- wrap_plots(list(p_class_rate, p_amr_class_pct_change), ncol = 1) + plot_annotation(tag_levels = 'A')
ggsave("amr_class_combined.png", plot = p_amr_class_combined, path = here::here("output/plots"), dpi = 600, height = 9.5, width = 7.5)
ggsave("amr_class_combined.pdf", plot = p_amr_class_combined, path = here::here("output/plots"), height = 9.5, width = 7.5)


# ---------------------------
# FREQUENCY OF PRESCRIBING ACROSS AMR CLASSES - BY CLINICAL CONDITION

# ---------------------------
# plot class breakdown faceted by clinical condition
p_class_cc <- ggplot(amr_df_class, aes(x = amr_class, y = percentage, fill = `Patient group`)) + geom_col(position = position_dodge(), width = 0.6) +
  facet_wrap(~clinical.condition, ncol = 2, labeller = as_labeller(clinical_condition_labeller)) +
  scale_fill_manual(values = c(paired_blues, paired_reds), labels = c('Jan 2020, prescribed non-repeat','Jan 2021, prescribed non-repeat','Jan 2020, prescribed repeat','Jan 2021, prescribed repeat')) + labs(x = 'BNF antibiotic class', y = 'Relative frequency of prescribed antibiotic class (percentage)', fill = 'Patient group') +
  scale_x_discrete(labels = as_labeller(amr_class_labeller)) +
  theme(plot.margin = unit(c(5.5,5.5,5.5,25), "pt"), axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position = "top") +
  guides(fill = guide_legend(nrow=2, byrow=TRUE))

my_xmax <- max(as.numeric(as.factor(amr_df_class$amr_class)))
shading <- tibble(xmin = seq(from = 0.5, to = my_xmax, by = 2), xmax = seq(from = 1.5, to = my_xmax + 0.5, by = 2), ymin = -Inf, ymax = Inf)
p_class_cc <- p_class_cc +
  geom_rect(
    mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), 
    data = shading, 
    alpha = 0.1,
    fill = "grey",
    inherit.aes = FALSE
  )

ggsave("amr_class_clinical_conditions.png", plot = p_class_cc, path = here::here("output/plots"), dpi = 600, height = 12, width = 9)
ggsave("amr_class_clinical_conditions.pdf", plot = p_class_cc, path = here::here("output/plots"), height = 12, width = 9)



# ---------------------------
# plot tetracycline subclass breakdown faceted by clinical condition 
p_class_tetracyclines_cc <- ggplot(amr_df_class_tetracyclines, aes(x = amr_class, y = percentage, fill = `Patient group`)) + geom_col(position = position_dodge(), width = 0.6) +
  facet_wrap(~clinical.condition, ncol = 2, labeller = as_labeller(clinical_condition_labeller)) +
  scale_fill_manual(values = c(paired_blues, paired_reds), labels = c('Jan 2020, prescribed non-repeat','Jan 2021, prescribed non-repeat','Jan 2020, prescribed repeat','Jan 2021, prescribed repeat')) + labs(x = 'BNF tetracycline sub-class', y = 'Relative frequency of prescribed tetracycline sub-class (percentage)', fill = 'Patient group') +
  scale_x_discrete(labels = as_labeller(amr_class_labeller)) +
  theme(plot.margin = unit(c(5.5,5.5,5.5,25), "pt"), axis.text.x = element_text(angle=45, vjust=1, hjust=1), legend.position = "top") +
  guides(fill = guide_legend(nrow=2, byrow=TRUE))

my_xmax <- max(as.numeric(as.factor(amr_df_class_tetracyclines$amr_class)))
shading <- tibble(xmin = seq(from = 0.5, to = my_xmax, by = 2), xmax = seq(from = 1.5, to = my_xmax + 0.5, by = 2), ymin = -Inf, ymax = Inf)
p_class_tetracyclines_cc <- p_class_tetracyclines_cc +
  geom_rect(
    mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax), 
    data = shading, 
    alpha = 0.1,
    fill = "grey",
    inherit.aes = FALSE
  )

ggsave("amr_class_tetracyclines_clinical_conditions.png", plot = p_class_tetracyclines_cc, path = here::here("output/plots"), dpi = 600, height = 10, width = 10)
ggsave("amr_class_tetracyclines_clinical_conditions.pdf", plot = p_class_tetracyclines_cc, path = here::here("output/plots"), height = 10, width = 10)
