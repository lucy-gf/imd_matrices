
## CONTACT MATRICES ASSORTATIVITY ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(ggplot2)
suppressPackageStartupMessages(library(viridis, warn.conflicts = FALSE))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","fitted_matrs.csv"),
  file.path("data", "census","imd_age.csv")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN DATA ####

fitted <- data.table(suppressWarnings(read_csv(.args[1], show_col_types = F)))[bootstrap_index != 'bootstrap_index',]

imd_age <- data.table(read_csv(.args[2], show_col_types = F))
imd_age$age <- gsub('.{1}$','',gsub('----','-',gsub('-----','',gsub("\\D", "-", imd_age$age))))
imd_age <- imd_age %>% 
  mutate(age = case_when(age == '4' ~ '0-4',
                         age == '75' ~ '75+',
                         T ~ age)) %>% 
  group_by(imd_q) %>% mutate(tot_pop_imd = sum(population)) %>% 
  ungroup() %>% 
  mutate(tot_pop = sum(population),
         prop_imd = population/tot_pop_imd,
         prop = population/tot_pop)
imd_age$age <- factor(imd_age$age, levels = age_labels)

#### BALANCE ####

balanced_matr <- balancing_fcn(
  data = fitted,
  age_structure = imd_age
)

#### SUMMARISE ####

add_age <- imd_age %>% 
  group_by(age) %>% 
  mutate(prop = prop/sum(prop)) %>% 
  select(imd_q, age, prop)
colnames(add_age) <- c('c_imd_q','c_age_group','c_imd_prop')

agg <- balanced_matr %>%
  group_by(bootstrap_index, p_age_group, c_age_group, p_imd_q) %>% 
  mutate(total_contacts = sum(n)) %>% 
  left_join(add_age, by = c('c_imd_q', 'c_age_group')) %>% 
  ungroup() %>% 
  mutate(prop_of_contacts = n/(total_contacts*c_imd_prop))

agg$p_age_group <- factor(agg$p_age_group,
                          levels = (age_labels))
agg$c_age_group <- factor(agg$c_age_group,
                          levels = rev(age_labels))
agg$p_imd_q <- factor(agg$p_imd_q,
                          levels = (1:5))
agg$c_imd_q <- factor(agg$c_imd_q,
                          levels = rev(1:5))

agg %>% 
  filter(!is.na(prop_of_contacts)) %>% 
  ggplot() + 
  geom_histogram(aes(prop_of_contacts, fill = p_age_group, group = p_age_group), bins = 50) +
  facet_grid(c_imd_q ~ p_imd_q) +
  theme_bw() +
  geom_vline(xintercept = 1, lty = 2) + 
  labs(x = 'Proportion of contacts, vs expected',
       y = 'Count', fill = 'Age')

agg_props <- agg %>% 
  filter(!is.na(prop_of_contacts)) %>% 
  group_by(p_imd_q, c_imd_q) %>% 
  summarise(med = median(prop_of_contacts),
            l50 = quantile(prop_of_contacts, 0.25),
            u50 = quantile(prop_of_contacts, 0.75))

agg_props$c_imd_q <- factor(agg_props$c_imd_q, 
                            levels = 1:5)

agg_props %>% 
  ggplot(aes(x = c_imd_q, col = as.factor(c_imd_q), group = c_imd_q)) + 
  geom_point(aes(y = med), size = 2) + 
  geom_errorbar(aes(ymin = l50, ymax = u50), lwd = 0.8) +
  theme_bw() + 
  facet_grid(. ~ p_imd_q) + 
  labs(col = 'Contact IMD quintile', x = 'Participant IMD quintile', 
       y = 'Proportion, vs expected proportion') +
  geom_hline(yintercept = 1, lty = 2) + ylim(c(0,3)) + 
  scale_color_manual(values = imd_quintile_colors) +
  ggtitle('Proportion of contacts, compared to expected if random mixing, with 50% CI')

ggsave(here::here('output','figures','cont_matrs','assortativity','assortativity.png'), width = 12, height = 6)

agg_props <- agg %>% 
  filter(!is.na(prop_of_contacts)) %>% 
  group_by(p_imd_q, c_imd_q, c_age_group) %>% 
  summarise(med = median(prop_of_contacts),
            l50 = quantile(prop_of_contacts, 0.25),
            u50 = quantile(prop_of_contacts, 0.75))

agg_props$c_age_group <- factor(agg_props$c_age_group, 
                                  levels = age_labels)
agg_props$c_imd_q <- factor(agg_props$c_imd_q, 
                            levels = 1:5)

agg_props %>% 
  ggplot(aes(x = interaction(c_age_group, c_imd_q), col = as.factor(c_imd_q), group = c_imd_q)) + 
  geom_point(aes(y = med), size = 2) + 
  geom_errorbar(aes(ymin = l50, ymax = u50), lwd = 0.8, alpha = 0.6) +
  theme_bw() + 
  facet_grid(. ~ p_imd_q) + 
  labs(col = 'Contact IMD quintile', x = '', 
       y = 'Proportion, vs expected proportion') +
  geom_hline(yintercept = 1, lty = 2) + ylim(c(0,3)) + 
  scale_color_manual(values = imd_quintile_colors) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  ggtitle('Proportion of contacts in each age group, compared to expected if random mixing, with 50% CI')

ggsave(here::here('output','figures','cont_matrs','assortativity','assortativity_by_c_age.png'), width = 12, height = 6)




