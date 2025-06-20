
#### CRUDE CONTACT MATRICES ####

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2)
suppressMessages(library(viridis))

# source colors
source(file.path("scripts", "setup", "colors.R"))
# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","connect_prob_pcd1age.rds"),
  file.path("data", "connect","connect_contacts_formatted.rds"),
  'prob_pcd1age',
  file.path("output", "figures", "assignment","prob_pcd1age","part_imd_contact_matrs.png")
) else commandArgs(trailingOnly = TRUE)

# read in data

connect_output <- readRDS(.args[1]) %>% 
  mutate(imd_quintile = paste0('IMD ', imd_quintile))

contacts_by_age <- readRDS(.args[2]) %>% 
  filter(p_id %in% unique(connect_output$p_id))

varname <- gsub('prob_', '', gsub('det_', '', .args[3]))
variables_input <- variables_from_name(varname)

n_bootstraps <- n_distinct(connect_output$bootstrap)

modal_in <- grepl('det', .args[3])

age_labels <- c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75+')

# merge Connect data

contact_matr_data <- connect_output %>% 
  select(p_id, p_age_group, bootstrap, imd_quintile) %>% 
  left_join(contacts_by_age, by = 'p_id')

contact_matr_data_all <- contact_matr_data %>% 
  select(p_age_group, bootstrap, starts_with('cag_')) %>% 
  group_by(p_age_group, bootstrap) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% select(!bootstrap) %>% 
  group_by(p_age_group) %>% 
  summarise_all(median) %>% 
  pivot_longer(!c(p_age_group)) %>% 
  mutate(name = gsub('cag_','',name)) %>% 
  rename(c_age_group = name,
         overall_mean = value)

contact_matr_data_summ <- contact_matr_data %>% 
  select(p_age_group, bootstrap, imd_quintile, starts_with('cag_')) %>% 
  group_by(p_age_group, bootstrap, imd_quintile) %>% 
  summarise_all(mean) %>% 
  ungroup() %>% select(!bootstrap) %>% 
  group_by(p_age_group, imd_quintile) %>% 
  summarise_all(median) %>% 
  pivot_longer(!c(p_age_group, imd_quintile)) %>% 
  mutate(name = gsub('cag_','',name)) %>% 
  rename(c_age_group = name)

contact_matr_data_diff <- contact_matr_data_summ %>% 
  left_join(contact_matr_data_all, by = c('p_age_group','c_age_group')) %>% 
  mutate(diff = value - overall_mean) 

contact_matr_data_summ$p_age_group <- factor(contact_matr_data_summ$p_age_group,
                                             levels = age_labels)
contact_matr_data_summ$c_age_group <- factor(contact_matr_data_summ$c_age_group,
                                             levels = age_labels)
contact_matr_data_diff$p_age_group <- factor(contact_matr_data_diff$p_age_group,
                                             levels = age_labels)
contact_matr_data_diff$c_age_group <- factor(contact_matr_data_diff$c_age_group,
                                             levels = age_labels)

fig_a <- contact_matr_data_summ %>% 
  ggplot() + 
  geom_tile(aes(x = p_age_group, y = c_age_group, fill = value)) + 
  scale_fill_viridis(trans = 'pseudo_log') + 
  facet_grid(.~ imd_quintile, scales = 'fixed') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = 'Participant age group', 
       y = 'Contact age group', 
       fill = 'Mean daily\ncontacts') +
  ggtitle(paste0('Unweighted mean contact matrices, Predictors = ', 
                 paste(unname(unlist(lapply(variables_input, FUN = simp_labels))),
                       collapse = ', '), ',\nMethod = ', 
                 ifelse(modal_in, 'deterministic', 
                        paste0('probabilistic, n_bootstraps = ', n_bootstraps)))); fig_a

# difference from population average

colorscale <- c('#045a8d', '#bdc9e1','#f1eef6','white','#fef0d9','#fdcc8a','#b30000')
breaks <- sort(c(max(contact_matr_data_diff$diff), min(contact_matr_data_diff$diff), 
               quantile(contact_matr_data_diff$diff, c(0.01, 0.1, 0.5, 0.9, 0.99))))

fig_b <- contact_matr_data_diff %>% 
  ggplot() + 
  geom_tile(aes(x = p_age_group, y = c_age_group, fill = diff)) + 
  scale_fill_gradientn(colors = colorscale, values = scales::rescale(breaks), 
                       breaks = breaks[c(1,2,4,6,7)],
                       na.value = "#e5e5e5", 
                       labels = round(breaks, 2)[c(1,2,4,6,7)]) +
  facet_grid(.~ imd_quintile, scales = 'fixed') +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = 'Participant age group', 
       y = 'Contact age group', 
       fill = 'Difference in mean\ndaily contacts') +
  ggtitle(paste0('Unweighted mean contact matrices minus population average')); fig_b

fig_a + fig_b + plot_layout(nrow = 2)

ggsave(.args[4],
       width = 15, height = 8)



