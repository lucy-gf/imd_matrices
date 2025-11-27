
#### AGE-SPECIFIC MEAN CONTACTS ####

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2)

# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))
# source colors
source(file.path("scripts", "setup", "colors.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","connect_prob_pcd1.rds"),
  file.path("data", "connect","connect_contacts.rds"),
  'prob_pcd1',
  file.path("output", "figures", "assignment","prob_pcd1","mean_age_contacts.png")
) else commandArgs(trailingOnly = TRUE)

## make directory for plots
directory_plots <- here::here(file.path("output", "figures", "assignment",.args[3]))

if(!dir.exists(directory_plots)){
  dir.create(directory_plots)
}

# cat('\n', .args, '\n', sep = '\n')

connect_output <- readRDS(.args[1])

connect_contacts <- readRDS(.args[2]) %>% 
  filter(p_id %in% unique(connect_output$p_id))

## age plots ##

# define variables of analysis
varname <- gsub('prob_', '', gsub('det_', '', .args[3]))
variables_input <- variables_from_name(varname)

modal_in <- grepl('det', .args[3])

n_bootstraps <- n_distinct(connect_output$bootstrap)

ci_age <- 0.95

# mean age-specific daily contacts by IMD

fig_a <- connect_output %>% 
  group_by(p_age_group, imd_quintile, bootstrap) %>% 
  summarise(mean = mean(n_contacts + large_n)) %>% 
  group_by(p_age_group, imd_quintile) %>% 
  summarise(med = median(mean),
            lower = quantile(mean, (1 - ci_age)/2),
            upper = quantile(mean, (1 - (1 - ci_age)/2))) %>% 
  ggplot() +
  geom_errorbar(aes(p_age_group, ymin = lower, ymax = upper, 
                    col = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                width = 0.2, lwd = 0.8, position = position_dodge(width = 0.9)) +
  geom_point(aes(p_age_group, med, col = as.factor(imd_quintile)), 
             size = 3, position = position_dodge(width = 0.9)) +
  theme_bw() + ylim(c(0,NA)) + 
  labs(color = 'IMD quintile', fill = 'IMD quintile', 
       x = 'Age group', y = 'Mean contacts') + 
  ggtitle(paste0('Age-specific mean contacts, ', 100*ci_age, '% CI, Predictors = ', 
                 paste(unname(unlist(lapply(variables_input, FUN = simp_labels))),
                       collapse = ', '), ',\nMethod = ', 
                 ifelse(modal_in, 'deterministic', 
                        paste0('probabilistic, n_bootstraps = ', n_bootstraps)))) + 
  scale_color_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


# proportion of contacts who are under 18, by participant age and IMD quintile

n_contacts <- nrow(connect_contacts)
n_bootstraps <- n_distinct(connect_output$bootstrap)

indiv_u18 <- connect_contacts %>%
  group_by(p_id) %>% 
  summarise(n_u18 = sum(c_age < 18)) %>% 
  complete(p_id = unique(connect_output$p_id),
           fill = list(n_u18 = 0))

u18_data <- connect_output %>% 
  select(bootstrap, p_id, p_age_group, imd_quintile, 
         n_contacts, large_n, starts_with('add_')) %>% 
  left_join(indiv_u18, by = 'p_id') %>% 
  filter(n_contacts + large_n > 0) %>% # remove those who have 0 contacts
  mutate(prop_u18 = (n_u18 + add_u18_school + add_u18_work + add_u18_other)/(n_contacts + large_n))
  
ci_u18 <- 0.5

fig_b <- u18_data %>% 
  group_by(p_age_group, imd_quintile) %>% 
  summarise(med = mean(prop_u18),
            lower = quantile(prop_u18, (1 - ci_u18)/2),
            upper = quantile(prop_u18, (1 - (1 - ci_u18)/2))) %>% 
  ggplot() +
  # geom_errorbar(aes(p_age_group, ymin = lower, ymax = upper, 
  #                   col = as.factor(imd_quintile), group = as.factor(imd_quintile)),
  #               width = 0.2, lwd = 0.8, position = position_dodge(width = 0.9)) +
  geom_point(aes(p_age_group, med, col = as.factor(imd_quintile)), 
             size = 3, position = position_dodge(width = 0.9)) +
  theme_bw() + ylim(c(0,NA)) + 
  labs(color = 'IMD quintile', fill = 'IMD quintile', 
       x = 'Age group', y = 'Proportion') + 
  ggtitle(paste0('Mean proportion of contacts who are <18 years old')) + 
  scale_color_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = 'none'); fig_b

fig_a + fig_b + plot_layout(nrow = 2, guides = 'collect')

ggsave(.args[4],
       width = 10, height = 10)



