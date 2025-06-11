
#### AGE-SPECIFIC MEAN CONTACTS ####

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2)

# source colors
source(file.path("scripts", "setup", "colors.R"))
# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","connect_prob_pcd1.rds"),
  'prob_pcd1',
  file.path("output", "figures", "assignment","prob_pcd1","mean_age_contacts.png")
) else commandArgs(trailingOnly = TRUE)

# cat('\n', .args, '\n', sep = '\n')

connect_output <- readRDS(.args[1])

## age plots ##

# define variables of analysis
varname <- gsub('prob_', '', gsub('det_', '', .args[2]))
variables_input <- variables_from_name(varname)

modal_in <- grepl('det', .args[2])

n_bootstraps <- n_distinct(connect_output$bootstrap)

ci_age <- 0.95

connect_output %>% 
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

ggsave(.args[3],
       width = 10, height = 7)

# connect_output %>% 
#   group_by(imd_quintile, p_age_group, bootstrap) %>% 
#   summarise(mean = mean(n_contacts + large_n),
#             n = n()) %>% 
#   group_by(imd_quintile, p_age_group) %>% 
#   summarise(med = median(mean),
#             lower = quantile(mean, (1 - ci_age)/2),
#             upper = quantile(mean, (1 - (1 - ci_age)/2)),
#             mean_n = round(mean(n),0)) %>% 
#   ggplot() +
#   geom_errorbar(aes(imd_quintile, ymin = lower, ymax = upper, col = as.factor(imd_quintile)),
#                 width = 0.2, lwd = 0.8) +
#   geom_line(aes(x = imd_quintile, y = med), lwd = 0.5, alpha = 0.5) +
#   geom_point(aes(imd_quintile, med, col = as.factor(imd_quintile)), size = 3) +
#   theme_bw() + ylim(c(0,NA)) + facet_wrap(p_age_group~., scales = 'free') + 
#   labs(color = 'IMD quintile', fill = 'IMD quintile', y = 'Mean contacts', x = 'IMD quintile') + 
#   scale_color_manual(values = imd_quintile_colors) +
#   ggtitle(paste0('Age-specific mean contacts, ', 100*ci_age, '% CI, Predictors = ', 
#                  paste(unname(unlist(lapply(variables_input, FUN = simp_labels))),
#                        collapse = ', '), ',\nMethod = ', 
#                  ifelse(modal_in, 'deterministic', 
#                         paste0('probabilistic, n_bootstraps = ', n_bootstraps)))) + 
#   geom_text(aes(x = imd_quintile, y = 1.1*upper, label = mean_n, color = as.factor(imd_quintile)))
# 
# ggsave(here::here(directory_plots, 'mean_contacts_imd_age_faceted.png'),
#        width = 12, height = 14)




