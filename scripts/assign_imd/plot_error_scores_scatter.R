
## PLOT COMPARISONS OF MSE/WSI/CRPS ACROSS PREDICTOR COMBINATIONS ##

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2)
library(scoringutils)

# source colors
source(file.path("scripts", "setup", "colors.R"))
# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","wis","merged_scores.csv"),
  'wis',
  file.path("output", "figures", "assignment","wis_scatter.png")
) else commandArgs(trailingOnly = TRUE)

## read in data

error_scores <- read_csv(.args[1], show_col_type = F) %>% 
  filter(imd_quintile != 'imd_quintile') %>%  # filter out the header rows from merging process
  mutate(stat = as.numeric(stat))

error_scores <- data.table(error_scores)

error_scores[, method := case_when(grepl('det_', model) ~ 'det',
                           grepl('prob_', model) ~ 'prob')]
error_scores[, variable := gsub('p_|_nm', '', variable)]

## plot

error_scores %>% 
  filter(method != 'det') %>%
  group_by(model, variable) %>% 
  summarise(mean_stat = mean(stat)) %>%
  ggplot() + 
  geom_point(aes(variable, mean_stat, col = model),
             position = position_dodge(width = 0.5), size = 3) + 
  theme_bw() + scale_color_brewer(palette = 'Set2') +
  labs(col = 'Predictors',
       y = toupper(.args[2]), 
       x = '') +
  ylim(c(0,NA))

## save
ggsave(.args[3],
       width = 10, height = 6)





