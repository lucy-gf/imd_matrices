
## Make error scores - MSE ##

summary_stat <- 'crps'
  
# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2)
library(scoringutils)

# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))
# source true value datasets
source(file.path("scripts", "assign_imd", "load_true_data.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","connect_prob_pcd1.rds"),
  'prob_pcd1',
  file.path("output", "data", "assignment","crps","prob_pcd1_scores.csv")
) else commandArgs(trailingOnly = TRUE)

connect_output <- readRDS(.args[1])

census_data_list <- list(true_vals_engreg,
                         true_vals_age,
                         true_vals_hh_tenure,
                         true_vals_hh_size,
                         true_vals_ethnicity,
                         true_vals_hiqual,
                         true_vals_nssec,
                         true_vals_urban)

summ_stats <- get(paste0('fcn_',summary_stat))(
  survey_data = connect_output,
  census_data_l = census_data_list
) %>% mutate(model = .args[2])
  
# save errors

write_csv(summ_stats, .args[3])

  
  