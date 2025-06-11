
#### EVALUATE EACH MODEL ASSIGNMENT ####

summary_stat_in <- c('mse','wis')[2]

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
# source true value datasets
source(file.path("scripts", "assign_imd", "load_true_data.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","connect_prob_pcd1.rds"),
  file.path("output", "data", "assignment","wis", "prob_pcd1_scores.csv"),
  'prob_pcd1',
  file.path("output", "figures", "assignment","evaluation_prob_pcd1.png")
) else commandArgs(trailingOnly = TRUE)

connect_output <- readRDS(.args[1])

summ_stats <- read_csv(.args[2], show_col_types = F)

modal_in <- grepl('det', .args[3])

# define variables of analysis
varname <- gsub('prob_', '', gsub('det_', '', .args[3]))
variables_input <- variables_from_name(varname)

n_bootstraps <- n_distinct(connect_output$bootstrap)

## plot patchwork of goodness-of-fit indicators
fcn_evaluate_imd(
  data_input = connect_output,
  census_data_list = list(true_vals_engreg,
                          true_vals_age,
                          true_vals_hh_tenure,
                          true_vals_hh_size,
                          true_vals_ethnicity,
                          true_vals_hiqual,
                          true_vals_nssec,
                          true_vals_urban),
  predictors = variables_input,
  modal = modal_in,
  summary_stat = summary_stat_in,
  scores = summ_stats
)

ggsave(.args[4],
       height = 15, width = 26)
