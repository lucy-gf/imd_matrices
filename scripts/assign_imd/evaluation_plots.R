
#### EVALUATE EACH MODEL ASSIGNMENT ####

summary_stat_in <- c('mse','wis','crps')[2]

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
# source colors
source(file.path("scripts", "setup", "colors.R"))
# source true value datasets
source(file.path("scripts", "assign_imd", "load_true_data.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","connect_prob_pcd1ageethnnssec.rds"),
  file.path("output", "data", "assignment","wis", "prob_pcd1ageethnnssec_scores.csv"),
  'prob_pcd1ageethnnssec',
  file.path("output", "figures", "assignment","prob_pcd1ageethnnssec","evaluation.png")
) else commandArgs(trailingOnly = TRUE)

connect_output <- readRDS(.args[1])

summ_stats <- read_csv(.args[2], show_col_types = F)

modal_in <- grepl('det', .args[3])

# define variables of analysis
varname <- gsub('prob_', '', gsub('det_', '', .args[3]))
variables_input <- variables_from_name(varname)

n_bootstraps <- n_distinct(connect_output$bootstrap)

## plot patchwork of goodness-of-fit indicators 
eval_plots <- fcn_evaluate_imd(
  data_input = connect_output,
  census_data_list = list(#true_vals_engreg,
                          true_vals_age,
                          true_vals_hh_tenure,
                          true_vals_hh_size,
                          true_vals_ethnicity,
                          true_vals_hiqual,
                          true_vals_nssec
                          # true_vals_urban
                          ),
  predictors = variables_input,
  modal = modal_in,
  summary_stat = summary_stat_in, 
  scores = summ_stats
)

p1 <- eval_plots[[1]]; p1

ggsave(gsub('evaluation','distribution',.args[4]),
       height = 10, width = 14)

p3 <- eval_plots[[3]]; p3

ggsave(gsub('evaluation','paper_fig',.args[4]),
       height = 10, width = 12)

p <- eval_plots[[2]]; p

ggsave(.args[4],
       height = 15, width = 20)





