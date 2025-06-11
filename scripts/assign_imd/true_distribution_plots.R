
#### MAKE SUMMARY PLOTS FOR EACH ASSIGNMENT ####

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
# source true value datasets
source(file.path("scripts", "assign_imd", "load_true_data.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","connect_prob_pcd1.rds"),
  'prob_pcd1',
  file.path("output", "figures", "assignment","prob_pcd1","true_distrs.png")
) else commandArgs(trailingOnly = TRUE)

# cat('\n', .args, '\n', sep = '\n')

## make directory for plots
directory_plots <- here::here(file.path("output", "figures", "assignment",.args[2]))

if(!dir.exists(directory_plots)){
  dir.create(directory_plots)
}

connect_output <- readRDS(.args[1])

#### PLOT IMD RATIOS ####
## PLOT THESE DISTRIBUTIONS COMPARED TO TRUE CENSUS DISTRIBUTIONS ##

# define variables of analysis
varname <- gsub('prob_', '', gsub('det_', '', .args[2]))

variables_input <- variables_from_name(varname)

# cat('\n', variables_input, '\n')

modal_in <- grepl('det', .args[2])

## barplot

fcn_barplot_imd(connect_output, 
                c('p_income','p_hiqual','p_age_group','p_ethnicity',
                  'p_emp_1','hh_size_nm','hh_tenure_nm','p_engreg',
                  'p_sec_input')
)

## english region distribution

p1 <- fcn_rev_errorbarplot_imd(connect_output, 
                         'p_engreg',
                         true_vals = true_vals_engreg,
                         ci_width = 0.95,
                         true_distr = T
)

## age distribution

p2 <- fcn_rev_errorbarplot_imd(connect_output, 
                         'p_age_group',
                         true_vals = true_vals_age,
                         ci_width = 0.95,
                         true_distr = T
)

## household tenure distribution

p3 <- fcn_rev_errorbarplot_imd(connect_output, 
                         'hh_tenure_nm',
                         true_vals = true_vals_hh_tenure,
                         ci_width = 0.95,
                         true_distr = T
)


## household size distribution

p4 <- fcn_rev_errorbarplot_imd(connect_output, 
                         'hh_size_nm',
                         true_vals = true_vals_hh_size,
                         ci_width = 0.95,
                         true_distr = T
)

## ethnicity distribution

p5 <- fcn_rev_errorbarplot_imd(connect_output, 
                         'p_ethnicity',
                         true_vals = true_vals_ethnicity,
                         ci_width = 0.95,
                         true_distr = T
)


## highest qualification distribution

p6 <- fcn_rev_errorbarplot_imd(connect_output, 
                         'p_hiqual',
                         true_vals = true_vals_hiqual,
                         ci_width = 0.95,
                         true_distr = T
)

## nssec distribution

p7 <- fcn_rev_errorbarplot_imd(connect_output, 
                         'p_sec_input',
                         true_vals = true_vals_nssec,
                         ci_width = 0.95,
                         true_distr = T
)

## urban/rural

p8 <- fcn_rev_errorbarplot_imd(connect_output, 
                         'p_urban_rural',
                         true_vals = true_vals_urban,
                         ci_width = 0.95,
                         true_distr = T
)

# patchwork plots

p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + plot_layout(ncol = 2)

ggsave(.args[3], width = 22, height = 18)




