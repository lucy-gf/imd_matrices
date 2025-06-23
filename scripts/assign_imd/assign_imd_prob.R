
#### RUN IMD ASSIGNMENT ####

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)

# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))

# set arguments 
.args <- if (interactive()) c(
  file.path("data", "connect", "connect_part.rds"),
  file.path("data", "census", "pcd1agehiqualnssec.csv"),
  "pcd1agehiqualnssec",
  file.path("output", "data", "assignment","connect_prob_pcd1agehiqualnssec.rds")
) else commandArgs(trailingOnly = TRUE)

# read in connect data
connect_part <- readRDS(.args[1]) %>% 
  filter(p_country == 'England')

# read in census data
census_input <- read_csv(.args[2], show_col_types = F)

# define variables of analysis
variables_input <- variables_from_name(.args[3])

# make assignments
assign_out <- fcn_assign_imd(
  data_input = connect_part,
  census_data = census_input,
  variables = variables_input,
  n_bootstraps = 100,
  modal = F
)

# save output
write_rds(assign_out, .args[4])

cat('\n', .args[3], ' done\n', sep = '')
