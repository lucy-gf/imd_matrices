
## FIT CONTACT MATRICES ##

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "contact_matrs","merged_data.rds"),
  file.path("output", "data", "contact_matrs","fitted_matrs.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

####
## READ IN DATA
####

####
## RUN NEGATIVE BINOMIAL FITTING
####

####
## SAVE RDS
####

