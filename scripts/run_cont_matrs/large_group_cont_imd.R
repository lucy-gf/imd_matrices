
## SAMPLE LARGE GROUP CONTACT IMD ##

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "contact_matrs","large_cont_ages.rds"),
  file.path("output", "data", "contact_matrs","cont_imd_distr.rds"),
  file.path("output", "data", "contact_matrs","large_cont_imd.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

####
## READ IN LARGE N AGES, IMD DISTRIBUTION
####

####
## SAMPLE LARGE GROUP CONTACT IMD
####

####
## SAVE RDS
####

