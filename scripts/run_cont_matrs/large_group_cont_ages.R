
## SAMPLE LARGE GROUP CONTACT AGES ##

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "contact_matrs","participants.rds"),
  file.path("output", "data", "contact_matrs","large_cont_ages.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

####
## MAKE POLYMOD WEIGHTS
####

####
## READ IN PARTICIPANT DATA, THEIR LARGE N
####

####
## SAMPLE LARGE GROUP CONTACT AGES
####

####
## SAVE RDS
####

