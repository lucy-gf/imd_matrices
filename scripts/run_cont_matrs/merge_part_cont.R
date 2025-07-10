
## MERGE PARTICIPANTS AND CONTACTS ##

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "contact_matrs","participants.rds"),
  file.path("output", "data", "contact_matrs","indiv_contacts.rds"),
  file.path("output", "data", "contact_matrs","large_cont_imd.rds"),
  file.path("output", "data", "contact_matrs","merged_data.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

####
## READ IN DATA
####

####
## MAKE NEAT
####

####
## SAVE RDS
####

