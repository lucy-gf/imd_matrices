
## INDIVIDUAL CONTACT IMD DISTRIBUTION ##

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "contact_matrs","indiv_contacts.rds"),
  file.path("output", "data", "contact_matrs","cont_imd_distr.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

####
## READ IN INDIVIDUAL CONTACTS
####

####
## TURN INTO DISTRIBUTION
# p_age_group x c_age_group x p_imd_quintile x c_location 
####

####
## SAVE RDS
####

