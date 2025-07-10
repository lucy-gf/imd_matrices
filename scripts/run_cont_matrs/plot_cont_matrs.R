
## PLOT CONTACT MATRICES ##

# set arguments
.args <- if (interactive()) c(
  file.path("output", "figures", "contact_matrs","fitted_matrs.rds"),
  file.path("output", "data", "contact_matrs","fitted_matrs.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

####
## READ IN FITTETD DATA
####

####
## PLOT
####

####
## SAVE PNG
####

