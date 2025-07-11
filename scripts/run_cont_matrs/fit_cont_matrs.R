
## FIT CONTACT MATRICES ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "contact_matrs","participants.rds"),
  file.path("output", "data", "contact_matrs","indiv_contacts.rds"),
  file.path("output", "data", "contact_matrs","cont_imd_distr.rds"),
  file.path("data", "ons","polymod_weights.rds"),
  file.path("output", "data", "contact_matrs","fitted_matrs.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN DATA ####

participants <- readRDS(.args[1]) %>% 
  rename(p_imd_q = imd_quintile)
indiv_contacts <- readRDS(.args[2])
cont_imd_distr <- readRDS(.args[3])
poly_weights <- readRDS(.args[4])

#### RUN NEGATIVE BINOMIAL FITTING ####
## parallelised across p_age_group, p_imd_quintile

# TODO remove later
participants <- participants %>% filter(bootstrap_index <= 10)

fit_matr_parallel <- function(imd){
  
  out <- fit_matr_age_spec(part = participants %>% filter(p_imd_q == imd),
                           cont = indiv_contacts %>% filter(p_imd_q == imd),
                           distr = cont_imd_distr %>% filter(p_imd_q == imd),
                           p_weights = poly_weights
  )
  
  out
  
}

fitted <- map(
  .x = 1:5,
  .f = fit_matr_parallel
)






####
## SAVE RDS
####

