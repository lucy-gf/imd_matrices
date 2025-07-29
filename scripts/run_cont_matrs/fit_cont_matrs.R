
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
  file.path("output", "data", "cont_matrs","participants.rds"),
  file.path("output", "data", "cont_matrs","indiv_contacts.rds"),
  file.path("output", "data", "cont_matrs","cont_imd_distr.rds"),
  file.path("data", "ons","polymod_weights.rds"),
  '0-4',
  file.path("output", "data", "cont_matrs","fitted_matrs_0-4.csv")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### SET SEED #### 

set.seed(70)

#### READ IN DATA ####

participants <- readRDS(.args[1]) %>% 
  rename(p_imd_q = imd_quintile)
indiv_contacts <- readRDS(.args[2]) %>% 
  mutate(c_location = tolower(c_location))
cont_imd_distr <- readRDS(.args[3]) %>% 
  mutate(c_location = tolower(c_location))
poly_weights <- readRDS(.args[4]) %>% 
  mutate(c_location = tolower(c_location))

age_in <- .args[5]

#### RUN NEGATIVE BINOMIAL FITTING ####
## parallelised across p_imd_quintile
## specific to p_age_group

fit_matr_parallel <- function(imd){
  
  out <- fit_matr(part_filt = participants %>% filter(p_imd_q == imd,
                                                 p_age_group == age_in),
                           cont_filt = indiv_contacts %>% filter(p_imd_q == imd,
                                                            p_age_group == age_in),
                           distr_filt = cont_imd_distr %>% filter(p_imd_q == imd,
                                                             p_age_group == age_in),
                           p_weights_filt = poly_weights %>% filter(p_age_group == age_in)
  )
  
  out
  
}

fitted_list <- map(
  .x = as.character(1:5),
  .f = fit_matr_parallel
)

fitted <- rbindlist(fitted_list)

# TODO Make reciprocal too


#### SAVE CSV ####

write_csv(fitted, .args[6])

