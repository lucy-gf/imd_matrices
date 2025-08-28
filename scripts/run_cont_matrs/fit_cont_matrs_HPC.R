
## FIT CONTACT MATRICES ##

setwd('imd_matrices')

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)

# set arguments
args <- commandArgs(trailingOnly = TRUE)
age_in_index <- as.numeric(args[1])

# select age
age_breaks <- c(-Inf, 5*1:(75/5), Inf)
age_vals <- age_breaks[is.finite(age_breaks)]
age_labels <- c(paste0(c(0, age_vals[1:length(age_vals)-1]), '-', c(age_vals-1)), paste0(age_vals[length(age_vals)], '+'))
age_in <- age_labels[age_in_index]

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### SET SEED #### 

set.seed(70)

#### READ IN DATA ####

participants <- readRDS( file.path("output", "data", "cont_matrs","participants.rds")) %>% 
  rename(p_imd_q = imd_quintile)
indiv_contacts <- readRDS(file.path("output", "data", "cont_matrs","indiv_contacts.rds")) %>% 
  mutate(c_location = tolower(c_location))
cont_imd_distr <- readRDS(file.path("output", "data", "cont_matrs","cont_imd_distr.rds")) %>% 
  mutate(c_location = tolower(c_location))
poly_weights <- readRDS(file.path("data", "ons","polymod_weights.rds")) %>% 
  mutate(c_location = tolower(c_location))

#### RUN NEGATIVE BINOMIAL FITTING ####
## parallelised across p_imd_quintile
## specific to p_age_group

# restrict large_n columns to max. 100
max_large_n <- 100
participants <- participants %>% 
  mutate(across(contains('add_'), ~ case_when(. > max_large_n ~ max_large_n, T ~ .))) %>% 
  mutate(large_n = rowSums(across(contains('add_'))))

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

#### SAVE CSV ####

write_csv(fitted, file.path("output", "data", "cont_matrs", paste0("fitted_matrs_", age_in, ".csv")))

