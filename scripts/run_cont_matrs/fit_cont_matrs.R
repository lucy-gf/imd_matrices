
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
  file.path("output", "data", "cont_matrs","base","participants.rds"),
  file.path("output", "data", "cont_matrs","base","indiv_contacts.rds"),
  file.path("output", "data", "cont_matrs","base","cont_imd_distr.rds"),
  file.path("data", "ons","polymod_weights.rds"),
  file.path("output", "data","cont_matrs","reconnect_weights.rds"),
  "base",
  "0-4",
  file.path("output", "data", "cont_matrs", "base", "fitted_matrs_0-4.csv")
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
reconnect_weights <- readRDS(.args[5]) %>% 
  mutate(c_location = tolower(c_location))

## set sensitivity analysis ##
sens_analysis <- .args[6]
if(! grepl(sens_analysis, .args[8])){stop('Sens. analysis name not in output file path.')}
if(!file.exists(file.path("output", "data", "cont_matrs", sens_analysis))){dir.create(file.path("output", "data", "cont_matrs", sens_analysis))}

## set age group ##
age_in <- .args[7]

#### RUN NEGATIVE BINOMIAL FITTING ####
## parallelised across p_imd_quintile
## specific to p_age_group

# restrict large_n columns to max. 100, unless in 'no_cap_100' sens analysis
max_large_n <- 100

if(sens_analysis != 'no_cap_100'){
  participants <- participants %>% 
    mutate(across(contains('add_'), ~ case_when(. > max_large_n ~ max_large_n, T ~ .))) %>% 
    mutate(large_n = rowSums(across(contains('add_'))))
}

# use polymod individual weights unless in 'large_n_age' sens analysis
weights <- if(sens_analysis != "large_n_age"){
  poly_weights
}else{
  reconnect_weights
}

# parallelised function

fit_matr_parallel <- function(imd){
  
  out <- fit_matr(part_filt = participants %>% filter(p_imd_q == imd,
                                                 p_age_group == age_in),
                           cont_filt = indiv_contacts %>% filter(p_imd_q == imd,
                                                            p_age_group == age_in),
                           distr_filt = cont_imd_distr %>% filter(p_imd_q == imd,
                                                             p_age_group == age_in),
                           p_weights_filt = weights %>% filter(p_age_group == age_in)
  ) 
  
  out
  
}

fit_matr_parallel_regional <- function(imd){
  
  out_df <- data.frame()
  
  for(reg in unique(participants$p_engreg)){
    
    print(reg)
    
    out <- fit_matr(part_filt = participants %>% filter(p_imd_q == imd,
                                                        p_age_group == age_in,
                                                        p_engreg == reg),
    cont_filt = indiv_contacts %>% filter(p_imd_q == imd,
                                          p_age_group == age_in,
                                          p_engreg == reg),
    distr_filt = cont_imd_distr %>% filter(p_imd_q == imd,
                                           p_age_group == age_in,
                                           p_engreg == reg),
    p_weights_filt = weights %>% filter(p_age_group == age_in)
    ) 
    
    out_df <- rbind(out_df, out %>% mutate(p_engreg = reg))
    
  }
  
  out
  
}

# run 

fitted_list <- map(
  .x = as.character(1:5),
  .f = if(sens_analysis == 'regional'){fit_matr_parallel_regional}else{fit_matr_parallel}
)

fitted <- rbindlist(fitted_list)

#### SAVE CSV ####

write_csv(fitted, .args[8])

