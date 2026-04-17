
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
  "regional_nhs_ages",
  "2"
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### SET SEED #### 

set.seed(70)

## set start time
start_time <- Sys.time()

## set sensitivity analysis ##
sens_analysis <- .args[1]
if(!file.exists(file.path("output", "data", "cont_matrs", sens_analysis))){dir.create(file.path("output", "data", "cont_matrs", sens_analysis))}

sens_analysis_folder <- if(grepl('regional', sens_analysis)){
  'regional'
}else{
  if(sens_analysis == 'old_imd'){
    'old_imd'
  }else{
    'base'
  }
}

sens_analysis_folder_contacts <- if(sens_analysis %in% c('regional','nhs_ages','regional_nhs_ages','old_imd')){
  sens_analysis
}else{
  'base'
}

#### READ IN DATA ####

participants <- readRDS(file.path("output", "data", "cont_matrs",sens_analysis_folder,"participants.rds")) %>% 
  rename(p_imd_q = imd_quintile)
indiv_contacts <- readRDS(file.path("output", "data", "cont_matrs",sens_analysis_folder_contacts,"indiv_contacts.rds")) %>% 
  mutate(c_location = tolower(c_location))
cont_imd_distr <- readRDS(file.path("output", "data", "cont_matrs",sens_analysis_folder_contacts,"cont_imd_distr.rds")) %>% 
  mutate(c_location = tolower(c_location))
poly_weights <- readRDS(file.path("data", "ons","polymod_weights.rds")) %>% 
  mutate(c_location = tolower(c_location))
reconnect_weights <- if(!grepl('nhs_ages',sens_analysis)){
  readRDS(file.path("output", "data","cont_matrs","reconnect_weights.rds")) %>% 
  mutate(c_location = tolower(c_location))
}else{
  readRDS(file.path("output", "data","cont_matrs","reconnect_weights_nhs_ages.rds")) %>% 
    mutate(c_location = tolower(c_location))
}

## change age groups if needed ##
if(grepl('nhs_ages',sens_analysis)){
  
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
  
  participants <- participants %>% 
    mutate(p_age_group = cut(p_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F))
  
  indiv_contacts <- indiv_contacts %>% 
    mutate(p_age_group = cut(p_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F),
           c_age_group = cut(c_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F))
  
}

## set age group ##
age_index <- as.numeric(.args[2])
n_age_groups <- length(unique(participants$p_age_group))
  
age_in <- unique(participants$p_age_group)[age_index]

## output file
output_folder <- file.path("output", "data", "cont_matrs", sens_analysis, paste0("fitted_matrs_", age_in, ".csv"))
cat('\nOutput file: ', output_folder, sep = '')

## create temp file path ##
tmp_file <- paste0(output_folder, ".tmp")

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
weights <- if(sens_analysis %notin% c("nhs_ages","regional_nhs_ages","large_n_age")){
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
    
    reg <- gsub('\n','', reg)
    
    cat('\n', reg, sep = '')
    
    if(nrow(participants %>% filter(p_imd_q == imd,
                                    p_age_group == age_in,
                                    p_engreg == reg)) == 0){warning('No participants in region')}
    
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
  
  out_df
  
}

# run 

fitted_list <- map(
  .x = as.character(1:5),
  .f = if(grepl('regional',sens_analysis)){fit_matr_parallel_regional}else{fit_matr_parallel}
)

fitted <- rbindlist(fitted_list)

#### SAVE CSV ####

# write to the temp file
write.csv(fitted, tmp_file)

# rename to final file (only  if write was successful)
file.rename(tmp_file, output_folder)

# Print data
cat('\nTime taken: ', floor(difftime(Sys.time(), start_time, units = 'secs')[[1]]/60), ' mins ',
    round(difftime(Sys.time(), start_time, units = 'secs')[[1]] -
            60*floor(difftime(Sys.time(), start_time, units = 'secs')[[1]]/60)), ' secs',
    sep = '')
mem <- memory.profile()
cat("\nPeak memory used (R side): ", round(sum(mem) / 1024 / 1024, 2), "GB\n", sep = '')





