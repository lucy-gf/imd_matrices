
## BALANCE CONTACT MATRICES ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","fitted_matrs.csv"),
  "base",
  file.path("output", "data", "cont_matrs","base","fitted_matrs_balanced.csv")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

sens_analysis <- .args[2] 

#### READ IN DATA ####
 
if(sens_analysis == 'regional'){
  
  imd_age_raw <- data.table(read_csv(file.path("data","imd_25","imd_ages_1.csv"), show_col_types = F))
  
  imd_age <- imd_age_raw %>% 
    mutate(p_engreg = case_when(
      grepl('London',p_engreg) ~ 'Greater London',
      grepl('Yorkshire',p_engreg) ~ 'Yorkshire and the Humber',
      T ~ p_engreg
    ),
    imd_q = imd_quintile,
    population = pop,
    age = age_grp) %>% 
    select(p_engreg, imd_q, age, population) %>% 
    group_by(p_engreg, imd_q, age) %>% 
    summarise(population = sum(population)) %>% ungroup() %>% 
    group_by(p_engreg, imd_q) %>% 
    mutate(tot_pop_imd = sum(population)) %>% 
    ungroup() %>% 
    group_by(p_engreg) %>% mutate(tot_pop = sum(population)) %>% ungroup() %>% 
    mutate(prop_imd = population/tot_pop_imd,
           prop = population/tot_pop)
  
  imd_age$age <- factor(imd_age$age, levels = age_labels)
  
}else{
  
  age_structure_num <- ifelse(sens_analysis != 'nhs_ages', 1, 2)
  
  if(sens_analysis == 'nhs_ages'){
    age_limits <- c(5,12,18,26,35,50,70,80)
    age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
  }
  
  imd_age_raw <- data.table(read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F))
  
  imd_age <- imd_age_raw %>% 
    mutate(
    imd_q = imd_quintile,
    population = pop,
    age = age_grp) %>% 
    select(imd_q, age, population) %>% 
    group_by(imd_q, age) %>% 
    summarise(population = sum(population)) %>% ungroup() %>% 
    group_by(imd_q) %>% 
    mutate(tot_pop_imd = sum(population)) %>% 
    ungroup() %>% 
    mutate(tot_pop = sum(population)) %>% ungroup() %>% 
    mutate(prop_imd = population/tot_pop_imd,
           prop = population/tot_pop)
  
  imd_age$age <- factor(imd_age$age, levels = age_labels)
}

#### BALANCE ####

if(sens_analysis == 'regional'){
  
  balanced_matr <- data.table()
  
  for(reg in unique(imd_age$p_engreg)){
    
    fitted <- data.table(suppressWarnings(read_csv(gsub('.csv',paste0('_', reg, '.csv'), .args[1]), show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
    
    balanced_matr_r <- balancing_fcn(
      data = fitted,
      age_structure = imd_age %>% filter(p_engreg==reg)
    )
    
    if(nrow(balanced_matr_r[is.na(n)]) > 0){warning(paste0('Some n = NA in ', reg))}
    
    balanced_matr <- rbind(balanced_matr, balanced_matr_r)

  }
  
}else{
  
  fitted <- data.table(suppressWarnings(read_csv(.args[1], show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
  
  balanced_matr <- balancing_fcn(
    data = fitted,
    age_structure = imd_age
  )
  
}

write_csv(balanced_matr, .args[3])








