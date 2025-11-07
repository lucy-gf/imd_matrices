
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
  file.path("output", "data", "cont_matrs","balance_sett_spec","fitted_matrs_balanced.csv")
) else commandArgs(trailingOnly = TRUE)
 
source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

if(!file.exists(file.path("output", "data", "cont_matrs","balance_sett_spec"))){dir.create(file.path("output", "data", "cont_matrs","balance_sett_spec"))}

#### READ IN DATA ####

fitted <- data.table(suppressWarnings(read_csv(.args[1], show_col_types = F)))[bootstrap_index != 'bootstrap_index',]

imd_age <- data.table(read_csv(file.path("data","imd_25","imd_ages_1.csv"), show_col_types = F))

imd_age <- imd_age <- imd_age_raw %>% 
  mutate(p_engreg = case_when(
    grepl('London',p_engreg) ~ 'Greater London',
    grepl('Yorkshire',p_engreg) ~ 'Yorkshire and the Humber',
    T ~ p_engreg
  ),
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

#### BALANCE ####

balanced_matr <- balancing_fcn(
  data = fitted,
  age_structure = imd_age,   
  setting_specific = T
)

write_csv(balanced_matr, gsub('_balanced.csv','.csv',.args[2]))

balanced_matr <- balanced_matr %>% 
  group_by(bootstrap_index, p_age_group, p_imd_q, c_age_group, c_imd_q) %>% 
  summarise(n = sum(n))

write_csv(balanced_matr, .args[2])





