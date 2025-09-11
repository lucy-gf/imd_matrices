
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

imd_age <- data.table(read_csv(file.path("data", "census","imd_age.csv"), show_col_types = F))
imd_age$age <- gsub('.{1}$','',gsub('----','-',gsub('-----','',gsub("\\D", "-", imd_age$age))))
imd_age <- imd_age %>% 
  mutate(age = case_when(age == '4' ~ '0-4',
                         age == '75' ~ '75+',
                         T ~ age)) %>% 
  group_by(imd_q) %>% mutate(tot_pop_imd = sum(population)) %>% 
  ungroup() %>% 
  mutate(tot_pop = sum(population),
         prop_imd = population/tot_pop_imd,
         prop = population/tot_pop)
imd_age$age <- factor(imd_age$age, levels = age_labels)

#### BALANCE ####

balanced_matr <- balancing_fcn(
  data = fitted,
  age_structure = imd_age,
  setting_specific = T
)

balanced_matr <- balanced_matr %>% 
  group_by(bootstrap_index, p_age_group, p_imd_q, c_age_group, c_imd_q) %>% 
  summarise(n = sum(n))

write_csv(balanced_matr, .args[2])





