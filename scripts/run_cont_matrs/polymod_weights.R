
## SAMPLE LARGE GROUP CONTACT AGES ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(socialmixr)

# set arguments
.args <- if (interactive()) c(
  file.path("data", "ons","age_ethn_sex.xlsx"),
  file.path("data", "ons","polymod_weights.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN ONS AGE STRUCTURE ####

age_structure_fine <- readxl::read_xlsx(.args[1]) %>% 
  filter(Countries == 'England') %>% 
  select(Age, Observation) %>% 
  mutate(Age = parse_number(Age)) %>% 
  mutate('p_age_group' = cut(Age,
                             breaks = age_breaks,
                             labels = age_labels,
                             right = F)) %>% 
  select(!Age) %>% 
  group_by(p_age_group) %>% summarise(n = sum(Observation)) %>% ungroup() %>% 
  mutate(proportion = n/sum(n)) %>% 
  complete(p_age_group, fill = list(n = 0, proportion = 0))

#### MAKE POLYMOD WEIGHTS ####

p_weights <- polymod_weights() 

#### SAVE RDS ####

write_rds(p_weights, .args[2])
