
## ASSIGN INDIVIDUAL CONTACT IMD ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "contact_matrs","participants.rds"),
  file.path("data", "connect", "connect_contacts.rds"),
  file.path("data", "census", "utlaageethn.csv"),
  file.path("data", "census", "utlaethnnssec.csv"),
  file.path("output", "data", "contact_matrs","indiv_contacts.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN DATA ####

sampled_parts <- readRDS(.args[1])

contacts <- readRDS(.args[2]) %>% 
  select(p_id, c_id, c_age, c_age_group, 
         c_ethnicity, c_sec_input, c_location)

utlaageethn <- read_csv(.args[3], show_col_types = F) %>% 
  rename(c_age_group = p_age_group, 
         c_ethnicity = p_ethnicity)

utlaethnnssec <- read_csv(.args[4], show_col_types = F) %>% 
  rename(c_ethnicity = p_ethnicity,
         c_sec_input = p_sec_input)

#### LINK SAMPLED PARTICIPANTS TO CONTACTS ####

merged <- sampled_parts %>% 
  filter(n_contacts > 0) %>% 
  full_join(contacts %>% 
              filter(p_id %in% unique(sampled_parts$p_id)), 
            by = 'p_id', relationship = 'many-to-many'
            )


#### ASSIGN IMD TO CONTACTS ####

# utla + age group + ethnicity for those aged <20, 65+, or unknown NS-SEC

sampled_imd_age_ethn <- fcn_assign_imd_cm(
  data_input = merged %>% filter(c_age < 20 | c_age >= 65 | c_sec_input == 'Unknown'),
  census_data = utlaageethn,
  variables = c('utla','c_age_group','c_ethnicity')
)

# utla + ethnicity + nssec for everyone else

sampled_imd_ethn_nssec <- fcn_assign_imd_cm(
  data_input = merged %>% filter(p_age %in% 20:64 & c_sec_input != 'Unknown'),
  census_data = utlaethnnssec,
  variables = c('utla','c_ethnicity','c_sec_input')
)

sampled_imd <- rbind(sampled_imd_age_ethn,
                     sampled_imd_ethn_nssec)


#### SAVE RDS ####

write_rds(sampled_imd, .args[5])

