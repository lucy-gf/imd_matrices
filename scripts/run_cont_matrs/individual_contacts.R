
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
            by = 'p_id', relationship = 'many-to-many') 

### FOR NOW TO REDUCE CALC. TIME
# TODO remove later
# merged <- merged %>% filter(bootstrap_index <= 10)

#### ASSIGN IMD TO CONTACTS ####

## Home location contacts

contacts_home <- merged %>% filter(c_location == 'Home') %>% 
  rename(p_imd_q = imd_quintile) %>% 
  mutate(c_imd_q = p_imd_q)
  
contacts_not_home <- merged %>% filter(c_location != 'Home') %>% 
  select(!imd_quintile)

## utla + age group + ethnicity for those aged <20, 65+, or unknown NS-SEC

sampled_imd_age_ethn <- fcn_assign_imd_cm(
  data_input = contacts_not_home %>% 
    filter(c_age < 20 | c_age >= 65 | c_sec_input == 'Unknown'),
  census_data = utlaageethn,
  variables = c('utla','c_age_group','c_ethnicity')
)

## utla + ethnicity + nssec for everyone else

sampled_imd_ethn_nssec <- fcn_assign_imd_cm(
  data_input = contacts_not_home %>% 
    filter(p_age %in% 20:64 & c_sec_input != 'Unknown'),
  census_data = utlaethnnssec,
  variables = c('utla','c_ethnicity','c_sec_input')
)

sampled_imd_not_home <- rbind(sampled_imd_age_ethn,
                              sampled_imd_ethn_nssec) %>% 
  rename(c_imd_q = imd_quintile) %>% 
  left_join(sampled_parts %>% 
              select(row_id, p_id, bootstrap_index, imd_quintile) %>% 
              rename(p_imd_q = imd_quintile),
            by = c('row_id','p_id','bootstrap_index'),
            relationship = 'many-to-many',
            suffix = c('_c','_p')) 

sampled_imd <- rbind(
  sampled_imd_not_home, 
  contacts_home
)

#### SAVE RDS ####

write_rds(sampled_imd, .args[5])



# sampled_imd$p_age_group <- factor(sampled_imd$p_age_group,
#                                   levels = age_labels)
# 
# sampled_imd %>% 
#      group_by(p_age_group, p_imd_q) %>% 
#      mutate(n_tot = n()) %>% 
#      group_by(p_age_group, c_age_group, p_imd_q, c_imd_q, n_tot) %>% 
#      summarise(n = n()) %>% 
#      mutate(cont = n/n_tot) %>% 
#      ggplot() + 
#      geom_tile(aes(x = p_age_group, y = c_age_group,
#                    fill = cont)) + 
#      facet_grid(p_imd_q ~ c_imd_q) +
#      theme_bw() 
