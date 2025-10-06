
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
  file.path("output", "data", "cont_matrs","base","participants.rds"),
  file.path("data", "reconnect", "reconnect_contacts.rds"),
  file.path("data", "census", "utlaageethn.csv"),
  file.path("data", "census", "utlaethnnssec.csv"),
  "base",
  file.path("output", "data", "cont_matrs","base","indiv_contacts.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### SET SEED #### 

set.seed(70)

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

sens_analysis <- .args[5]

#### LINK SAMPLED PARTICIPANTS TO CONTACTS ####

merged <- sampled_parts %>% 
  filter(n_contacts > 0) %>% 
  full_join(contacts %>% 
              filter(p_id %in% unique(sampled_parts$p_id)), 
            by = 'p_id', relationship = 'many-to-many') 

#### ASSIGN IMD TO CONTACTS ####

## Home location contacts

contacts_home <- merged %>% filter(c_location == 'Home') %>% 
  rename(p_imd_q = imd_quintile) %>% 
  mutate(c_imd_q = p_imd_q)
  
contacts_not_home <- merged %>% filter(c_location != 'Home') %>% 
  mutate(row_num_def = 1:n())

## School location contacts

# school contacts where age groups match
contacts_school <- contacts_not_home %>% 
  filter(c_location == 'School',
         p_age_group == c_age_group,
         p_age < 20)

# all remaining contacts
contacts_not_home_not_school <- contacts_not_home %>% 
  filter(row_num_def %notin% contacts_school$row_num_def) %>% 
  select(!imd_quintile)

# remove row_num_def
contacts_school <- contacts_school %>% select(!row_num_def)
contacts_not_home_not_school <- contacts_not_home_not_school %>% select(!row_num_def)

if(nrow(contacts_school) + nrow(contacts_not_home_not_school) != nrow(contacts_not_home)){ warning('Rows not adding up') }

year <- "24" # TODO Change to "25" when possible

dfe_distr <- if(sens_analysis == 'regional'){
  data.table(read_csv(file.path("output", "data", "cont_matrs","dfe",year,"cm_IMD5_AgeRegion_class.csv"), show_col_types = F)) %>% 
    rename(p_engreg = Region) %>% 
    mutate(p_engreg = case_when(grepl('London', p_engreg) ~ 'Greater London',
                              grepl('Yorkshire', p_engreg) ~ 'Yorkshire and the Humber',
                              T ~ p_engreg))
}else{
  data.table(read_csv(file.path("output", "data", "cont_matrs","dfe",year,"cm_IMD5_Age_class.csv"), show_col_types = F))
} 

dfe_distr <- dfe_distr %>% 
  mutate(p_age_group = paste0(gsub(",.*$", "", gsub('\\[','',Agp)),
                              '-',
                              as.numeric(gsub(".*,\\s*", "", gsub(')','',Agp))) - 1),
         c_age_group = p_age_group) %>% 
  select(!c(Agp, n_attr_tot)) %>% 
  rename(imd_quintile = imd_five,
         imd_quintile_c = imd_five_c,
         probability = value)


# run IMD assignment (probabilistic)

sampled_imd_school <- fcn_assign_imd_dfe(
  contacts_school,
  dfe_distr,
  regional = (sens_analysis == 'regional')
) 

sampled_imd_school <- sampled_imd_school %>% 
  rename(p_imd_q = imd_quintile,
         c_imd_q = imd_quintile_c)

## All other contacts

## utla + age group + ethnicity for those aged <20, 65+, or unknown NS-SEC

# sampled_imd_age_ethn <- fcn_assign_imd_cm(
#   data_input = contacts_not_home_not_school %>% 
#     filter(c_age < 18 | c_age >= 65 | c_sec_input == 'Unknown'),
#   census_data = utlaageethn,
#   variables = c('utla','c_age_group','c_ethnicity')
# )

parallel_fcn_assign_imd_cm_1 <- function(ethn){
  
  fcn_assign_imd_cm(
    data_input = contacts_not_home_not_school %>% 
      filter(c_age < 18 | c_age >= 65 | c_sec_input == 'Unknown') %>% 
      filter(c_ethnicity == ethn),
    census_data = utlaageethn %>% filter(c_ethnicity == ethn),
    variables = c('utla','c_age_group','c_ethnicity')
  )
  
}

parallel_fcn_assign_imd_cm_2 <- function(ethn){
  
  fcn_assign_imd_cm(
    data_input = contacts_not_home_not_school %>% 
      filter(p_age %in% 18:64 & c_sec_input != 'Unknown') %>% 
      filter(c_ethnicity == ethn),
    census_data = utlaethnnssec %>% filter(c_ethnicity == ethn),
    variables = c('utla','c_ethnicity','c_sec_input')
  )
  
}

sampled_imd_age_ethn_list <- map(
  .x = unique(utlaageethn$c_ethnicity),
  .f = parallel_fcn_assign_imd_cm_1
)

sampled_imd_age_ethn <- rbindlist(sampled_imd_age_ethn_list)

## utla + ethnicity + nssec for everyone else

# sampled_imd_ethn_nssec <- fcn_assign_imd_cm(
#   data_input = contacts_not_home_not_school %>% 
#     filter(p_age %in% 18:64 & c_sec_input != 'Unknown'),
#   census_data = utlaethnnssec,
#   variables = c('utla','c_ethnicity','c_sec_input')
# )

sampled_imd_ethn_nssec_list <- map(
  .x = unique(utlaethnnssec$c_ethnicity),
  .f = parallel_fcn_assign_imd_cm_2
)

sampled_imd_ethn_nssec <- rbindlist(sampled_imd_ethn_nssec_list)

## merge all sampled outputs
sampled_imd_not_home_not_school <- rbind(sampled_imd_age_ethn,
                                         sampled_imd_ethn_nssec) %>% 
  rename(c_imd_q = imd_quintile) %>% 
  left_join(sampled_parts %>% 
              select(row_id, p_id, bootstrap_index, imd_quintile) %>% 
              rename(p_imd_q = imd_quintile),
            by = c('row_id','p_id','bootstrap_index'),
            relationship = 'many-to-many',
            suffix = c('_c','_p')) 

sampled_imd <- rbind(
  sampled_imd_not_home_not_school, 
  contacts_home,
  sampled_imd_school
)

#### SAVE RDS ####

write_rds(sampled_imd, .args[6])

