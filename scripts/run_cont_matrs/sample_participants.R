
## BOOTSTRAP PARTICIPANTS, ASSIGN IMD ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)

# set arguments
.args <- if (interactive()) c(
  file.path("data", "connect", "connect_part.rds"),
  file.path("data", "ons", "age_ethn_sex.xlsx"),
  file.path("data", "census", "pcd1ageethn.csv"),
  file.path("data", "census", "pcd1ethnnssec.csv"),
  file.path("output", "data", "cont_matrs","participants.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### MAKE OUTPUT DIRS IF DON'T EXIST ####

if(!file.exists(file.path("output"))){
  dir.create(file.path("output"))
  dir.create(file.path("output", "data"))
  dir.create(file.path("output", "figures"))
}

if(!file.exists(file.path("output", "data", "cont_matrs"))){
  dir.create(file.path("output", "data", "cont_matrs"))
}
if(!file.exists(file.path("output", "figures", "cont_matrs"))){
  dir.create(file.path("output", "figures", "cont_matrs"))
}

#### SET SEED #### 

set.seed(70)

#### SET PARS #### 

n_bootstraps <- 1000

#### READ IN DATA FOR WEIGHTING #### ]
part <- readRDS(.args[1])
age_ethn_sex <- read_xlsx(.args[2])
pcd1ageethn <- readr::read_csv(.args[3], show_col_types = F)
pcd1ethnnssec <- readr::read_csv(.args[4], show_col_types = F)

# filter to only those usable in analysis

part <- part %>% 
  filter(p_country == 'England',
         ! (is.na(p_sec_input) & p_age %in% 18:64),
         p_ethnicity != 'Prefer not to say',
         pcd1 %in% unique(pcd1ageethn$pcd1))

# census 2021 age/ethnicity/sex populations

age_ethn_sex <- age_ethn_sex %>% 
  filter(Countries == 'England') %>% 
  select(Age, Ethnicity, Sex, Observation) %>% 
  mutate(Age = parse_number(Age),
         Ethnicity = case_when(
           Ethnicity %like% 'Asian' ~ 'Asian',
           Ethnicity %like% 'Black' ~ 'Black',
           Ethnicity %like% 'Mixed' ~ 'Mixed',
           Ethnicity %like% 'White' ~ 'White',
           T ~ 'Other'
         )) %>% 
  mutate('p_age_group' = cut(Age,
                             breaks = age_breaks,
                             labels = age_labels,
                             right = F)) %>% 
  select(!Age) %>% 
  rename(p_ethnicity = Ethnicity,
         p_gender = Sex) %>% 
  group_by(p_age_group, p_ethnicity, p_gender) %>% summarise(value = sum(Observation)) %>% ungroup() %>% 
  mutate(proportion = value/sum(value)) %>% complete(p_age_group, p_ethnicity, p_gender,
                                                     fill = list(value = 0, proportion = 0))

#### MAKE AGE-SPECIFIC WEIGHTS (GENDER, ETHNICITY, DAY_WEEK)

weights <- weight_participants(
  participant_input = part, 
  eth_age_sex_structure = age_ethn_sex,
  weighting = c('p_ethnicity','p_gender','day_week'),
  group_vars = 'p_age_group',
  truncation_percentile = c(0.05,0.95)
)

part <- part %>% 
  left_join(weights %>% select(p_id, post_strat_weight), by = 'p_id')

#### SAMPLE PARTICIPANTS, BY AGE GROUP ####

sampled_list <- map(
  .x = levels(part$p_age_group),
  .f = fcn_sample_participants
)

sampled <- rbindlist(sampled_list) %>% 
  left_join(part %>% select(p_id, p_age, p_ethnicity, p_sec_input, 
                            pcd1, utla,
                            n_contacts, large_n,
                            contains('add_')),
            by = 'p_id')


#### ASSIGN IMD ####

# pcd1, age, ethnic group

sampled_imd_age_ethn <- fcn_assign_imd_cm(
  data_input = sampled %>% filter(p_age < 18 | p_age >= 65),
  census_data = pcd1ageethn,
  variables = c('pcd1','p_age_group','p_ethnicity')
  )

# pcd1, ethnic group, NS-SEC code

sampled_imd_ethn_nssec <- fcn_assign_imd_cm(
  data_input = sampled %>% filter(p_age %in% 18:64),
  census_data = pcd1ethnnssec,
  variables = c('pcd1','p_ethnicity','p_sec_input')
)

sampled_imd <- rbind(sampled_imd_age_ethn,
                     sampled_imd_ethn_nssec)

sampled_imd <- sampled_imd %>% 
  mutate(row_id = paste0(p_id, '_', bootstrap_index, '_', 1:nrow(sampled_imd)))

#### SAVE RDS ####

write_rds(sampled_imd, .args[5])

