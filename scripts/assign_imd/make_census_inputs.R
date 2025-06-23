
############################################
#### FORMATTING CENSUS AND ONS DATA FOR ####
### INFERRING IMD OF SURVEY PARTICIPANTS ###
############################################

time <- Sys.time()

## load packages
library(readr)
library(data.table)
library(tibble)
library(tidyr)
library(dplyr)
library(here)

here::here()

select <- dplyr::select

## Matching postcodes to LSOAs
## from https://geoportal.statistics.gov.uk/datasets/3770c5e8b0c24f1dbe6d2fc6b46a0b18 
pcd_to_lsoa <- data.table(read_csv(here::here('data','ons','PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv'), show_col_types = F) %>% 
                            select(starts_with('pcd'),'lsoa21cd') %>% 
                            separate_wider_delim(pcds, delim = " ", names = c("pcd1", "pcd2")))

# select only LSOAs in England
pcd_to_lsoa <- pcd_to_lsoa[substr(lsoa21cd, 1, 1) == 'E',]

cat('Unique pcd1s: ', n_distinct(pcd_to_lsoa$pcd1), ', Unique LSOAs: ', n_distinct(pcd_to_lsoa$lsoa21cd), '\n', sep = '')

unique_pcd_imd <- unique(pcd_to_lsoa[, c('pcd1','lsoa21cd')]) %>% arrange(lsoa21cd) 
rle_upi <- rle(unique_pcd_imd$lsoa21cd)

cat('Uniquely identified LSOAs: ', length(rle_upi$lengths[rle_upi$lengths == 1]), '/', length(rle_upi$lengths), sep = '')

# IMD by LSOA
# (https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)

lsoa_imd <- data.table(read_xlsx(here::here('data','ons','File_2_-_IoD2019_Domains_of_Deprivation.xlsx'), sheet = 2) %>% 
                         select(starts_with('LSOA'),starts_with('Overall'),starts_with('Index')))
colnames(lsoa_imd) <- c('lsoa21cd','lsoa21nm','imd_rank','imd_decile')
lsoa_imd[, imd_quintile := ceiling(imd_decile/2)]

pcd_imd <- unique_pcd_imd[lsoa_imd[lsoa21cd %in% unique(unique_pcd_imd$lsoa21cd)][, c('lsoa21cd','imd_quintile')], on = 'lsoa21cd']
cat('LSOAs after adding IMD: ', n_distinct(pcd_imd$lsoa21cd), ', IMD distribution: ', paste(round(prop.table(table(pcd_imd$imd_quintile)), 3), collapse = ', '), '\n', sep = '')
# Some LSOAs missing as they were defined pre-Census but post-IMD definitions (2019-2021)
cat('LSOAs without IMD (dropped): ', length(setdiff(unique_pcd_imd$lsoa21cd, lsoa_imd$lsoa21cd)), sep = '')

## Urban/rural
# https://geoportal.statistics.gov.uk/search?q=PRD_RUC_LSOA%202021&sort=Date%20Created%7Ccreated%7Cdesc
lsoa_ur <- data.table(read_csv(here::here('data','ons','Rural_Urban_Classification_(2021)_of_LSOAs_in_EW.csv'), show_col_types = F))[, c(1, 6)]
colnames(lsoa_ur) <- c('lsoa21cd','urban_rural')

pcd_imd <- pcd_imd[lsoa_ur[lsoa21cd %in% unique(pcd_imd$lsoa21cd)], on = 'lsoa21cd']
cat('LSOAs after adding urban/rural: ', n_distinct(pcd_imd$lsoa21cd), ', R/U distribution: ', paste(round(prop.table(table(pcd_imd$urban_rural)), 3), collapse = ', '), '\n', sep = '')

## Regions of England
# https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2021-to-bua-to-lad-to-region-december-2022-best-fit-lookup-in-ew-v2/about
lsoa_to_reg <- data.table(read_csv(here::here('data','ons','lsoa_to_region.csv'), show_col_types = F))[, c(1,11)]
colnames(lsoa_to_reg) <- c('lsoa21cd','eng_reg')

pcd_imd <- pcd_imd[lsoa_to_reg[lsoa21cd %in% unique(pcd_imd$lsoa21cd)], on = 'lsoa21cd']
cat('LSOAs after adding region: ', n_distinct(pcd_imd$lsoa21cd), '\n', sep = '')


#####################
#### CENSUS DATA ####
#####################

## Census datasource:
## https://www.ons.gov.uk/datasets/create

## DATASET 1 ##

# Population: All usual residents 
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Age (17 categories), Highest level of qualification

# 35,277 out of 35,672 areas available
# Protecting personal data will prevent 395 areas from being published.

hiqual <- data.table(read_csv(here::here('data','census','raw','hiqual.csv'), show_col_types = F))
colnames(hiqual) <- c('lsoa21cd','lsoa21nm','age_cd','age_nm','hiqual_cd','hiqual_nm','population')
hiqual[hiqual_cd == -8, hiqual_nm_short := 'Not applic.']
hiqual[hiqual_cd == 0, hiqual_nm_short := 'No qualifications']
hiqual[hiqual_cd == 1, hiqual_nm_short := '1-4 GCSEs']
hiqual[hiqual_cd == 2, hiqual_nm_short := '5+ GCSEs']
hiqual[hiqual_cd == 3, hiqual_nm_short := '2+ A levels']
hiqual[hiqual_cd == 4, hiqual_nm_short := 'Degree']
hiqual[hiqual_cd == 5, hiqual_nm_short := 'Apprentice/vocational']

# merge with postcodes
hiqual_pcd1 <- data.table(full_join(hiqual, pcd_imd, by = 'lsoa21cd', relationship = 'many-to-many') %>% filter(!is.na(population), !is.na(pcd1)))

# remove categories with no observations
cd_remove <- (hiqual_pcd1 %>% group_by(hiqual_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$hiqual_cd
cd_remove <- unname(cd_remove)
if(length(cd_remove) > 0){
  hiqual_pcd1 <- hiqual_pcd1[hiqual_cd != cd_remove, ]  
}

hiqual_pcd1$hiqual_nm_short <- factor(hiqual_pcd1$hiqual_nm_short,
                                      levels = unique(hiqual_pcd1$hiqual_nm_short))

hiqual_pcd1 %>%
  group_by(age_nm, imd_quintile, eng_reg) %>%
  mutate(n = sum(population)) %>% 
  group_by(age_nm, hiqual_nm_short, imd_quintile, eng_reg, n) %>%
  summarise(s = sum(population)) %>%
  ggplot() + 
  geom_line(aes(x = age_nm, y = s/n, color = imd_quintile,
                group = imd_quintile)) + 
  theme_bw() + facet_grid(hiqual_nm_short ~ eng_reg, scales = 'free') +
  labs(y = 'Proportion', 
       x = '',
       col = 'IMD quintile') + 
  ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(here::here('output','figures','census','hiqual_age_region.png'),
       width = n_distinct(hiqual_pcd1$eng_reg)*2, height = n_distinct(hiqual_pcd1$hiqual_nm_short)*2)

## DATASET 2 ##

# Population: All usual residents 
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Age (18 categories), Ethnic group (6 categories)

# 32,643 out of 35,672 areas available
# Protecting personal data will prevent 3,029 areas from being published.

age_ethn <- data.table(read_csv(here::here('data','census','raw','age_ethn.csv'), show_col_types = F))
colnames(age_ethn) <- c('lsoa21cd','lsoa21nm','age_cd','age_nm','ethn_cd','ethn_nm','population')
age_ethn <- age_ethn[substr(lsoa21cd,1,1) == 'E']

# merge with postcodes
age_ethn_pcd1 <- full_join(age_ethn, pcd_imd, by = 'lsoa21cd', relationship = 'many-to-many')
age_ethn_pcd1 <- data.table(age_ethn_pcd1 %>% filter(!is.na(population), !is.na(pcd1)))

# remove categories with no observations ('Does not apply')
cd_remove <- (age_ethn_pcd1 %>% group_by(ethn_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$ethn_cd
cd_remove <- unname(cd_remove)
if(length(cd_remove) > 0){
  age_ethn_pcd1 <- age_ethn_pcd1[ethn_cd != cd_remove, ]
}

age_ethn_pcd1 <- age_ethn_pcd1 %>%
  rename(age_grp_6 = age_nm) %>% 
  mutate(p_ethnicity = case_when(
    ethn_nm %like% 'Asian' ~ 'Asian',
    ethn_nm %like% 'Black' ~ 'Black',
    ethn_nm %like% 'Mixed' ~ 'Mixed',
    ethn_nm %like% 'Other' ~ 'Other',
    ethn_nm %like% 'White' ~ 'White',
  )) 

age_ethn_pcd1 %>%
  group_by(age_grp_6, imd_quintile, eng_reg) %>%
  mutate(n = sum(population)) %>% 
  group_by(age_grp_6, p_ethnicity, imd_quintile, eng_reg, n) %>%
  summarise(s = sum(population)) %>%
  ggplot() + 
  geom_line(aes(x = age_grp_6, y = s/n, color = imd_quintile,
                group = imd_quintile)) + 
  theme_bw() + facet_grid(p_ethnicity ~ eng_reg, scales = 'free') + 
  labs(y = 'Proportion', 
       x = '',
       col = 'IMD quintile') + ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(here::here('output','figures','census','ethn_age_region.png'),
       width = n_distinct(age_ethn_pcd1$eng_reg)*2, height = n_distinct(age_ethn_pcd1$p_ethnicity)*2)

## DATASET 3 ##

# Population: All households
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Household size, Tenure of household
# Choose 7 categories for household size

# 31,502 out of 35,672 areas available
# Protecting personal data will prevent 4,170 areas from being published.

hh_st <- data.table(read_csv(here::here('data','census','raw','household_size_tenure.csv'), show_col_types = F))
colnames(hh_st) <- c('lsoa21cd','lsoa21nm','hh_size_cd','hh_size_nm','hh_tenure_cd','hh_tenure_nm','n_obs')
hh_st <- hh_st[lsoa21cd %in% unique(pcd_imd$lsoa21cd),]

# merge with postcodes
hh_st_pcd1 <- full_join(hh_st, pcd_imd, by = 'lsoa21cd', relationship = 'many-to-many') %>% 
  mutate(population = hh_size_cd*n_obs)
hh_st_pcd1 <- data.table(hh_st_pcd1 %>% filter(!is.na(population), !is.na(pcd1)))

# remove categories with no observations ('Does not apply')
cd_size_remove <- unname((hh_st_pcd1 %>% group_by(hh_size_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$hh_size_cd)
cd_ten_remove <- unname((hh_st_pcd1 %>% group_by(hh_tenure_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$hh_tenure_cd)
if(length(cd_size_remove) > 0){
  hh_st_pcd1 <- hh_st_pcd1[hh_size_cd != cd_size_remove, ]
}
if(length(cd_ten_remove) > 0){
  hh_st_pcd1 <- hh_st_pcd1[hh_tenure_cd != cd_ten_remove, ]
}

hh_st_pcd1 %>%
  group_by(hh_size_nm, imd_quintile, eng_reg) %>%
  mutate(n = sum(population)) %>% 
  group_by(hh_size_nm, hh_tenure_nm, imd_quintile, eng_reg, n) %>%
  summarise(s = sum(population)) %>%
  ggplot() + 
  geom_line(aes(x = hh_size_nm, y = s/n, color = imd_quintile,
                group = imd_quintile)) + 
  theme_bw() + facet_grid(hh_tenure_nm ~ eng_reg, scales = 'free') + 
  labs(y = 'Proportion', 
       x = '',
       col = 'IMD quintile') + ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(here::here('output','figures','census','hh_size_tenure_region.png'),
       width = n_distinct(hh_st_pcd1$hh_size_nm)*2, height = n_distinct(hh_st_pcd1$hh_tenure_nm)*2)

## DATASET 5 ##

# Population: All usual residents 
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Age (8 categories), National Statistics Socio-economic Classification (NS-SeC),
# Highest level of qualification

# 28,545 out of 35,672 areas available
# Protecting personal data will prevent 7,127 areas from being published.

ns_hq <- data.table(read_csv(here::here('data','census','raw','ns_hq.csv'), show_col_types = F))
colnames(ns_hq) <- c('lsoa21cd','lsoa21nm','nssec_cd','nssec_nm','age_cd','age_nm','hiqual_cd','hiqual_nm','population')
ns_hq[hiqual_cd == -8, hiqual_nm_short := 'Not applic.']
ns_hq[hiqual_cd == 0, hiqual_nm_short := 'No qualifications']
ns_hq[hiqual_cd == 1, hiqual_nm_short := '1-4 GCSEs']
ns_hq[hiqual_cd == 2, hiqual_nm_short := '5+ GCSEs']
ns_hq[hiqual_cd == 3, hiqual_nm_short := '2+ A levels']
ns_hq[hiqual_cd == 4, hiqual_nm_short := 'Degree']
ns_hq[hiqual_cd == 5, hiqual_nm_short := 'Apprentice/vocational']

# merge with postcodes
ns_hq_pcd1 <- full_join(ns_hq, pcd_imd, by = 'lsoa21cd', relationship = 'many-to-many')
ns_hq_pcd1 <- data.table(ns_hq_pcd1 %>% filter(!is.na(population), !is.na(pcd1)))

# remove categories with no observations ('Does not apply')
cd_remove <- (ns_hq_pcd1 %>% group_by(nssec_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$nssec_cd
cd_remove <- unname(cd_remove)
if(length(cd_remove) > 0){
  ns_hq_pcd1 <- ns_hq_pcd1[nssec_cd != cd_remove, ]
}
cd_remove <- (ns_hq_pcd1 %>% group_by(hiqual_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$hiqual_cd
cd_remove <- unname(cd_remove)
if(length(cd_remove) > 0){
  ns_hq_pcd1 <- ns_hq_pcd1[hiqual_cd != cd_remove, ]
}

ns_hq_pcd1 <- ns_hq_pcd1 %>% 
  rename(age_grp_8 = age_nm) %>% 
  rename(p_hiqual = hiqual_nm_short) %>% 
  mutate(p_sec_input = case_when(
    nssec_nm %like% 'L3' ~ '1',
    nssec_nm %like% 'L4' ~ '2',
    nssec_nm %like% 'L7' ~ '3',
    nssec_nm %like% 'L8' ~ '4',
    nssec_nm %like% 'L10' ~ '5',
    nssec_nm %like% 'L12' ~ '6',
    nssec_nm %like% 'L13' ~ '7',
    nssec_nm %like% 'student' ~ 'Student',
    nssec_nm %like% 'unemployed' ~ 'Unemployed', # this is an assumption
    T ~ 'Not applic.'
  )) 

ns_hq_pcd1 %>%
  group_by(age_grp_8, imd_quintile, p_hiqual) %>%
  mutate(n = sum(population)) %>% 
  group_by(age_grp_8, p_sec_input, imd_quintile, p_hiqual, n) %>%
  summarise(s = sum(population)) %>%
  filter(n > 0, p_hiqual != 'Not applic.', p_sec_input != 'Does not apply') %>% 
  ggplot() + 
  geom_line(aes(x = age_grp_8, y = s/n, color = imd_quintile, 
                group = imd_quintile)) + 
  theme_bw() + facet_grid(p_sec_input ~ p_hiqual, scales = 'free') + 
  labs(y = 'Proportion', 
       x = '',
       col = 'IMD quintile') + ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(here::here('output','figures','census','nssec_hiqual_age.png'),
       width = n_distinct(ns_hq_pcd1$age_grp_8)*2, height = n_distinct(ns_hq_pcd1$p_sec_input)*2)

## DATASET 6 ##

# Population: All usual residents 
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Age (18 categories)

# All areas available 

age_dt <- data.table(read_csv(here::here('data','census','raw','age.csv'), show_col_types = F))
colnames(age_dt) <- c('lsoa21cd','lsoa21nm','age_cd','age_nm','population')

# merge with postcodes
age_pcd1 <- full_join(age_dt, pcd_imd, by = 'lsoa21cd', relationship = 'many-to-many')
age_pcd1 <- age_pcd1 %>% filter(!is.na(population), !is.na(pcd1)) %>% 
  rename(age_grp = age_nm) %>% 
  mutate(age_grp = case_when(
    age_grp %like% '75|80|85' ~ 'Aged 75+',
    T ~ age_grp
  ))

age_pcd1$age_grp <- factor(age_pcd1$age_grp,
                          levels = unique(age_pcd1$age_grp))
  
age_pcd1 %>%
  group_by(imd_quintile, eng_reg) %>%
  mutate(n = sum(population)) %>% 
  group_by(age_grp, imd_quintile, eng_reg, n) %>%
  summarise(s = sum(population)) %>%
  ggplot() + 
  geom_line(aes(x = age_grp, y = s/n, color = imd_quintile, 
                group = imd_quintile)) + 
  theme_bw() + facet_wrap(eng_reg~., scales = 'free') + 
  labs(y = 'Proportion', 
       x = '',
       col = 'IMD quintile') + ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(here::here('output','figures','census','age_region.png'),
       width = 15, height = 12)

## DATASET 7 ##

# FROM UK DATA SERVICE BULK DOWNLOAD

# Population: All usual residents 
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Tenure and Ethnicity of Household Reference Persons

# All areas available 

ethn_tenure <- data.table(read_csv(here::here('data','census','raw','ukds_tenure_ethnicity.csv'), show_col_types = F))
colnames(ethn_tenure) <- c('lsoa21cd','lsoa21nm','tenure_cd','tenure_nm','ethn_cd','ethn_nm','population')

# merge with postcodes
ethn_tenure_pcd1 <- full_join(ethn_tenure, pcd_imd, by = 'lsoa21cd', relationship = 'many-to-many')
ethn_tenure_pcd1 <- ethn_tenure_pcd1 %>% filter(!is.na(population), !is.na(pcd1)) 

# remove categories with no observations ('Does not apply')
cd_remove <- (ethn_tenure_pcd1 %>% group_by(tenure_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$tenure_cd
cd_remove <- unname(cd_remove)
if(length(cd_remove) > 0){
  ethn_tenure_pcd1 <- ethn_tenure_pcd1[tenure_cd != cd_remove, ]
}
cd_remove <- (ethn_tenure_pcd1 %>% group_by(ethn_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$ethn_cd
cd_remove <- unname(cd_remove)
if(length(cd_remove) > 0){
  ethn_tenure_pcd1 <- ethn_tenure_pcd1[ethn_cd != cd_remove, ]
}

ethn_tenure_pcd1 <- ethn_tenure_pcd1 %>% 
  mutate(p_ethnicity = case_when(
    ethn_nm %like% 'White' ~ 'White',
    ethn_nm %like% 'Black' ~ 'Black',
    ethn_nm %like% 'Asian' ~ 'Asian',
    ethn_nm %like% 'Mixed' ~ 'Mixed',
    ethn_nm %like% 'Other ethnic' ~ 'Other'
  )) %>% 
  rename(p_tenure_short = tenure_nm)

ethn_tenure_pcd1 %>%
  group_by(p_ethnicity, imd_quintile, eng_reg) %>%
  mutate(n = sum(population)) %>% 
  group_by(p_ethnicity, p_tenure_short, imd_quintile, eng_reg, n) %>%
  summarise(s = sum(population)) %>%
  ggplot() + 
  geom_bar(aes(x = p_ethnicity, y = s, fill = imd_quintile),
           position = 'fill', stat = 'identity') + 
  theme_bw() + facet_grid(p_tenure_short~eng_reg, scales = 'free') + 
  labs(y = 'Proportion', 
       x = '',
       fill = 'IMD quintile') + ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggsave(here::here('output','figures','census','tenure_ethn_region.png'),
       width = 2*n_distinct(ethn_tenure_pcd1$p_ethnicity), height = 2*n_distinct(ethn_tenure_pcd1$p_tenure_short))

###############################
###############################
## SAVE DATA AS MODEL INPUTS ##
###############################
###############################

write_csv(age_pcd1, here::here('data','census','pcd1age.csv'))
write_csv(age_pcd1, here::here('data','census','pcd1.csv'))
write_csv(age_pcd1, here::here('data','census','engreg.csv'))

write_csv(hh_st_pcd1, here::here('data','census','pcd1household.csv'))

write_csv(ns_hq_pcd1, here::here('data','census','pcd1agehiqualnssec.csv'))

write_csv(age_ethn_pcd1, here::here('data','census','pcd1ageethn.csv'))

write_csv(ethn_tenure_pcd1, here::here('data','census','pcd1ethntenure.csv'))

####################################
####################################
## MAKE VARIABLES IN CONNECT_PART ##
####################################
####################################

connect_part <- readRDS(here::here('data','connect','connect_part.rds'))

if('age_lower' %notin% colnames(connect_part)){
  connect_part <- connect_part %>% 
    mutate(p_age_group_2 = case_when(
      p_age_group %like% '75+' ~ '75+-',
      T ~ p_age_group
    )) %>% 
    separate_wider_delim(p_age_group_2, delim = '-', 
                         names = c('age_lower','age_upper'))
}


connect_input <- connect_part %>% 
  mutate(hh_tenure_nm = case_when(
    p_tenure == 'Owned outright' ~ "Owned: Owns outright",
    p_tenure == 'Rented from private landlord or letting agency' ~ "Private rented: Private landlord or letting agency",
    p_tenure == 'Rented from Council (Local Authority)' ~ "Social rented: Rents from council or Local Authority",
    p_tenure %in% c('Owned with a mortgage or loan',
                    'Shared ownership - with a mortgage and paying rent') ~ "Owned: Owns with a mortgage or loan or shared ownership",
    p_tenure == 'Rented from housing association, housing co-operative, charitable trust, or registered social landlord' ~ "Social rented: Other social rented",
    p_tenure %in% c('Living rent free',
                    'Tied accommodation (accommodation provided by employer of a household member)',
                    'Rented from a relative or friend of household member',
                    'Other private rented') ~ "Private rented: Other private rented or lives rent free",
  )) %>% 
  mutate(p_tenure_short = case_when(
    hh_tenure_nm == "Owned: Owns outright" ~ hh_tenure_nm,
    hh_tenure_nm %in% c("Social rented: Rents from council or Local Authority","Social rented: Other social rented") ~ 'Rented: Social rented',
    hh_tenure_nm == "Owned: Owns with a mortgage or loan or shared ownership" ~ hh_tenure_nm,
    hh_tenure_nm %in% c("Private rented: Other private rented or lives rent free",
                        "Private rented: Private landlord or letting agency") ~ 'Private rented or lives rent free'
  )) %>% 
  mutate(hh_size_nm = case_when(
    household_members == '0' ~ "1 person in household",
    household_members %in% as.character(1:4) ~ paste0(as.numeric(household_members) + 1, ' people in household'),
    as.numeric(household_members) > 4 ~ "6 or more people in household"
  )) %>% 
  mutate(eng_reg = case_when(
    p_engreg %like% 'London' ~ 'London',
    p_engreg %like% 'Yorkshire' ~ 'Yorkshire and The Humber',
    T ~ p_engreg
  )) %>% 
  mutate(age_grp = case_when(
    p_age_group == '0-4' ~ 'Aged 4 years and under',
    p_age_group == '75+' ~ 'Aged 75+',
    T ~ paste0('Aged ', age_lower, ' to ', age_upper, ' years')
    ),
    age_grp_8 = case_when(
      p_age < 16 ~ "Aged 15 years and under",
      p_age %in% 16:24 ~ "Aged 16 to 24 years",
      p_age %in% 25:34 ~ "Aged 25 to 34 years",
      p_age %in% 35:44 ~ "Aged 35 to 44 years",
      p_age %in% 45:54 ~ "Aged 45 to 54 years",
      p_age %in% 55:64 ~ "Aged 55 to 64 years",
      p_age %in% 65:74 ~ "Aged 65 to 74 years",  
      p_age >= 75 ~ "Aged 75 years and over"
    ),
    age_grp_6 = case_when(
      p_age < 16 ~ "Aged 15 years and under",
      p_age %in% 16:24 ~ "Aged 16 to 24 years",
      p_age %in% 25:34 ~ "Aged 25 to 34 years",
      p_age %in% 35:49 ~ "Aged 35 to 49 years",
      p_age %in% 50:64 ~ "Aged 50 to 64 years",
      p_age >= 65 ~ "Aged 65 years and over"
    )) %>% 
  mutate(p_hiqual = case_when(
    p_hiqual %like% 'Level 1' ~ '1-4 GCSEs',
    p_hiqual %like% 'Level 2' ~ '5+ GCSEs',
    p_hiqual %like% 'Level 3' ~ '2+ A levels',
    p_hiqual %like% 'Level 4' ~ 'Degree',
    p_hiqual %like% 'Apprent' ~ 'Apprentice/vocational',
    p_hiqual %like% 'Child' ~ 'Not applic.',
    p_hiqual == 'Does not apply' ~ 'Not applic.',
    T ~ p_hiqual
  )) %>% 
  mutate(p_sec_input = case_when(
    p_sec_input %like% 'Under 17' ~ 'Not applic.',
    p_sec_input %like% 'Retired' ~ 'Not applic.',
    p_sec_input %like% 'Unknown' ~ NA,
    T ~ p_sec_input
  ))

write_rds(connect_input, here::here('data','connect','connect_part.rds'))

cat('Time taken: ', 
    floor(difftime(Sys.time(), time, units = 'secs')[[1]]/60), ' mins ',
    round(difftime(Sys.time(), time, units = 'secs')[[1]] - 
            60*floor(difftime(Sys.time(), time, units = 'secs')[[1]]/60)), ' secs',
    sep = '')






