
######################################
## TESTING METHODS OF INFERRING IMD ##
### FROM PCD1 AND CENSUS VARIABLES ###
######################################

## Training dataset: England population

## Testing dataset: Connect participants

## Variables of interest:
# 1. First half of postcode (pcd1)
# 2. Age group
# 3. Ethnicity? Likely not
# 4. Housing tenure
# 5. Household income (adults only)
# 6. Highest qualification (adults only)
# 7. Household size

## LSOA data (England only as ONS IMD not comparable across devolved nations)

# Matching postcodes to LSOAs
# https://geoportal.statistics.gov.uk/datasets/3770c5e8b0c24f1dbe6d2fc6b46a0b18 
pcd_to_lsoa <- data.table(read_csv(here::here('data','ons','PCD_OA21_LSOA21_MSOA21_LAD_AUG23_UK_LU.csv'), show_col_types = F) %>% 
                            select(starts_with('pcd'),'lsoa21cd'))
pcd_imd <- pcd_to_lsoa %>% 
  separate_wider_delim(pcds, delim = " ", names = c("pcd1", "pcd2")) 

pcd_imd <- unique(data.table(pcd_imd[,c('pcd1','lsoa21cd')]))
pcd_imd <- pcd_imd[substr(lsoa21cd, 1, 1) == 'E',]

cat('Unique pcd1s: ', n_distinct(pcd_imd$pcd1), ', Unique LSOAs: ', n_distinct(pcd_imd$lsoa21cd), '\n', sep = '')

# add populations (Mid-2022)
# (https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates)

ew_pop <- data.table(read_xlsx(here::here('data','ons','sapelsoasyoa20192022.xlsx'), sheet = 8, skip = 3))
ew_pop <- ew_pop[substr(`LSOA 2021 Code`, 1, 1) == 'E', ]
colnames(ew_pop) <- c('lad21cd','lad21nm','lsoa21cd','lsoa21nm','total', paste0('F',as.character(0:90)), paste0('M',as.character(0:90)))

ew_pop_l <- melt(ew_pop, id.vars = c('lad21cd','lad21nm','lsoa21cd','lsoa21nm','total'))
ew_pop_l[, gender := substr(variable,1,1)]
ew_pop_l[, age := as.numeric(gsub("\\D", "", variable))]
ew_pop_l[, lower := age - (age %% 5)]
ew_pop_l[lower > 75, lower := 75]
ew_pop_l[, age_grp := paste0(lower, '-', lower + 4)]
ew_pop_l[lower == 75, age_grp := '75+']

ew_pop_agg <- ew_pop_l[, c('lsoa21cd','age_grp','value')][, lapply(.SD, sum), by = c('lsoa21cd','age_grp')]
setnames(ew_pop_agg, 'value', 'population')

pcd_imd <- full_join(pcd_imd, ew_pop_agg, relationship = 'many-to-many')
cat('Total population (millions): ', round(sum(pcd_imd$population)/1e6, 1), ', LSOAs spanning multiple pcd1s: ', 
    nrow(data.table(table(pcd_imd$lsoa21cd))[N > 1]), '/', n_distinct(pcd_imd$lsoa21cd), '\n', sep = '')
# Population will be greater than Eng pop where one LSOA spans 2 pcd1s

# IMD by LSOA
# (https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019)

lsoa_imd <- data.table(read_xlsx(here::here('data','ons','File_2_-_IoD2019_Domains_of_Deprivation.xlsx'), sheet = 2) %>% 
                         select(starts_with('LSOA'),starts_with('Overall'),starts_with('Index')))
colnames(lsoa_imd) <- c('lsoa21cd','lsoa21nm','imd_rank','imd_decile')
lsoa_imd[, imd_quintile := ceiling(imd_decile/2)]

pcd_imd <- pcd_imd[lsoa_imd[lsoa21cd %in% unique(pcd_imd$lsoa21cd)][, c('lsoa21cd','imd_quintile')], on = 'lsoa21cd']
cat('Unique LSOAs: ', n_distinct(pcd_imd$lsoa21cd), ', IMD distribution: ', paste(round(prop.table(table(pcd_imd$imd_quintile)), 3), collapse = ', '), '\n', sep = '')
# TODO - some missing, fix?

## Urban/rural
# https://geoportal.statistics.gov.uk/search?q=PRD_RUC_LSOA%202021&sort=Date%20Created%7Ccreated%7Cdesc
lsoa_ur <- data.table(read_csv(here::here('data','ons','Rural_Urban_Classification_(2021)_of_LSOAs_in_EW.csv'), show_col_types = F))[, c(1, 6)]
colnames(lsoa_ur) <- c('lsoa21cd','urban_rural')

pcd_imd <- pcd_imd[lsoa_ur[lsoa21cd %in% unique(pcd_imd$lsoa21cd)], on = 'lsoa21cd']

## Regions of England
# https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2021-to-bua-to-lad-to-region-december-2022-best-fit-lookup-in-ew-v2/about
lsoa_to_reg <- data.table(read_csv(here::here('data','ons','lsoa_to_region.csv'), show_col_types = F))[, c(1,11)]
colnames(lsoa_to_reg) <- c('lsoa21cd','eng_reg')

pcd_imd <- pcd_imd[lsoa_to_reg[lsoa21cd %in% unique(pcd_imd$lsoa21cd)], on = 'lsoa21cd']



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

hiqual <- data.table(read_csv(here::here('data','census','hiqual.csv'), show_col_types = F))
colnames(hiqual) <- c('lsoa21cd','lsoa21nm','age_cd','age_nm','hiqual_cd','hiqual_nm','population')
hiqual[hiqual_cd == 0, hiqual_nm_short := 'No qualifications']
hiqual[hiqual_cd == 1, hiqual_nm_short := '1-4 GCSEs']
hiqual[hiqual_cd == 2, hiqual_nm_short := '5+ GCSEs']
hiqual[hiqual_cd == 3, hiqual_nm_short := '2+ A levels']
hiqual[hiqual_cd == 4, hiqual_nm_short := 'Degree']
hiqual[hiqual_cd == 5, hiqual_nm_short := 'Apprentice/vocational']

# merge with postcodes
hiqual_pcd1 <- full_join(hiqual, unique(pcd_imd[, c('pcd1','lsoa21cd','imd_quintile','urban_rural','eng_reg')]), by = 'lsoa21cd', relationship = 'many-to-many')
hiqual_pcd1 <- data.table(hiqual_pcd1 %>% filter(!is.na(population), !is.na(pcd1)))

# only using hiqual for 18+:
hiqual_pcd1 <- hiqual_pcd1[age_cd > 2, ]

# remove hiqual categories with no observations ('Does not apply')
cd_remove <- (hiqual_pcd1 %>% group_by(hiqual_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$hiqual_cd
cd_remove <- unname(cd_remove)
hiqual_pcd1 <- hiqual_pcd1[hiqual_cd != cd_remove, ]

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
  labs(y = 'Proportion') + ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## DATASET 2 ##

# Population: All usual residents 
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Age (18 categories), Ethnic group (6 categories)

# 32,643 out of 35,672 areas available
# Protecting personal data will prevent 3,029 areas from being published.

age_ethn <- data.table(read_csv(here::here('data','census','age_ethn.csv'), show_col_types = F))
colnames(age_ethn) <- c('lsoa21cd','lsoa21nm','age_cd','age_nm','ethn_cd','ethn_nm','population')
age_ethn <- age_ethn[substr(lsoa21cd,1,1) == 'E']

# merge with postcodes
age_ethn_pcd1 <- full_join(age_ethn, unique(pcd_imd[, c('pcd1','lsoa21cd','imd_quintile','urban_rural','eng_reg')]), by = 'lsoa21cd', relationship = 'many-to-many')
age_ethn_pcd1 <- data.table(age_ethn_pcd1 %>% filter(!is.na(population), !is.na(pcd1)))

# remove hiqual categories with no observations ('Does not apply')
cd_remove <- (age_ethn_pcd1 %>% group_by(ethn_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$ethn_cd
cd_remove <- unname(cd_remove)
age_ethn_pcd1 <- age_ethn_pcd1[ethn_cd != cd_remove, ]

age_ethn_pcd1 %>%
  group_by(age_nm, imd_quintile, eng_reg) %>%
  mutate(n = sum(population)) %>% 
  group_by(age_nm, ethn_nm, imd_quintile, eng_reg, n) %>%
  summarise(s = sum(population)) %>%
  ggplot() + 
  geom_line(aes(x = age_nm, y = s/n, color = imd_quintile,
                group = imd_quintile)) + 
  theme_bw() + facet_grid(ethn_nm ~ eng_reg, scales = 'free') + 
  labs(y = 'Proportion') + ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


## DATASET 3 ##

# Population: All households
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Household size, Tenure of household
# Choose 7 categories for household size

# 31,502 out of 35,672 areas available
# Protecting personal data will prevent 4,170 areas from being published.

hh_st <- data.table(read_csv(here::here('data','census','household_size_tenure.csv'), show_col_types = F))
colnames(hh_st) <- c('lsoa21cd','lsoa21nm','hh_size_cd','hh_size_nm','hh_tenure_cd','hh_tenure_nm','n_obs')
hh_st <- hh_st[lsoa21cd %in% unique(pcd_imd$lsoa21cd),]


## DATASET 4 ##

# Population: All usual residents 
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Age (8 categories), National Statistics Socio-economic Classification (NS-SeC)

# 35,647 out of 35,672 areas available
# Protecting personal data will prevent 25 areas from being published.

nssec <- data.table(read_csv(here::here('data','census','nssec.csv'), show_col_types = F))
colnames(nssec) <- c('lsoa21cd','lsoa21nm','nssec_cd','nssec_nm','age_cd','age_nm','population')

# merge with postcodes
nssec_pcd1 <- full_join(nssec, unique(pcd_imd[, c('pcd1','lsoa21cd','imd_quintile','urban_rural','eng_reg')]), by = 'lsoa21cd', relationship = 'many-to-many')
nssec_pcd1 <- data.table(nssec_pcd1 %>% filter(!is.na(population), !is.na(pcd1)))

# only using nssec for 16+:
nssec_pcd1 <- nssec_pcd1[age_cd > 1, ]

# remove nssec categories with no observations ('Does not apply')
cd_remove <- (nssec_pcd1 %>% group_by(nssec_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$nssec_cd
cd_remove <- unname(cd_remove)
nssec_pcd1 <- nssec_pcd1[nssec_cd != cd_remove, ]

nssec_pcd1$nssec_nm <- factor(nssec_pcd1$nssec_nm, 
                              levels = unique(nssec_pcd1$nssec_nm))

nssec_pcd1 %>%
  group_by(age_nm, imd_quintile, eng_reg) %>%
  mutate(n = sum(population)) %>% 
  group_by(age_nm, nssec_nm, imd_quintile, eng_reg, n) %>%
  summarise(s = sum(population)) %>%
  ggplot() + 
  geom_line(aes(x = age_nm, y = s/n, color = imd_quintile,
                group = imd_quintile)) + 
  theme_bw() + facet_grid(nssec_nm ~ eng_reg, scales = 'free') +
  labs(y = 'Proportion') + ylim(c(0,NA)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

## DATASET 5 ##

# Population: All usual residents 
# Area type: Lower layer Super Output Area
# Coverage: England and Wales

# Variables: Age (8 categories), National Statistics Socio-economic Classification (NS-SeC),
# Highest level of qualification

# 28,545 out of 35,672 areas available
# Protecting personal data will prevent 7,127 areas from being published.

ns_hq <- data.table(read_csv(here::here('data','census','ns_hq.csv'), show_col_types = F))
colnames(ns_hq) <- c('lsoa21cd','lsoa21nm','nssec_cd','nssec_nm','age_cd','age_nm','hiqual_cd','hiqual_nm','population')
ns_hq[hiqual_cd == 0, hiqual_nm_short := 'No qualifications']
ns_hq[hiqual_cd == 1, hiqual_nm_short := '1-4 GCSEs']
ns_hq[hiqual_cd == 2, hiqual_nm_short := '5+ GCSEs']
ns_hq[hiqual_cd == 3, hiqual_nm_short := '2+ A levels']
ns_hq[hiqual_cd == 4, hiqual_nm_short := 'Degree']
ns_hq[hiqual_cd == 5, hiqual_nm_short := 'Apprentice/vocational']

# merge with postcodes
ns_hq_pcd1 <- full_join(ns_hq, unique(pcd_imd[, c('pcd1','lsoa21cd','imd_quintile','urban_rural','eng_reg')]), by = 'lsoa21cd', relationship = 'many-to-many')
ns_hq_pcd1 <- data.table(ns_hq_pcd1 %>% filter(!is.na(population), !is.na(pcd1)))

# only using for 16+:
ns_hq_pcd1 <- ns_hq_pcd1[age_cd > 1, ]

# remove categories with no observations ('Does not apply')
cd_remove <- (ns_hq_pcd1 %>% group_by(nssec_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$nssec_cd
cd_remove <- unname(cd_remove)
ns_hq_pcd1 <- ns_hq_pcd1[nssec_cd != cd_remove, ]
cd_remove <- (ns_hq_pcd1 %>% group_by(hiqual_cd) %>% summarise(s = sum(population)) %>% filter(s == 0))$hiqual_cd
cd_remove <- unname(cd_remove)
ns_hq_pcd1 <- ns_hq_pcd1[hiqual_cd != cd_remove, ]


## DATASET 6 ##

hh_stc <- data.table(read_csv(here::here('data','census','household_size_tenure_composition.csv'), show_col_types = F))
colnames(hh_stc) <- c('lsoa21cd','lsoa21nm','hh_comp_cd','hh_comp_nm','hh_tenure_cd','hh_tenure_nm','hh_size_cd','hh_size_nm','n_obs')
hh_stc <- hh_stc[lsoa21cd %in% unique(pcd_imd$lsoa21cd),]

hh_stc <- hh_stc %>%
  left_join(unique(pcd_imd %>% select(pcd1, lsoa21cd, imd_quintile, urban_rural, eng_reg)),
            by = 'lsoa21cd', relationship = 'many-to-many') %>%
  mutate(population = hh_size_cd*n_obs) %>% # scale up by household size for population sizes
  filter(hh_tenure_cd != -8,
         hh_size_cd != -8,
         hh_comp_cd != -8)


## DATASET 7 ##

hh_tc <- data.table(read_csv(here::here('data','census','household_tenure_composition.csv'), show_col_types = F))
colnames(hh_tc) <- c('lsoa21cd','lsoa21nm','hh_comp_cd','hh_comp_nm','hh_tenure_cd','hh_tenure_nm','n_obs')
hh_tc <- hh_tc[lsoa21cd %in% unique(pcd_imd$lsoa21cd),]

hh_tc <- hh_tc %>%
  left_join(unique(pcd_imd %>% select(pcd1, lsoa21cd, imd_quintile, urban_rural, eng_reg)),
            by = 'lsoa21cd', relationship = 'many-to-many') %>%
  filter(hh_tenure_cd != -8,
         hh_comp_cd != -8)














