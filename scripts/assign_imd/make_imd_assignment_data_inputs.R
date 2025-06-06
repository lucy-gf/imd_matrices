
#### MAKE INPUTS ####

hh_input <- hh_st %>%
  left_join(unique(pcd_imd %>% select(pcd1, lsoa21cd, imd_quintile, urban_rural, eng_reg)),
            by = 'lsoa21cd', relationship = 'many-to-many') %>%
  mutate(population = hh_size_cd*n_obs) %>% # scale up by household size for population sizes
  filter(hh_tenure_cd != -8,
         population != 0)

age_input <- copy(pcd_imd)

ns_hq_input <- ns_hq_pcd1 %>% 
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
    T ~ NA
  )) 

ethn_input <- age_ethn_pcd1 %>%
  rename(age_grp_6 = age_nm) %>% 
  mutate(p_ethnicity = case_when(
    ethn_nm %like% 'Asian' ~ 'Asian',
    ethn_nm %like% 'Black' ~ 'Black',
    ethn_nm %like% 'Mixed' ~ 'Mixed',
    ethn_nm %like% 'Other' ~ 'Other',
    ethn_nm %like% 'White' ~ 'White',
  )) 

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
  mutate(age_grp = p_age_group,
         age_grp_8 = case_when(
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
    T ~ p_hiqual
  ))
