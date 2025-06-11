
# ## english region distribution
# true_vals_engreg <- pcd_imd %>% 
#   group_by(eng_reg, imd_quintile) %>% 
#   summarise(n = sum(population)) %>% 
#   group_by(eng_reg) %>% 
#   mutate(n_tot = sum(n),
#          prop = n/n_tot) %>% 
#   rename(p_engreg = eng_reg) %>% 
#   mutate(p_engreg = case_when(
#     p_engreg %like% 'London' ~ 'Greater London',
#     p_engreg %like% 'Yorkshire' ~ 'Yorkshire and the Humber',
#     T ~ p_engreg
#   ))
# 
# ## age distribution
# 
# true_vals_age <- pcd_imd %>% 
#   group_by(age_grp, imd_quintile) %>% 
#   summarise(n = sum(population)) %>% 
#   group_by(age_grp) %>% 
#   mutate(n_tot = sum(n),
#          prop = n/n_tot) %>% 
#   rename(p_age_group = age_grp) 
# 
# ## household tenure distribution
# 
# true_vals_hh_tenure <- hh_input %>% 
#   group_by(hh_tenure_nm, imd_quintile) %>% 
#   summarise(n = sum(population)) %>% 
#   group_by(hh_tenure_nm) %>% 
#   mutate(n_tot = sum(n),
#          prop = n/n_tot)
# 
# ## household size distribution
# 
# true_vals_hh_size <- hh_input %>% 
#   group_by(hh_size_nm, imd_quintile) %>% 
#   summarise(n = sum(population)) %>% 
#   group_by(hh_size_nm) %>% 
#   mutate(n_tot = sum(n),
#          prop = n/n_tot)
# 
# ## ethnicity distribution
# 
# true_vals_ethnicity <- age_ethn_pcd1 %>% 
#   group_by(ethn_nm, imd_quintile) %>% 
#   summarise(n = sum(population)) %>% 
#   group_by(ethn_nm) %>% 
#   mutate(n_tot = sum(n),
#          prop = n/n_tot) %>% 
#   rename(p_ethnicity = ethn_nm) %>% 
#   mutate(p_ethnicity = case_when(
#     p_ethnicity %like% 'Asian' ~ 'Asian',
#     p_ethnicity %like% 'Black' ~ 'Black',
#     p_ethnicity %like% 'Mixed' ~ 'Mixed',
#     p_ethnicity %like% 'White' ~ 'White',
#     T ~ 'Other'
#   ))
# 
# ## highest qualification distribution
# 
# true_vals_hiqual <- hiqual_pcd1 %>% 
#   group_by(hiqual_nm_short, imd_quintile) %>% 
#   summarise(n = sum(population)) %>% 
#   group_by(hiqual_nm_short) %>% 
#   mutate(n_tot = sum(n),
#          prop = n/n_tot) %>% 
#   rename(p_hiqual = hiqual_nm_short) 
# 
# 
# ## nssec distribution
# 
# true_vals_nssec <- ns_hq_pcd1 %>%
#   group_by(nssec_nm, imd_quintile) %>%
#   summarise(n = sum(population)) %>%
#   group_by(nssec_nm) %>%
#   mutate(n_tot = sum(n),
#          prop = n/n_tot) %>%
#   rename(p_sec_input = nssec_nm) %>%
#   mutate(p_sec_input = case_when(
#     p_sec_input %like% 'L1, L2' ~ '1',
#     p_sec_input %like% 'L4' ~ '2',
#     p_sec_input %like% 'L7' ~ '3',
#     p_sec_input %like% 'L8' ~ '4',
#     p_sec_input %like% 'L11' ~ '5',
#     p_sec_input %like% 'L12' ~ '6',
#     p_sec_input %like% 'L13' ~ '7',
#     T ~ NA
#   ))
# 
# ## urban/rural
# 
# true_vals_urban <- pcd_imd %>% 
#   group_by(urban_rural, imd_quintile) %>% 
#   summarise(n = sum(population)) %>% 
#   group_by(urban_rural) %>% 
#   mutate(n_tot = sum(n),
#          prop = n/n_tot) %>% 
#   rename(p_urban_rural = urban_rural)
# 
# # save
# write_csv(true_vals_engreg, 
#           here::here('data','census','true_vals','true_vals_engreg.csv'))
# write_csv(true_vals_age, 
#           here::here('data','census','true_vals','true_vals_age.csv'))
# write_csv(true_vals_hh_tenure, 
#           here::here('data','census','true_vals','true_vals_hh_tenure.csv'))
# write_csv(true_vals_hh_size, 
#           here::here('data','census','true_vals','true_vals_hh_size.csv'))
# write_csv(true_vals_ethnicity, 
#           here::here('data','census','true_vals','true_vals_ethnicity.csv'))
# write_csv(true_vals_hiqual, 
#           here::here('data','census','true_vals','true_vals_hiqual.csv'))
# write_csv(true_vals_nssec,
#           here::here('data','census','true_vals','true_vals_nssec.csv'))
# write_csv(true_vals_urban, 
#           here::here('data','census','true_vals','true_vals_urban.csv'))


## read in

true_vals_engreg <- read_csv(here::here('data','census','true_vals','true_vals_engreg.csv'), show_col_types = F)
true_vals_age <- read_csv(here::here('data','census','true_vals','true_vals_age.csv'), show_col_types = F)
true_vals_hh_tenure <- read_csv(here::here('data','census','true_vals','true_vals_hh_tenure.csv'), show_col_types = F)
true_vals_hh_size <- read_csv(here::here('data','census','true_vals','true_vals_hh_size.csv'), show_col_types = F)
true_vals_ethnicity <- read_csv(here::here('data','census','true_vals','true_vals_ethnicity.csv'), show_col_types = F)
true_vals_hiqual <- read_csv(here::here('data','census','true_vals','true_vals_hiqual.csv'), show_col_types = F)
true_vals_nssec <- read_csv(here::here('data','census','true_vals','true_vals_nssec.csv'), show_col_types = F)
true_vals_urban <- read_csv(here::here('data','census','true_vals','true_vals_urban.csv'), show_col_types = F)













