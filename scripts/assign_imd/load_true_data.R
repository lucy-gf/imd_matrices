
# ## english region distribution
# 
# pcd_imd <- read_csv(here::here('data','census','pcd1.csv'), show_col_types = F)
# 
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
#          prop = n/n_tot)
# 
# ## household tenure distribution
# 
# hh_input <- read_csv(here::here('data','census','pcd1household.csv'), show_col_types = F)
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
# age_ethn_pcd1 <- read_csv(here::here('data','census','pcd1ageethn.csv'), show_col_types = F)
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
# ns_hq_pcd1 <- read_csv(here::here('data','census','pcd1agehiqualnssec.csv'), show_col_types = F)
# 
# true_vals_hiqual <- ns_hq_pcd1 %>%
#   group_by(p_hiqual, imd_quintile) %>%
#   summarise(n = sum(population)) %>%
#   group_by(p_hiqual) %>%
#   mutate(n_tot = sum(n),
#          prop = n/n_tot)
# 
# ## nssec distribution
# 
# true_vals_nssec <- ns_hq_pcd1 %>%
#   group_by(p_sec_input, imd_quintile) %>%
#   summarise(n = sum(population)) %>%
#   group_by(p_sec_input) %>%
#   mutate(n_tot = sum(n),
#          prop = n/n_tot)
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













