
#### RUN IMD ASSIGNMENT ####

connect_output <- if(read){
  read_rds(here::here('output','data','exploratory','assignment',
                            paste0('connect_output_', 
                                   paste(unname(unlist(lapply(variables_input, FUN = simp_labels))), 
                                         collapse = '_'), ifelse(modal_in,'_det','_probab'),
                                   ifelse(modal_in, '', paste0('_', n_bootstraps)), '.rds')))
}else{
  fcn_assign_imd(
    data_input = connect_input,
    census_data = if('age_grp' %in% variables_input){
      age_input
      }else{
        if('age_grp_8' %in% variables_input){
          ns_hq_input
          }else{
            if('age_grp_6' %in% variables_input){
              ethn_input
            }else{
            hh_input
            }
          }
        },
    variables = variables_input,
    n_bootstraps = n_bootstraps,
    modal = modal_in
  )
}

#### DISTRIBUTION PLOTS ####

# make directory for plots
directory_plots <- here::here(here::here('output','figures','exploratory','assignment',
                                         paste0(paste(unname(unlist(lapply(variables_input, FUN = simp_labels))), 
                                               collapse = '_'), 
                                               ifelse(modal_in,'_det','_probab'), 
                                               ifelse(modal_in, '', paste0('_', n_bootstraps)))))
if(!dir.exists(directory_plots)){
  dir.create(directory_plots)
}

# plots

fcn_barplot_imd(connect_output, 
             c('p_income','p_hiqual','p_age_group','p_ethnicity',
               'p_emp_1','hh_size_nm','hh_tenure_nm','p_engreg',
               'p_sec_input')
             )

fcn_lineplot_imd(connect_output, 
                c('p_income','p_hiqual','p_age_group','p_ethnicity',
                  'p_emp_1','hh_size_nm','hh_tenure_nm','p_engreg',
                  'p_sec_input')
)

fcn_errorbarplot_imd(connect_output, 
                     c('p_income','p_hiqual','p_age_group','p_ethnicity',
                       'p_emp_1','hh_size_nm','hh_tenure_nm','p_engreg',
                       'p_sec_input')
)


## age plots ##

ci_age <- 0.95

connect_output %>% 
  group_by(p_age_group, imd_quintile, bootstrap) %>% 
  summarise(mean = mean(n_contacts + large_n)) %>% 
  group_by(p_age_group, imd_quintile) %>% 
  summarise(med = median(mean),
            lower = quantile(mean, (1 - ci_age)/2),
            upper = quantile(mean, (1 - (1 - ci_age)/2))) %>% 
  ggplot() +
  geom_errorbar(aes(p_age_group, ymin = lower, ymax = upper, 
                    col = as.factor(imd_quintile), group = as.factor(imd_quintile)),
                width = 0.2, lwd = 0.8, position = position_dodge(width = 0.9)) +
  geom_point(aes(p_age_group, med, col = as.factor(imd_quintile)), 
             size = 3, position = position_dodge(width = 0.9)) +
  theme_bw() + ylim(c(0,NA)) + 
  labs(color = 'IMD quintile', fill = 'IMD quintile', 
       x = 'Age group', y = 'Mean contacts') + 
  ggtitle(paste0('Age-specific mean contacts, ', 100*ci_age, '% CI, Predictors = ', 
                 paste(unname(unlist(lapply(variables_input, FUN = simp_labels))),
                       collapse = ', '), ',\nMethod = ', 
                 ifelse(modal_in, 'deterministic', 
                        paste0('probabilistic, n_bootstraps = ', n_bootstraps)))) + 
  scale_color_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

ggsave(here::here(directory_plots, 'mean_contacts_imd_age.png'),
       width = 10, height = 7)

connect_output %>% 
  group_by(imd_quintile, p_age_group, bootstrap) %>% 
  summarise(mean = mean(n_contacts + large_n),
            n = n()) %>% 
  group_by(imd_quintile, p_age_group) %>% 
  summarise(med = median(mean),
            lower = quantile(mean, (1 - ci_age)/2),
            upper = quantile(mean, (1 - (1 - ci_age)/2)),
            mean_n = round(mean(n),0)) %>% 
  ggplot() +
  geom_errorbar(aes(imd_quintile, ymin = lower, ymax = upper, col = as.factor(imd_quintile)),
                width = 0.2, lwd = 0.8) +
  geom_line(aes(x = imd_quintile, y = med), lwd = 0.5, alpha = 0.5) +
  geom_point(aes(imd_quintile, med, col = as.factor(imd_quintile)), size = 3) +
  theme_bw() + ylim(c(0,NA)) + facet_wrap(p_age_group~., scales = 'free') + 
  labs(color = 'IMD quintile', fill = 'IMD quintile', y = 'Mean contacts', x = 'IMD quintile') + 
  scale_color_manual(values = imd_quintile_colors) +
  ggtitle(paste0('Age-specific mean contacts, ', 100*ci_age, '% CI, Predictors = ', 
                 paste(unname(unlist(lapply(variables_input, FUN = simp_labels))),
                       collapse = ', '), ',\nMethod = ', 
                 ifelse(modal_in, 'deterministic', 
                        paste0('probabilistic, n_bootstraps = ', n_bootstraps)))) + 
  geom_text(aes(x = imd_quintile, y = 1.1*upper, label = mean_n, color = as.factor(imd_quintile)))

ggsave(here::here(directory_plots, 'mean_contacts_imd_age_faceted.png'),
       width = 12, height = 14)


#### PLOT IMD RATIOS ####
## PLOT THESE DISTRIBUTIONS COMPARED TO TRUE CENSUS DISTRIBUTIONS ##

## english region distribution
true_vals_engreg <- pcd_imd %>% 
  group_by(eng_reg, imd_quintile) %>% 
  summarise(n = sum(population)) %>% 
  group_by(eng_reg) %>% 
  mutate(n_tot = sum(n),
         prop = n/n_tot) %>% 
  rename(p_engreg = eng_reg) %>% 
  mutate(p_engreg = case_when(
    p_engreg %like% 'London' ~ 'Greater London',
    p_engreg %like% 'Yorkshire' ~ 'Yorkshire and the Humber',
    T ~ p_engreg
  ))

fcn_rev_errorbarplot_imd(connect_output, 
                     'p_engreg',
                     true_vals = true_vals_engreg,
                     ci_width = 0.95,
                     true_distr = T
)

## age distribution

true_vals_age <- pcd_imd %>% 
  group_by(age_grp, imd_quintile) %>% 
  summarise(n = sum(population)) %>% 
  group_by(age_grp) %>% 
  mutate(n_tot = sum(n),
         prop = n/n_tot) %>% 
  rename(p_age_group = age_grp) 

fcn_rev_errorbarplot_imd(connect_output, 
                     'p_age_group',
                     true_vals = true_vals_age,
                     ci_width = 0.95,
                     true_distr = T
)

## household tenure distribution

true_vals_hh_tenure <- hh_input %>% 
  group_by(hh_tenure_nm, imd_quintile) %>% 
  summarise(n = sum(population)) %>% 
  group_by(hh_tenure_nm) %>% 
  mutate(n_tot = sum(n),
         prop = n/n_tot)

fcn_rev_errorbarplot_imd(connect_output, 
                     'hh_tenure_nm',
                     true_vals = true_vals_hh_tenure,
                     ci_width = 0.95,
                     true_distr = T
)


## household size distribution

true_vals_hh_size <- hh_input %>% 
  group_by(hh_size_nm, imd_quintile) %>% 
  summarise(n = sum(population)) %>% 
  group_by(hh_size_nm) %>% 
  mutate(n_tot = sum(n),
         prop = n/n_tot)

fcn_rev_errorbarplot_imd(connect_output, 
                     'hh_size_nm',
                     true_vals = true_vals_hh_size,
                     ci_width = 0.95,
                     true_distr = T
)

## ethnicity distribution

true_vals_ethnicity <- age_ethn_pcd1 %>% 
  group_by(ethn_nm, imd_quintile) %>% 
  summarise(n = sum(population)) %>% 
  group_by(ethn_nm) %>% 
  mutate(n_tot = sum(n),
         prop = n/n_tot) %>% 
  rename(p_ethnicity = ethn_nm) %>% 
  mutate(p_ethnicity = case_when(
    p_ethnicity %like% 'Asian' ~ 'Asian',
    p_ethnicity %like% 'Black' ~ 'Black',
    p_ethnicity %like% 'Mixed' ~ 'Mixed',
    p_ethnicity %like% 'White' ~ 'White',
    T ~ 'Other'
  ))

fcn_rev_errorbarplot_imd(connect_output, 
                     'p_ethnicity',
                     true_vals = true_vals_ethnicity,
                     ci_width = 0.95,
                     true_distr = T
)


## highest qualification distribution

true_vals_hiqual <- hiqual_pcd1 %>% 
  group_by(hiqual_nm_short, imd_quintile) %>% 
  summarise(n = sum(population)) %>% 
  group_by(hiqual_nm_short) %>% 
  mutate(n_tot = sum(n),
         prop = n/n_tot) %>% 
  rename(p_hiqual = hiqual_nm_short) 

connect_output <- connect_output %>% 
  mutate(p_hiqual = case_when(
    p_hiqual %like% 'Level 1' ~ '1-4 GCSEs',
    p_hiqual %like% 'Level 2' ~ '5+ GCSEs',
    p_hiqual %like% 'Level 3' ~ '2+ A levels',
    p_hiqual %like% 'Level 4' ~ 'Degree',
    p_hiqual %like% 'Apprent' ~ 'Apprentice/vocational',
    T ~ p_hiqual
  ))

fcn_rev_errorbarplot_imd(connect_output, 
                     'p_hiqual',
                     true_vals = true_vals_hiqual,
                     ci_width = 0.95,
                     true_distr = T
)


## nssec distribution

true_vals_nssec <- nssec_pcd1 %>% 
  group_by(nssec_nm, imd_quintile) %>% 
  summarise(n = sum(population)) %>% 
  group_by(nssec_nm) %>% 
  mutate(n_tot = sum(n),
         prop = n/n_tot) %>% 
  rename(p_sec_input = nssec_nm) %>% 
  mutate(p_sec_input = case_when(
    p_sec_input %like% 'L1, L2' ~ '1',
    p_sec_input %like% 'L4' ~ '2',
    p_sec_input %like% 'L7' ~ '3',
    p_sec_input %like% 'L8' ~ '4',
    p_sec_input %like% 'L11' ~ '5',
    p_sec_input %like% 'L12' ~ '6',
    p_sec_input %like% 'L13' ~ '7',
    T ~ NA
  ))

fcn_rev_errorbarplot_imd(connect_output, 
                     'p_sec_input',
                     true_vals = true_vals_nssec,
                     ci_width = 0.95,
                     true_distr = T
)


## urban/rural

true_vals_urban <- pcd_imd %>% 
  group_by(urban_rural, imd_quintile) %>% 
  summarise(n = sum(population)) %>% 
  group_by(urban_rural) %>% 
  mutate(n_tot = sum(n),
         prop = n/n_tot) %>% 
  rename(p_urban_rural = urban_rural)

fcn_rev_errorbarplot_imd(connect_output, 
                     'p_urban_rural',
                     true_vals = true_vals_urban,
                     ci_width = 0.95,
                     true_distr = T
)


#### EVALUATE ####

## plot patchwork of goodness-of-fit indicators
fcn_evaluate_imd(
    data_input = connect_output,
    census_data_list = list(true_vals_engreg,
                            true_vals_age,
                            true_vals_hh_tenure,
                            true_vals_hh_size,
                            true_vals_ethnicity,
                            true_vals_hiqual,
                            true_vals_nssec,
                            true_vals_urban),
    predictors = variables_input,
    modal = modal_in
)





#---------------------------------------------------

# library(eq5d)
# 
# phrase_to_num <- function(string){
#   z <- case_when(
#     grepl(' no', string) ~ 1,
#     grepl(' slight', string) ~ 2,
#     grepl(' moderate', string) ~ 3,
#     grepl(' severe', string) ~ 4,
#     grepl(' extreme|unable', string) ~ 5)
#   z
# }
# 
# connect_output <- connect_output %>% mutate(score_5 = '',
#                                             eq5d = 0)
# for(id in unique(connect_output$p_id)){
#   if(connect_output[p_id == id,]$p_adult_child[1] == 'Adult'){
#     
#     score_5 <- paste0(
#       phrase_to_num(connect_output[p_id == id,]$p_dis_1[1]),
#       phrase_to_num(connect_output[p_id == id,]$p_dis_2[1]),
#       phrase_to_num(connect_output[p_id == id,]$p_dis_3[1]),
#       phrase_to_num(connect_output[p_id == id,]$p_dis_4[1]),
#       phrase_to_num(connect_output[p_id == id,]$p_dis_5[1])
#     )
#     
#     score <- eq5d(scores=score_5, country="England", version="5L", type="VT")
#     
#     connect_output[p_id == id, score_5 := score_5]
#     connect_output[p_id == id, eq5d := score]
#   }
#   if(id %% 100 == 0){cat(id, ' ', sep = '')}
# }
# 
# connect_output <- connect_output %>% 
#   mutate(eq5d_rounded = floor(eq5d*10)/10) %>% 
#   mutate(eq5d_rounded = case_when(eq5d_rounded < 0 ~ -0.1,
#                                   T ~ eq5d_rounded))
