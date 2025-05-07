
##-----------------------------------------##
#### INFER IMD FROM PCD1 AND CENSUS DATA ####
##-----------------------------------------##

set.seed(68)

method <- 'probabilistic'

##----------##
#### PCD1 ####
##----------##

read <- T

imd_samples <- if(!read){
  infer_imd(data_input = pcd_imd,
            census_data = pcd_imd,
            variables = c('pcd1'),
            testing = T,
            save_suffix = 'pcd1')
}else{
  readRDS(here::here('output','data','exploratory',method,'imd_samples_pcd1.rds'))
}

## model validation

imd_pcd1_out <- validate_imd(
  samples = imd_samples,
  variables = c('pcd1'), 
  lsoa_data = pcd_imd
  )

imd_pcd1_out %>% 
  ggplot() +
  geom_point(aes(x = category, y = med), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))

##------------------##
#### PCD1 AND AGE ####
##------------------##

read <- T

## running in 4 parallel sessions to reduce runtime
age_parallel <- function(i){
  
  ages <- unique(pcd_imd$age_grp)
  
  ages_i <- ages[((4*(i-1) + 1):(4*i))]
  
  infer_imd(data_input = pcd_imd %>% filter(age_grp %in% ages_i), 
            census_data = pcd_imd %>% filter(age_grp %in% ages_i), 
            variables = c('pcd1', 'age_grp'), 
            testing = T, 
            save_suffix = paste0('pcd1_age_grp_', i))
  
}

if(!read){
  imd_samples_age <- rbindlist(mclapply(1:4, age_parallel, mc.cores = 4))
}else{
  imd_samples_age <- data.table()
  # read in and merge saved outputs
  for(i in 1:4){
    imd_samples_age <- rbind(imd_samples_age, readRDS(here::here('output','data','exploratory',method,paste0('imd_samples_pcd1_age_grp_', i, '.rds'))))
  }
}

## model validation

imd_age_pcd1_out <- validate_imd(
  samples = imd_samples_age,
  variables = c('pcd1','age_grp'), 
  lsoa_data = pcd_imd
)

imd_age_pcd1_out %>% 
  ggplot() +
  geom_point(aes(x = category, y = med), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))


##-----------------------------##
#### PCD1 AND HOUSEHOLD SIZE ####
##-----------------------------##

## make input dataframe
hh_input <- hh_st %>% 
  left_join(unique(pcd_imd %>% select(pcd1, lsoa21cd, imd_quintile, urban_rural, eng_reg)),
            by = 'lsoa21cd', relationship = 'many-to-many') %>% 
  mutate(population = hh_size_cd*n_obs) %>% # scale up by household size for population sizes
  filter(hh_tenure_cd != -8, 
         population != 0)  

## running in 6 parallel sessions to reduce runtime
hh_size_parallel <- function(i){
  
  infer_imd(data_input = hh_input %>% filter(hh_size_cd == i), 
            census_data = hh_input %>% filter(hh_size_cd == i), 
            variables = c('pcd1','hh_size_cd'),
            testing = T, 
            save_suffix = paste0('pcd1_hh_size_', i))
  
}

read <- T

if(!read){
  imd_samples_hh_size <- rbindlist(mclapply(1:6, hh_parallel, mc.cores = 6))
}else{
  imd_samples_hh_size <- data.table()
  # read in and merge saved outputs
  for(i in 1:6){
    imd_samples_hh_size <- rbind(imd_samples_hh_size, readRDS(here::here('output','data','exploratory',method,paste0('imd_samples_pcd1_hh_size_', i, '.rds'))))
  }
}

## model validation

imd_hh_size_out <- validate_imd(
  samples = imd_samples_hh_size,
  variables = c('pcd1','hh_size_cd'),
  lsoa_data = pcd_imd
)

imd_hh_size_out %>% 
  ggplot() +
  geom_point(aes(x = category, y = med), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))

##-------------------------------##
#### PCD1 AND HOUSEHOLD TENURE ####
##-------------------------------##

## running in 6 parallel sessions to reduce runtime
hh_ten_parallel <- function(i){
  
  infer_imd(data_input = hh_input %>% filter(hh_tenure_cd == i - 1), 
            census_data = hh_input %>% filter(hh_tenure_cd == i - 1), 
            variables = c('pcd1','hh_tenure_cd'),
            testing = T, 
            save_suffix = paste0('pcd1_hh_ten_', i))
  
}

read <- T

if(!read){
  imd_samples_hh_ten <- rbindlist(mclapply(1:6, hh_ten_parallel, mc.cores = 6))
}else{
  imd_samples_hh_ten <- data.table()
  # read in and merge saved outputs
  for(i in 1:6){
    imd_samples_hh_ten <- rbind(imd_samples_hh_ten, readRDS(here::here('output','data','exploratory',method,paste0('imd_samples_pcd1_hh_ten_', i, '.rds'))))
  }
}

## model validation

imd_hh_ten_out <- validate_imd(
  samples = imd_samples_hh_ten,
  variables = c('pcd1','hh_tenure_cd'),
  lsoa_data = pcd_imd
)

imd_hh_ten_out %>% 
  ggplot() +
  geom_point(aes(x = category, y = med), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))


##-------------------------------##
#### PCD1 AND HH SIZE & TENURE ####
##-------------------------------##

## running in 6 parallel sessions to reduce runtime
hh_parallel <- function(i){
  
  infer_imd(data_input = hh_input %>% filter(hh_size_cd == i), 
            census_data = hh_input %>% filter(hh_size_cd == i), 
            variables = c('pcd1','hh_size_cd','hh_tenure_cd'),
            testing = T, 
            save_suffix = paste0('pcd1_hh_', i))
  
}

read <- T

if(!read){
  imd_samples_hh <- rbindlist(mclapply(1:6, hh_parallel, mc.cores = 6))
}else{
  imd_samples_hh <- data.table()
  # read in and merge saved outputs
  for(i in 1:6){
    imd_samples_hh <- rbind(imd_samples_hh, readRDS(here::here('output','data','exploratory',method,paste0('imd_samples_pcd1_hh_', i, '.rds'))))
  }
}

## model validation

imd_hh_out <- validate_imd(
  samples = imd_samples_hh,
  variables = c('pcd1','hh_size_cd','hh_tenure_cd'),
  lsoa_data = pcd_imd
)

imd_hh_out %>% 
  ggplot() +
  geom_point(aes(x = category, y = med), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))


##-------------------##
#### DETERMINISTIC ####
##-------------------##

## pcd1

imd_samples_det <- infer_imd(data_input = pcd_imd,
                             census_data = pcd_imd,
                             variables = c('pcd1'),
                             testing = T,
                             save_suffix = 'pcd1',
                             modal = T)

imd_pcd1_out_det <- validate_imd(
  samples = imd_samples_det,
  variables = c('pcd1'), 
  lsoa_data = pcd_imd,
  modal = T,
)
imd_pcd1_out_det %>% 
  ggplot() +
  geom_point(aes(x = category, y = mean), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))

## age

imd_samples_age_det <- infer_imd(data_input = pcd_imd,
                                 census_data = pcd_imd,
                                 variables = c('pcd1','age_grp'),
                                 testing = T,
                                 save_suffix = 'pcd1_age_grp',
                                 modal = T)

imd_pcd1_out_age_det <- validate_imd(
  samples = imd_samples_age_det,
  variables = c('pcd1','age_grp'), 
  lsoa_data = pcd_imd,
  modal = T,
)
imd_pcd1_out_age_det %>% 
  ggplot() +
  geom_point(aes(x = category, y = mean), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))

## household data
imd_samples_hh_det <- infer_imd(data_input = hh_input,
                                census_data = hh_input,
                                variables = c('pcd1','hh_size_cd','hh_tenure_cd'),
                                testing = T,
                                save_suffix = 'pcd1_hh_',
                                modal = T)

imd_pcd1_out_det <- validate_imd(
  samples = imd_samples_hh_det,
  variables = c('pcd1','hh_size_cd','hh_tenure_cd'),
  lsoa_data = pcd_imd,
  modal = T,
)
imd_pcd1_out_det %>% 
  ggplot() +
  geom_point(aes(x = category, y = mean), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))




##------------------------##
#### PCD1 AND ETHNICITY ####
##------------------------##

read <- T

## running in 4 parallel sessions to reduce runtime
age_ethn_parallel <- function(i){
  
  ages <- unique(age_ethn$age_nm)
  
  ages_i <- ages[((3*(i-1) + 1):(3*i))]
  
  infer_imd(data_input = age_ethn %>% filter(age_nm %in% ages_i), 
            census_data = age_ethn %>% filter(age_nm %in% ages_i), 
            variables = c('age_nm', 'ethn_nm'), 
            testing = T, 
            save_suffix = paste0('pcd1_age_ethn_', i))
  
}

if(!read){
  imd_samples_age_ethn <- rbindlist(mclapply(1:6, age_ethn_parallel, mc.cores = 6))
}else{
  imd_samples_age_ethn <- data.table()
  # read in and merge saved outputs
  for(i in 1:4){
    imd_samples_age_ethn <- rbind(imd_samples_age_ethn, readRDS(here::here('output','data','exploratory',method,paste0('imd_samples_pcd1_age_ethn_', i, '.rds'))))
  }
}

## model validation

imd_age_pcd1_out <- validate_imd(
  samples = imd_samples_age_ethn,
  variables = c('pcd1', 'age_nm', 'ethn_nm'), 
  lsoa_data = age_ethn
)

imd_age_pcd1_out %>% 
  ggplot() +
  geom_point(aes(x = category, y = med), size = 2) + 
  geom_errorbar(aes(x = category, ymin = lower, ymax = upper), width = 0.2) + 
  ylim(c(0,NA)) + labs(y = 'Proportion correct', x = '') + theme_bw() + 
  facet_wrap(variable ~ ., scales = 'free_x', nrow = n_distinct(imd_pcd1_out$variable))

























