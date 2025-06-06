
## RUN ASSIGNING IMD ##

library(here)

here::here()

source(here::here('scripts','setup','install_packages.R'))
source(here::here('scripts','setup','colors.R'))

##---------------------------##
#### ASSIGNING CONNECT IMD #### 
##---------------------------##

## LOAD DATA ##

connect_part <- readRDS(here::here('data','connect','connect_part.rds')) %>% 
  filter(p_country == 'England')

## LOAD RELEVANT DATA ##

source(here::here('scripts','exploration','load_pcd_imd_census_data.R'))
source(here::here('scripts','assign_imd','make_imd_assignment_data_inputs.R'))

## LOAD RELEVANT FUNCTIONS ##

source(here::here('scripts','exploration','pcd_imd_census_function.R'))
source(here::here('scripts','assign_imd','assign_imd_fcns.R'))

## ASSIGN IMD, EVALUATE ASSIGNMENT ##

read <- T
n_bootstraps <- 100

variables_input <- c('pcd1','hh_size_nm','hh_tenure_nm')
modal_in <- F # F = probabilistic, T = deterministic
source(here::here('scripts','assign_imd','assign_imd.R'))

variables_input <- c('pcd1','hh_size_nm','hh_tenure_nm')
modal_in <- T # F = probabilistic, T = deterministic
source(here::here('scripts','assign_imd','assign_imd.R'))

variables_input <- c('pcd1','age_grp')
modal_in <- F # F = probabilistic, T = deterministic
source(here::here('scripts','assign_imd','assign_imd.R'))

variables_input <- c('pcd1')
modal_in <- F # F = probabilistic, T = deterministic
source(here::here('scripts','assign_imd','assign_imd.R'))

variables_input <- c('eng_reg')
modal_in <- F # F = probabilistic, T = deterministic
source(here::here('scripts','assign_imd','assign_imd.R'))

variables_input <- c('eng_reg')
modal_in <- T # F = probabilistic, T = deterministic
source(here::here('scripts','assign_imd','assign_imd.R'))

variables_input <- c('pcd1','age_grp_8','p_sec_input','p_hiqual')
modal_in <- F # F = probabilistic, T = deterministic
source(here::here('scripts','assign_imd','assign_imd.R'))

variables_input <- c('pcd1','age_grp_6','p_ethnicity')
modal_in <- F # F = probabilistic, T = deterministic
source(here::here('scripts','assign_imd','assign_imd.R'))


## plot error scores

source(here::here('scripts','assign_imd','plot_error_scores.R'))






