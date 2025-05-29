
## RUN EXPLORATION ##

library(here)

here::here()

source(here::here('scripts','setup','install_packages.R'))
source(here::here('scripts','setup','colors.R'))

##---------------------------##
#### ASSIGNING CONNECT IMD #### 
##---------------------------##

## LOAD DATA ##

connect_part <- readRDS(here::here('data','connect','connect_part.rds'))

## LOAD RELEVANT DATA/FUNCTIONS ##

source(here::here('scripts','exploration','load_pcd_imd_census_data.R'))
source(here::here('scripts','exploration','pcd_imd_census_function.R'))

## ASSIGN IMD ##

source(here::here('scripts','assign_imd','assign_imd.R'))

## RUN ANALYSIS ##

source(here::here('scripts','exploration','pcd_imd_census.R'))











