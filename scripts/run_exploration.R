
## RUN EXPLORATION ##

library(here)

here::here()

source(here::here('scripts','setup','install_packages.R'))
source(here::here('scripts','setup','colors.R'))

##--------------------------##
#### IMD CENSUS INFERENCE #### 
##--------------------------##

## LOAD DATA ##

source(here::here('scripts','exploration','load_pcd_imd_census_data.R'))

## LOAD FUNCTIONS ##

source(here::here('scripts','exploration','pcd_imd_census_function.R'))

## RUN ANALYSIS ##

source(here::here('scripts','exploration','pcd_imd_census.R'))















