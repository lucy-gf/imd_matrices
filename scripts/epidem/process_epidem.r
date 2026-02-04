# produce daily and overall attack rates as .rds

suppressPackageStartupMessages(require(bench))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(ggtext))
suppressPackageStartupMessages(require(gridExtra))
suppressPackageStartupMessages(require(Rcpp))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(patchwork))
suppressPackageStartupMessages(require(viridis))
options(dplyr.summarise.inform = FALSE)

.args <- if (interactive()) c(
  file.path("output", "data", "epidem","regional","byall.rds"),
  "regional",
  file.path('output','data','epidem','regional','epidemic_outputs.rds')
) else commandArgs(trailingOnly = TRUE)

sens_analysis <- .args[2]

#### RUN ALL SETUP ####
if(T){
# source colors etc.
source(here::here('scripts','assign_imd','assign_imd_fcns.R'))
source(here::here('scripts','setup','colors.R'))

### Basic setting
source_dir <- "scripts/epidem"
source(here::here(source_dir,'setup.r')) #repo

### Diseases cycle
pset$Disease <- "Influenza"

# set ages
if(sens_analysis == 'nhs_ages'){
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
}
age_structure_num <- ifelse(sens_analysis != 'nhs_ages', 1, 2)
demog <- read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F) %>% 
  group_by(age_grp) %>% summarise(population = sum(pop)) 
demog$age_grp <- factor(demog$age_grp, levels = age_labels)
demog <- demog %>% arrange(age_grp)
demog_population <- demog$population

## Parameters
source(paste0(source_dir,"/parsF_.r"))
## If R0 low, make runtime longer
if(pset$R0fixed & (pars$R0 <= 1.1)){ 
  
  pars$times  <- 0:1000     #days sequence
  pars$nt     <- (max(pars$times)-min(pars$times))/pars$dt + 1       #no. time points, iterations
  pars$nw     <- ceiling((max(pars$times)-min(pars$times))/7)   #weeks length of model run
  pars$nd     <- ceiling((max(pars$times)-min(pars$times)))+1   #days length of model run
  
}else{
  if(pset$R0fixed & (pars$R0 < 1.65)){ 
    
    pars$times  <- 0:130     #days sequence
    pars$nt     <- (max(pars$times)-min(pars$times))/pars$dt + 1       #no. time points, iterations
    pars$nw     <- ceiling((max(pars$times)-min(pars$times))/7)   #weeks length of model run
    pars$nd     <- ceiling((max(pars$times)-min(pars$times)))+1   #days length of model run
    
  }
  
}

## Set base levels for IMD and age
base_imd_arr <- 5
base_age_arr <- ifelse(sens_analysis != 'nhs_ages', '35-39', '35-49')

# set seed
set.seed(120)

age_colors <- if(sens_analysis != 'nhs_ages'){
  colors_p_age_group
}else{
  colors_p_age_group_nhs
}

## Population by age group (over SES), by SES (over age), overall
# number of age groups
na <- pars$na
# number of SES
nimd <- pars$nimd
# number of groups
ng <- na*nimd
# number of days
nd <- pars$nd 

variable_labels <- CJ(
  imd = as.character(1:nimd),
  age = age_labels
)
variable_labels$age <- factor(variable_labels$age,
                              levels = age_labels)
variable_labels <- variable_labels %>% 
  arrange(imd, age)
variable_labels$variable <- paste0('Iw_g', 1:(na*nimd))

## Figures

ar=1 #aspect ratio

l95_func <- function(x){quantile(x, probs=0.025)}; u95_func <- function(x){quantile(x, probs=0.975)}

}

#### DEMOGRAPHY ####

if(sens_analysis == 'regional'){
  
  imd_age_raw <- data.table(read_csv(file.path("data","imd_25","imd_ages_1.csv"), show_col_types = F))
  
  demog_allreg <- imd_age_raw %>% 
    mutate(p_engreg = case_when(
      grepl('London',p_engreg) ~ 'Greater London',
      grepl('Yorkshire',p_engreg) ~ 'Yorkshire and the Humber',
      T ~ p_engreg
    ),
    imd = as.character(imd_quintile),
    population = pop,
    age = age_grp) %>% 
    select(p_engreg, imd, age, population) %>% 
    group_by(p_engreg, imd, age) %>% 
    summarise(Population = sum(population)) %>% ungroup() %>% 
    group_by(p_engreg, imd) %>% 
    mutate(tot_pop = sum(Population)) %>% ungroup() %>% 
    mutate(Proportion = Population/tot_pop)
  
  demog_allreg$age <- factor(demog_allreg$age,
                             levels = age_labels)
  demog_allreg <- demog_allreg %>% arrange(p_engreg, imd, age)
  
  demog_allreg <- data.table(demog_allreg)
    
  
}else{
  
  age_structure_num <- ifelse(sens_analysis != 'nhs_ages', 1, 2)
  
  demog <- read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F) %>% 
    group_by(imd_quintile, age_grp) %>% summarise(population = sum(pop)) %>% 
    group_by(imd_quintile) %>% mutate(tot_pop = sum(population)) %>% 
    group_by(imd_quintile, age_grp, tot_pop) %>% summarise(Population = sum(population)) %>% 
    mutate(Proportion = Population/tot_pop) %>% rename(Age = age_grp, IMD = imd_quintile) 
  demog$Age <- factor(demog$Age,
                      levels = age_labels)
  demog <- demog %>% arrange(IMD, Age) %>% mutate(IMD = as.character(IMD))
  
  n_pop <- sum(demog$Population)
  
}

#### NOT REGIONAL ####
## If NOT in regional sensitivity analysis

if(sens_analysis != 'regional'){
  
  byall <- readRDS(.args[1])
  
  data1000 <- copy(byall)
  if('beta' %in% colnames(data1000)){data1000[, beta := NULL]}
  data1000[, iW := NULL]
  data <- melt.data.table(data1000, id.vars = c('sim','time'))
  setnames(data,'value','infections')
  data <- data[variable_labels, on = c('variable')]
  data[, variable := NULL]
  data <- data[demog %>% rename(age=Age,imd=IMD,imd_pop=tot_pop,pop=Population) %>% select(!Proportion), on = c('imd','age')]
  data[, attack_rate := infections/pop]
  
  ## save daily data
  write_rds(data, gsub('.rds','_daily.rds',.args[3]))
  
  ## total infections
  data <- data[, lapply(.SD, sum), by = c('sim','age','imd','imd_pop','pop')]
  data[, time := NULL]
  
  ## save total infections
  write_rds(data, .args[3])
  
}else{
  
  #### REGIONAL ####
  
  for(reg_sens_analysis in 1){# 1:4){ # only using main analysis (R0 = 1.5) for now
    
    if(reg_sens_analysis == 1){reg_sens_analysis_name <- 'R0_1.5'}
    if(reg_sens_analysis == 2){reg_sens_analysis_name <- 'R0_1.1'}
    if(reg_sens_analysis == 3){reg_sens_analysis_name <- 'R0_3'}
    if(reg_sens_analysis == 4){reg_sens_analysis_name <- 'R0_variable'}
    
    # change input name
    input <- gsub('all', paste0('all_', reg_sens_analysis), .args[1])
    
    ## read files
    byall <- data.table()
    
    for(region in unique(demog_allreg$p_engreg)){
      
      region_collapsed <- gsub(' ', '_', region)
      
      byall <- rbind(byall,
                     readRDS(gsub('.rds',paste0('_', region_collapsed, '.rds'),input)))
      
    }
    
    nreg <- n_distinct(demog_allreg$p_engreg)
    
    data1000 <- copy(byall)
    if('beta' %in% colnames(data1000)){data1000[, beta := NULL]}
    data1000[, iW := NULL]
    data <- melt.data.table(data1000, id.vars = c('sim','time','p_engreg'))
    setnames(data,'value','infections')
    data <- data[variable_labels, on = c('variable')]
    data[, variable := NULL]
    data <- data[demog_allreg %>% rename(imd_pop=tot_pop,pop=Population) %>% select(!Proportion), on = c('imd','age','p_engreg')]
    data[, attack_rate := infections/pop]
    
    ## save daily data
    write_rds(data, gsub('.rds',paste0('_daily_',reg_sens_analysis,'.rds'),.args[3]))
    
    ## total infections
    data <- data[, lapply(.SD, sum), by = c('sim','p_engreg','age','imd','imd_pop','pop')]
    data[, time := NULL]
    
    ## save total infections
    write_rds(data, gsub('.rds',paste0('_',reg_sens_analysis,'.rds'),.args[3]))
    
    write_rds(data.table(x=0), .args[3])
  
  }
  
}




