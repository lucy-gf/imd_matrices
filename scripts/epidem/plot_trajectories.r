# epidemic output figures - trajectories

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
  file.path("output", "data", "epidem","base","epidemic_outputs.rds"),
  "base",
  file.path('output','figures','epidem','base','time_series.png')
) else commandArgs(trailingOnly = TRUE)

sens_analysis <- .args[2]

#### RUN ALL SETUP ####
{
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

nd <- pars$nd

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
    IMD = imd_quintile,
    population = pop,
    Age = age_grp) %>% 
    select(p_engreg, IMD, Age, population) %>% 
    group_by(p_engreg, IMD, Age) %>% 
    summarise(Population = sum(population)) %>% ungroup() %>% 
    group_by(p_engreg, IMD) %>% 
    mutate(tot_pop = sum(Population)) %>% ungroup() %>% 
    mutate(Proportion = Population/tot_pop)
  
  demog_allreg$Age <- factor(demog_allreg$Age,
                             levels = age_labels)
  demog_allreg <- demog_allreg %>% arrange(p_engreg, IMD, Age)
    
  
}else{
  
  age_structure_num <- ifelse(sens_analysis != 'nhs_ages', 1, 2)
  
  demog <- read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F) %>% 
    group_by(imd_quintile, age_grp) %>% summarise(population = sum(pop)) %>% 
    group_by(imd_quintile) %>% mutate(tot_pop = sum(population)) %>% 
    group_by(imd_quintile, age_grp, tot_pop) %>% summarise(Population = sum(population)) %>% 
    mutate(Proportion = Population/tot_pop) %>% rename(Age = age_grp, IMD = imd_quintile) 
  demog$Age <- factor(demog$Age,
                      levels = age_labels)
  demog <- demog %>% arrange(IMD, Age)
  
  n_pop <- sum(demog$Population)
  
}

#### PLOTTING FUNCTIONS ####

plot_trajectory <- function(
    data_in,
    variables = c(),
    cumulative = F,
    regional = F
    ){
  
  data_in <- data.table(data_in)
  
  ## column name checks
  if('sim' %notin% colnames(data_in) | 'time' %notin% colnames(data_in)){
    stop('Sim/time not in column names')
  }
  if(regional & 'p_engreg' %notin% colnames(data_in)){
    stop('Region not in column names')
  }
  for(var in variables){
    if(var %notin% colnames(data_in)){
      stop(paste0(var, ' not in column names'))
    }
  }
  
  ## grouping vars
  grouping_vars_in <- if(!regional){variables}else{c(variables, 'p_engreg')}
  grouping_vars <- c('sim','time', grouping_vars_in)
  time_var_vec <- c('time', grouping_vars_in)
  
  ## y axis label
  y_lab <- if(cumulative){'Cumulative infections per 1000 population'}else{
    'Infections per 1000 population'
  }
  
  ## total population data table
  pop_vec <- c(grouping_vars, 'pop')
  tot_pop <- data_in[, ..pop_vec][, lapply(.SD, sum), by = grouping_vars]
  
  ## total infections data table
  inf_vec <- c(grouping_vars, 'infections')
  dat_inf <- data_in[, ..inf_vec][, lapply(.SD, sum), by = grouping_vars]
  dat_inf <- dat_inf[tot_pop, on = grouping_vars]
  dat_inf[, attack_rate := infections/pop]
  
  ## median and CI data table
  attack_rate_vec <- c(time_var_vec, 'attack_rate')
  dat_med <- dat_inf[, ..attack_rate_vec][, lapply(.SD, median), by = time_var_vec]
  setnames(dat_med, 'attack_rate', 'median')
  dat_l <- dat_inf[, ..attack_rate_vec][, lapply(.SD, l95_func), by = time_var_vec]
  setnames(dat_l, 'attack_rate', 'l95')
  dat_u <- dat_inf[, ..attack_rate_vec][, lapply(.SD, u95_func), by = time_var_vec]
  setnames(dat_u, 'attack_rate', 'u95')
  
  dat_agg <- cbind(dat_med, l95 = dat_l$l95, u95 = dat_u$u95)
  
  ## basic plot
  p <- ggplot(dat_agg, aes(x=time)) + 
    theme_bw() +
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = y_lab, x = "Day")
  
  ## no stratifications
  if(length(variables) == 0){
    
    if(!regional){
      
      p <- p + 
        geom_ribbon(aes(ymin = 1000*l95, ymax = 1000*u95), alpha=0.25, fill = 'darkgreen')  +
        geom_line(aes(y = 1000*median), lwd=0.8, col = 'darkgreen') 
      
    }else{
      
      p <- p + 
        geom_ribbon(aes(ymin = 1000*l95, ymax = 1000*u95, fill = p_engreg), alpha=0.25)  +
        geom_line(aes(y = 1000*median, col = p_engreg), lwd=0.8) +
        scale_fill_manual(values = colors_p_engreg) + 
        scale_color_manual(values = colors_p_engreg) +
        labs(col = 'Region', fill = 'Region')
      
    }
    
  }else{
    
    ## age-stratified
    if(variables == c('age')){
      p <- p + 
        geom_ribbon(aes(ymin = 1000*l95, ymax = 1000*u95, 
                        fill = age, group = age), alpha=0.25)  +
        geom_line(aes(y = 1000*median, col = age, group = age), lwd=0.8) +
        scale_fill_manual(values = age_colors) + 
        scale_color_manual(values = age_colors) +
        labs(col = 'Age', fill = 'Age')
    }
    
    ## imd-stratified
    if(variables == c('imd')){
      p <- p + 
        geom_ribbon(aes(ymin = 1000*l95, ymax = 1000*u95, 
                        fill = imd, group = imd), alpha=0.25)  +
        geom_line(aes(y = 1000*median, col = imd, group = imd), lwd=0.8) +
        scale_fill_manual(values = imd_quintile_colors) + 
        scale_color_manual(values = imd_quintile_colors) +
        labs(col = 'IMD quintile', fill = 'IMD quintile')
    }
    
  }
  
  ## facet if regional (and some other stratification)
  if(regional & length(variables) > 0){p <- p + facet_wrap(. ~ p_engreg, scales = 'free')}
  
  p
  
}


#### NOT REGIONAL ####
## If NOT in regional sensitivity analysis

if(sens_analysis != 'regional'){
  
  ## read files
  daily_inf <- data.table(readRDS(gsub('.rds','_daily.rds',.args[1])))
  
  ## cumulative infections
  cum_inf <- daily_inf[, lapply(.SD, cumsum), by = c('sim','age','imd','imd_pop','pop')]
  cum_inf$time <- daily_inf$time
  
  ## make plots
  p1 <- plot_trajectory(daily_inf)
  p1b <- plot_trajectory(cum_inf, cumulative = T)
  p2 <- plot_trajectory(daily_inf, c('age'))
  p2b <- plot_trajectory(cum_inf, c('age'), cumulative = T)
  p3 <- plot_trajectory(daily_inf, c('age'))
  p3b <- plot_trajectory(cum_inf, c('age'), cumulative = T)
  
  ## patchwork
  p1 + p1b + p2 + p2b + p3 + p3b + plot_layout(nrow = 3, guides = 'collect')
  
  ## save
  ggsave(.args[3], dpi=600, 
         device = "png", width = 12, height = 9)
  
  
}else{
  
  #### REGIONAL ####
  
  for(reg_sens_analysis in 1){ # 1:4){ # only using main analysis (R0 = 1.5) for now
    
    ## set input/output folder
    reg_sens_analysis_name <- c('R0_1.5','R0_1.1','R0_3','R0_variable')[reg_sens_analysis]
    sens_analysis_sens_analysis <- paste0(sens_analysis, '/', reg_sens_analysis_name)
    
    ## read files
    daily_inf <- data.table(readRDS(gsub('.rds',paste0('_daily_',reg_sens_analysis,'.rds'),.args[1])))
    
    nsim <- n_distinct(daily_inf$sim)
    
    ## cumulative infections
    cum_inf <- daily_inf[, lapply(.SD, cumsum), by = c('sim','p_engreg','age','imd','imd_pop','pop')]
    cum_inf$time <- daily_inf$time
    
    ## regional stratification, incidence
    
    p1 <- plot_trajectory(daily_inf, regional = T)
    p1b <- plot_trajectory(cum_inf, cumulative = T, regional = T)
    p2 <- plot_trajectory(daily_inf, c('age'), regional = T)
    p2b <- plot_trajectory(cum_inf, c('age'), cumulative = T, regional = T)
    p3 <- plot_trajectory(daily_inf, c('imd'), regional = T)
    p3b <- plot_trajectory(cum_inf, c('imd'), cumulative = T, regional = T)
    
    ## overall attack rates
    
    regional_ars <- cum_inf %>% 
      filter(time == max(time)) %>% 
      group_by(sim, p_engreg) %>% 
      summarise(infections = sum(infections),
                pop = sum(pop),
                attack_rate = infections/pop) %>% 
      group_by(p_engreg) %>% 
      summarise(median = median(attack_rate),
                l95 = l95_func(attack_rate),
                u95 = u95_func(attack_rate)) %>% 
      ggplot(aes(x=p_engreg, col = p_engreg)) +
      geom_errorbar(aes(ymin = l95, ymax = u95), lwd = 1, width = 0.4) + 
      geom_point(aes(y = median), size = 3) + theme_bw() + 
      ylim(c(0,0.75)) + 
      scale_color_manual(values = colors_p_engreg) + 
      labs(x = '', y = 'Attack rate') +
      theme(legend.position = 'none'); regional_ars
      
    ## save
    regional_ars + (p1 + p1b + p2 + p2b + p3 + p3b + plot_layout(nrow = 3, guides = 'collect')) + 
      plot_layout(nrow = 2, heights = c(1,3))
    
    ggsave(here::here('output','figures','epidem',sens_analysis_sens_analysis,'time_series.png'), dpi=600, 
           device = "png", width = 16, height = 12)
    
    ggsave(.args[3], dpi=600, 
           device = "png", width = 16, height = 12)
    
  }
}
  









