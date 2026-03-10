
## PLOT CONTACT MATRIX SUMMARY STATISTICS ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(ggplot2)
suppressPackageStartupMessages(library(viridis, warn.conflicts = FALSE))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","regional","fitted_matrs_balanced.csv"),
  "regional",
  file.path("output", "data", "cont_matrs","regional","imd_assortativity.csv")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))
source(here::here('scripts','setup','colors.R'))

#### READ IN DATA ####

balanced_matr <- data.table(read_csv(.args[1], show_col_types = F))

sens_analysis <- .args[2]

## age distribution 

age_structure_num <- ifelse(!grepl('nhs_ages',sens_analysis), 1, 2)

if(grepl('nhs_ages',sens_analysis)){
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
}

imd_age_raw <- data.table(read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F))

if(grepl('regional',sens_analysis)){
  
  imd_age <- imd_age_raw %>% 
    mutate(p_engreg = case_when(
      grepl('London',p_engreg) ~ 'Greater London',
      grepl('Yorkshire',p_engreg) ~ 'Yorkshire and the Humber',
      T ~ p_engreg
    ),
    imd_q = imd_quintile,
    population = pop,
    age = age_grp) %>% 
    select(p_engreg, imd_q, age, population) %>% 
    group_by(p_engreg, imd_q, age) %>% 
    summarise(population = sum(population)) %>% ungroup() %>% 
    group_by(p_engreg, imd_q) %>% 
    mutate(tot_pop_imd = sum(population)) %>% 
    ungroup() %>% 
    group_by(p_engreg) %>% mutate(tot_pop = sum(population)) %>% ungroup() %>% 
    mutate(prop_imd = population/tot_pop_imd,
           prop = population/tot_pop)
  
  imd_age$age <- factor(imd_age$age, levels = age_labels)
  
}else{
  
  imd_age <- imd_age_raw %>% 
    mutate(
      imd_q = imd_quintile,
      population = pop,
      age = age_grp) %>% 
    select(imd_q, age, population) %>% 
    group_by(imd_q, age) %>% 
    summarise(population = sum(population)) %>% ungroup() %>% 
    group_by(imd_q) %>% 
    mutate(tot_pop_imd = sum(population)) %>% 
    ungroup() %>% 
    mutate(tot_pop = sum(population)) %>% ungroup() %>% 
    mutate(prop_imd = population/tot_pop_imd,
           prop = population/tot_pop)
  
  imd_age$age <- factor(imd_age$age, levels = age_labels)
  imd_age <- imd_age %>% arrange(imd_q, age)
  
}

#### FUNCTIONS ####
mean_age_diff <- function(agg_dat){
  
  data <- if(!grepl('regional',sens_analysis)){
    CJ(p_i = 1:5, c_i = 1:5, m = 0)}else{
      CJ(p_engreg=unique(agg_dat$p_engreg),
         p_i = 1:5, c_i = 1:5, m = 0)
    }
  
  if(!grepl('regional',sens_analysis)){
    for(i in 1:5){for(j in 1:5){
      filt <- agg_dat %>% filter(p_imd_q==i, c_imd_q==j) %>% 
        ungroup() %>% 
        mutate(p_age_group = case_when(p_age_group=='75+' ~ '77.5-', 
                                       p_age_group=='80+' ~ '82.5-', 
                                       T ~ p_age_group),
               c_age_group = case_when(c_age_group=='75+' ~ '77.5-',
                                       c_age_group=='80+' ~ '82.5-', 
                                       T ~ c_age_group),
               p_mid_age = as.numeric(gsub("-.*$", "", p_age_group)) + 2.5,
               c_mid_age = as.numeric(gsub("-.*$", "", c_age_group)) + 2.5) %>% 
        group_by(p_age_group, p_mid_age) %>% 
        summarise(tot_n = sum(med_n), 
                  exp_age_diff = weighted.mean(x = abs(p_mid_age - c_mid_age),
                                               w = med_n)) %>% 
        arrange(p_mid_age) %>% 
        left_join(imd_age %>% 
                    filter(imd_q == i) %>% 
                    mutate(p_age_group = case_when(age=='75+' ~ '77.5-', 
                                                   age=='80+' ~ '82.5-', 
                                                   T ~ age)) %>% 
                    select(p_age_group, prop_imd), by = 'p_age_group') %>% 
        ungroup() %>% 
        summarise(m = weighted.mean(x = exp_age_diff, w = tot_n*prop_imd))
      data[data$p_i==i & data$c_i==j,]$m <- filt$m
    }}}else{
      for(reg in unique(data$p_engreg)){for(i in 1:5){for(j in 1:5){
        filt <- agg_dat %>% filter(p_engreg==reg, p_imd_q==i, c_imd_q==j) %>% 
          ungroup() %>% 
          mutate(p_age_group = case_when(p_age_group=='75+' ~ '77.5-', 
                                         p_age_group=='80+' ~ '82.5-', 
                                         T ~ p_age_group),
                 c_age_group = case_when(c_age_group=='75+' ~ '77.5-',
                                         c_age_group=='80+' ~ '82.5-', 
                                         T ~ c_age_group),
                 p_mid_age = as.numeric(gsub("-.*$", "", p_age_group)) + 2.5,
                 c_mid_age = as.numeric(gsub("-.*$", "", c_age_group)) + 2.5) %>% 
          group_by(p_age_group, p_mid_age) %>% 
          summarise(tot_n = sum(med_n), 
                    exp_age_diff = weighted.mean(x = abs(p_mid_age - c_mid_age),
                                                 w = med_n)) %>% 
          arrange(p_mid_age) %>% 
          left_join(imd_age %>% 
                      filter(imd_q == i, p_engreg == reg) %>% 
                      mutate(p_age_group = case_when(age=='75+' ~ '77.5-', 
                                                     age=='80+' ~ '82.5-', 
                                                     T ~ age)) %>% 
                      select(p_age_group, prop_imd), by = 'p_age_group') %>% 
          ungroup() %>% 
          summarise(m = weighted.mean(x = exp_age_diff, w = tot_n*prop_imd))
        data[data$p_engreg==reg & data$p_i==i & data$c_i==j,]$m <- filt$m
      }}}
    }
  
  data
  
}

mean_imd_diff <- function(agg_dat,
                          abs_value = T){
  
  data <- if(!grepl('regional',sens_analysis)){
    CJ(p_a = age_labels, c_a = age_labels, m = 0)}else{
      CJ(p_engreg=unique(agg_dat$p_engreg),
         p_a = age_labels, c_a = age_labels, m = 0)
    }
  
  if(!grepl('regional',sens_analysis)){
    for(i in age_labels){for(j in age_labels){
      filt <- agg_dat %>% filter(p_age_group==i, c_age_group==j) %>% 
        ungroup() %>% 
        mutate(p_imd_q = as.numeric(p_imd_q),
               c_imd_q = as.numeric(c_imd_q)) 
      
      if(abs_value){
        filt <- filt %>% 
          group_by(p_imd_q) %>% 
          summarise(tot_n = sum(med_n), 
                    exp_imd_diff = weighted.mean(x = abs(c_imd_q - p_imd_q),
                                                 w = med_n)) %>% 
          arrange(p_imd_q)
      }else{
        filt <- filt %>% 
          group_by(p_imd_q) %>% 
          summarise(tot_n = sum(med_n), 
                    exp_imd_diff = weighted.mean(x = (c_imd_q - p_imd_q),
                                                 w = med_n)) %>% 
          arrange(p_imd_q)
      }
      
      filt <- filt %>% 
        left_join(imd_age %>% 
                    rename(p_imd_q = imd_q) %>% 
                    group_by(age) %>% mutate(age_pop=sum(population)) %>% 
                    ungroup() %>% mutate(prop_age = population/age_pop) %>% 
                    filter(age == i) %>%
                    select(p_imd_q, prop_age), by = 'p_imd_q') %>% 
        ungroup() %>% 
        summarise(m = weighted.mean(x = exp_imd_diff, w = tot_n*prop_age))
      data[data$p_a==i & data$c_a==j,]$m <- filt$m
    }}}else{
      for(reg in unique(data$p_engreg)){for(i in 1:5){for(j in 1:5){
        filt <- agg_dat %>% filter(p_engreg==reg, p_age_group==i, c_age_group==j) %>% 
          ungroup() %>% 
          mutate(p_imd_q = as.numeric(p_imd_q),
                 c_imd_q = as.numeric(c_imd_q)) 
        
        if(abs_value){
          filt <- filt %>% 
            group_by(p_imd_q) %>% 
            summarise(tot_n = sum(med_n), 
                      exp_imd_diff = weighted.mean(x = abs(c_imd_q - p_imd_q),
                                                   w = med_n)) %>% 
            arrange(p_imd_q)
        }else{
          filt <- filt %>% 
            group_by(p_imd_q) %>% 
            summarise(tot_n = sum(med_n), 
                      exp_imd_diff = weighted.mean(x = (c_imd_q - p_imd_q),
                                                   w = med_n)) %>% 
            arrange(p_imd_q)
        }
        
        filt <- filt %>% 
          left_join(imd_age %>% 
                      rename(p_imd_q = imd_q) %>% 
                      group_by(age) %>% mutate(age_pop=sum(population)) %>% 
                      ungroup() %>% mutate(prop_age = population/age_pop) %>% 
                      filter(age == i) %>%
                      select(p_imd_q, prop_age), by = 'p_imd_q') %>% 
          ungroup() %>% 
          summarise(m = weighted.mean(x = exp_imd_diff, w = tot_n*prop_age))
        data[data$p_engreg==reg & data$p_a==i & data$c_a==j,]$m <- filt$m
      }}}
    }
  
  data
  
}

fcn_assortativity <- function(
    var, # imd_quintile or age_group
    matrix,
    population_input,
    order,
    ci = 0.95
){
  
  pop <- copy(population_input) 
  
  if(var == 'age_group'){
    pop_var = 'age'
    p_var = 'p_age_group'
    c_var = 'c_age_group'
  }
  if(var == 'imd_quintile'){
    pop_var = 'imd_q'
    p_var = 'p_imd_q'
    c_var = 'c_imd_q'
  }
  
  pop_var_col <- which(colnames(pop) == pop_var)
  setnames(pop, colnames(pop)[pop_var_col], 'p_var')
  
  pop <- pop %>% select('p_var','prop') %>% 
    rename(proportion = prop)
  
  bs_vars <- c()
  if('bootstrap_index' %in% colnames(matrix)){
    if(n_distinct(matrix$bootstrap_index) > 1){
      bs_vars <- c('bootstrap_index')
    }
  }
  
  matrix_rename <- copy(matrix)
  
  p_var_col <- which(colnames(matrix_rename) == p_var)
  setnames(matrix_rename, colnames(matrix_rename)[p_var_col], 'p_var')
  c_var_col <- which(colnames(matrix_rename) == c_var)
  setnames(matrix_rename, colnames(matrix_rename)[c_var_col], 'c_var')
  
  selection_vars <- c('p_var','c_var','n')
  
  beta_thin <- matrix_rename %>% select(!!!syms(bs_vars), !!!syms(selection_vars))
  
  if(!is.null(bs_vars)){
    if(nrow(beta_thin) != n_distinct(beta_thin$bootstrap_index)*n_distinct(beta_thin$p_var)*n_distinct(beta_thin$c_var)){
      stop('Contact matrix defined by index other than bs.')
    }
  }else{
    if(nrow(beta_thin) != n_distinct(beta_thin$p_var)*n_distinct(beta_thin$c_var)){
      stop('Contact matrix defined by index other than bs.')
    }
  }
  
  beta_hat <- beta_thin %>% left_join(pop, by = 'p_var') %>% mutate(beta_hat_var = n*proportion)
  
  beta_hat$p_var <- factor(beta_hat$p_var, levels = order)
  beta_hat$c_var <- factor(beta_hat$c_var, levels = order)
  
  B <- beta_hat %>% left_join(beta_hat %>% group_by(!!!syms(bs_vars), c_var) %>% summarise(sum_beta_hat_var = sum(beta_hat_var)),
                              by = c(bs_vars,'c_var')) %>% 
    mutate(B_var = beta_hat_var/sum_beta_hat_var) %>% 
    drop_na() # remove NAs where dividing by 0, i.e. no contacts reported
  
  Q <- B %>% filter(p_var == c_var) %>% group_by(!!!syms(bs_vars)) %>% 
    summarise(sum_Bii = sum(B_var)) %>% mutate(Q_var = (sum_Bii - 1)/(n_distinct(beta_thin$p_var) - 1)) %>% 
    select(!!!syms(bs_vars), Q_var) %>% filter(!is.na(Q_var))
  
  return(data.table(
    variable = c('Q'),
    mean = c(mean(Q$Q_var)),
    lower_ci = c(quantile(Q$Q_var, (1 - ci)/2)),
    upper_ci = c(quantile(Q$Q_var, 1 - (1 - ci)/2))
  ))
  
}



#### SUMMARISE ####

group_vars <- c('p_age_group', 'c_age_group', 'p_imd_q', 'c_imd_q')
if(grepl('regional',sens_analysis)){group_vars <- c(group_vars, 'p_engreg')}

agg <- balanced_matr %>% 
  group_by(!!!syms(group_vars)) %>% 
  summarise(med_n = mean(n),
            width = quantile(n, 0.975) - quantile(n, 0.025)) 

agg$p_age_group <- factor(agg$p_age_group,
                          levels = age_labels)
agg$c_age_group <- factor(agg$c_age_group,
                          levels = age_labels)
agg$c_imd_q <- factor(agg$c_imd_q,
                      levels = rev(as.character(1:5)))

agg <- agg %>% 
  arrange(p_imd_q, p_age_group, c_age_group)

#### CALCULATE ####

m_a_d <- mean_age_diff(agg)
m_i_d <- mean_imd_diff(agg)
m_i_d_no_abs <- mean_imd_diff(agg, abs_value = F)

if(!grepl('regional',sens_analysis)){
  
  #### NOT REGIONAL ####
  
  ## assortativity by age group
  
  imd_dataframe <- CJ(p_imd_q = 1:5, c_imd_q = 1:5)
  
  imd_assortativity <- map(
    .x = 1:nrow(imd_dataframe),
    .f = ~fcn_assortativity(
      var = 'age_group',
      matrix = balanced_matr %>% 
        filter(p_imd_q == imd_dataframe$p_imd_q[.x], 
               c_imd_q == imd_dataframe$c_imd_q[.x]),
      population_input = imd_age %>% filter(imd_q == imd_dataframe$p_imd_q[.x]),
      order = age_labels
    )
  )
  
  imd_assort_df <- cbind(imd_dataframe, rbindlist(imd_assortativity))
  
  ## assortativity by IMD quintile
  
  age_dataframe <- CJ(p_age_group = age_labels, c_age_group = age_labels) 
  age_dataframe$p_age_group <- factor(age_dataframe$p_age_group, levels = age_labels)
  age_dataframe$c_age_group <- factor(age_dataframe$c_age_group, levels = age_labels)
  age_dataframe <- age_dataframe %>% arrange(p_age_group, c_age_group)
  
  age_assortativity <- map(
    .x = 1:nrow(age_dataframe),
    .f = ~fcn_assortativity(
      var = 'imd_quintile',
      matrix = balanced_matr %>% 
        filter(p_age_group == age_dataframe$p_age_group[.x], 
               c_age_group == age_dataframe$c_age_group[.x]),
      population_input = imd_age %>% filter(age == age_dataframe$p_age_group[.x]) %>% 
        mutate(prop = population/sum(population)),
      order = as.character(1:5)
    )
  )
  
  age_assort_df <- cbind(age_dataframe, rbindlist(age_assortativity))
  
}else{
  
  #### REGIONAL ####
  
  imd_assort_df <- data.frame()
  
  for(reg in unique(balanced_matr$p_engreg)){
    
    imd_dataframe <- CJ(p_imd_q = 1:5, c_imd_q = 1:5)
    
    imd_assortativity <- map(
      .x = 1:nrow(imd_dataframe),
      .f = ~fcn_assortativity(
        var = 'age_group',
        matrix = balanced_matr %>% 
          filter(p_engreg == reg,
                 p_imd_q == imd_dataframe$p_imd_q[.x], 
                 c_imd_q == imd_dataframe$c_imd_q[.x]),
        population_input = imd_age %>% filter(imd_q == imd_dataframe$p_imd_q[.x],
                                              p_engreg == reg),
        order = age_labels
      )
    )
    
    imd_assort_df <- rbind(imd_assort_df,
                           cbind(imd_dataframe, rbindlist(imd_assortativity), p_engreg = reg))
    
    cat(reg, '-')
    
  }
  
  age_assort_df <- data.table()
  
  for(reg in unique(balanced_matr$p_engreg)){
    
    age_dataframe <- CJ(p_age_group = age_labels, c_age_group = age_labels) 
    age_dataframe$p_age_group <- factor(age_dataframe$p_age_group, levels = age_labels)
    age_dataframe$c_age_group <- factor(age_dataframe$c_age_group, levels = age_labels)
    age_dataframe <- age_dataframe %>% arrange(p_age_group, c_age_group)
    
    age_assortativity <- map(
      .x = 1:nrow(age_dataframe),
      .f = ~fcn_assortativity(
        var = 'imd_quintile',
        matrix = balanced_matr %>% 
          filter(p_engreg == reg,
                 p_age_group == age_dataframe$p_age_group[.x], 
                 c_age_group == age_dataframe$c_age_group[.x]),
        population_input = imd_age %>% filter(p_engreg == reg,
                                              age == age_dataframe$p_age_group[.x]) %>% 
          mutate(prop = population/sum(population)),
        order = as.character(1:5)
      )
    )
    
    age_assort_df <- rbind(age_assort_df,
                           cbind(age_dataframe, rbindlist(age_assortativity), p_engreg=reg))
    
    cat(reg, '-')
    
  }
}
  

#### SAVE ####
write_csv(m_a_d, gsub('imd_assortativity.csv','m_a_d.csv',.args[3]))
write_csv(m_i_d, gsub('imd_assortativity.csv','m_i_d.csv',.args[3]))
write_csv(m_i_d_no_abs, gsub('imd_assortativity.csv','m_i_d_no_abs.csv',.args[3]))
write_csv(age_assort_df, gsub('imd_assortativity.csv','age_assortativity.csv',.args[3]))
write_csv(imd_assort_df, .args[3])

