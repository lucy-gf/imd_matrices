
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
  file.path("output", "figures", "cont_matrs","regional","summstats.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))
source(here::here('scripts','setup','colors.R'))

if(!file.exists(gsub('/summstats.png','',.args[3]))){dir.create(gsub('/summstats.png','',.args[3]))}

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

#### PLOT ####

#### MEAN AGE DIFFERENCE ####
## (using midpoint of each age group), 
## 80 for 75+ age group
## (or 85 for 80+ age group in nhs_ages)

m_a_d <- mean_age_diff(agg)

mean_age_diff_plot <- m_a_d %>% 
  ggplot() + 
  geom_tile(aes(x=p_i, y=c_i, fill=m)) +
  theme_bw() + scale_fill_viridis(option='A') +
  theme(text = element_text(size = 12)) +
  labs(x='Participant IMD quintile',
       y='Contact IMD quintile',
       fill = 'Mean age\ndifference')

if(grepl('regional',sens_analysis)){
  mean_age_diff_plot <- mean_age_diff_plot + facet_wrap(.~p_engreg)
}

mean_age_diff_plot + ggtitle("Mean age difference")

ggsave(gsub('summstats.png','mean_age_diff.png',.args[3]), width = 8, height = 7)

m_a_d <- mean_age_diff(agg %>% filter(p_age_group != c_age_group))

mean_age_diff_plot_nodiag <- m_a_d %>% 
  ggplot() + 
  geom_tile(aes(x=p_i, y=c_i, fill=m)) +
  theme_bw() + scale_fill_viridis(option='A') +
  theme(text = element_text(size = 12)) +
  labs(x='Participant IMD quintile',
       y='Contact IMD quintile',
       fill = 'Mean age\ndifference') +
  ggtitle("Mean age difference") +
  ggtitle("Mean age difference, excluding the age-diagonal")

if(grepl('regional',sens_analysis)){
  mean_age_diff_plot_nodiag <- mean_age_diff_plot_nodiag + facet_wrap(.~p_engreg)
}

mean_age_diff_plot_nodiag  

ggsave(gsub('summstats.png','mean_age_diff_no_diag.png',.args[3]), width = 8, height = 7)

## children's contacts only
m_a_d <- mean_age_diff(agg %>% 
                         filter(p_age_group %notin% c("75+","80+")) %>% 
                         filter(as.numeric(gsub("-.*$", "", p_age_group)) < 20))

mean_age_diff_plot_children <- m_a_d %>% 
  ggplot() + 
  geom_tile(aes(x=p_i, y=c_i, fill=m)) +
  theme_bw() + scale_fill_viridis(option='A') +
  theme(text = element_text(size = 12)) +
  labs(x='Participant IMD quintile',
       y='Contact IMD quintile',
       fill = 'Mean age\ndifference') +
  ggtitle("Mean age difference") +
  ggtitle("Mean age difference, children's contacts only")

if(grepl('regional',sens_analysis)){
  mean_age_diff_plot_children <- mean_age_diff_plot_children + facet_wrap(.~p_engreg)
}

mean_age_diff_plot_children  

ggsave(gsub('summstats.png','mean_age_diff_only_kids.png',.args[3]), width = 8, height = 7)

#### MEAN IMD DIFFERENCE ####

m_i_d <- mean_imd_diff(agg)

m_i_d$p_a <- factor(m_i_d$p_a, levels = age_labels)
m_i_d$c_a <- factor(m_i_d$c_a, levels = age_labels)

m_i_d_plot <- m_i_d %>% 
  ggplot() + 
  geom_tile(aes(x=p_a, y=c_a, fill=m)) +
  theme_bw() + scale_fill_viridis(option='A') +
  theme(text = element_text(size = 12)) +
  labs(x='Participant age group',
       y='Contact age group',
       fill = 'Mean absolute\nIMD difference') + 
  coord_fixed()

if(grepl('regional',sens_analysis)){
  m_i_d_plot <- m_i_d_plot + facet_wrap(.~p_engreg)
}

m_i_d_plot
ggsave(gsub('summstats.png','mean_imd_diff.png',.args[3]), width = 8.5, height = 7)

m_i_d <- mean_imd_diff(agg, abs_value = F)

m_i_d$p_a <- factor(m_i_d$p_a, levels = age_labels)
m_i_d$c_a <- factor(m_i_d$c_a, levels = age_labels)

m_i_d_plot_no_abs <- m_i_d %>% 
  ggplot() + 
  geom_tile(aes(x=p_a, y=c_a, fill=m)) +
  theme_bw() + 
  scale_fill_gradientn(colours = c("blue", "dodgerblue", "white", "orange", "red"),
                      rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  theme(text = element_text(size = 12)) +
  labs(x='Participant age group',
       y='Contact age group',
       fill = 'Mean IMD\ndifference') + 
  coord_fixed(); m_i_d_plot_no_abs

if(sens_analysis == 'regional'){
  m_i_d_plot_no_abs <- m_i_d_plot_no_abs + facet_wrap(.~p_engreg)
}

m_i_d_plot_no_abs
ggsave(gsub('summstats.png','mean_imd_diff_not_abs.png',.args[3]), width = 8, height = 7)


#### ASSORTATIVITY ####

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
  
  imd_assort_matr <- imd_assort_df %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = mean)) + 
    scale_fill_viridis() + 
    theme_bw() +
    labs(x = 'Participant IMD quintile', y = 'Contact IMD quintile', 
         fill = 'Assortativity\nby age group') + 
    theme(text = element_text(size = 12))
  
  age_assort_matr <- age_assort_df %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = mean)) + 
    scale_fill_viridis(limits = c(0,NA), trans='pseudo_log') + 
    theme_bw() +
    labs(x = 'Participant age group', y = 'Contact age group', 
         fill = 'Assortativity\nby IMD quintile') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          text = element_text(size = 12))
  
  imd_assort_matr + age_assort_matr + plot_layout(nrow = 1)
  
  ggsave(gsub('.png','_assortativity_matrices.png',.args[3]), width = 12, height = 5)
  
  age_assort_matr + imd_assort_matr + mean_age_diff_plot + plot_layout(nrow = 1) +
    plot_annotation(tag_levels = 'a',
                    tag_prefix = '(', tag_suffix = ')')
  
  ggsave(file = .args[3], width = 18, height = 5)
  
  age_assort_matr
  
  ggsave(gsub('summstats.png','imd_assortativity_matrix.png',.args[3]), width = 6, height = 5)
  
}else{
  
  #### REGIONAL ####
  ## assortativity by age group
  
  ## switch to F if needing to rerun
  imd_read <- T
  age_read <- T
  
  if(!imd_read){
    
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
    
    write_csv(imd_assort_df, gsub('summstats.png','imd_assortativity.csv',.args[3]))
    
  }else{
    
    imd_assort_df <- read_csv(gsub('summstats.png','imd_assortativity.csv',.args[3]), show_col_types = F)
    
  }
  
  if(!age_read){
    
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
    
    write_csv(age_assort_df, gsub('summstats.png','age_assortativity.csv',.args[3]))
    
  }else{
    
    age_assort_df <- read_csv(gsub('summstats.png','age_assortativity.csv',.args[3]), show_col_types = F)
    
    age_assort_df$p_age_group <- factor(age_assort_df$p_age_group,
                                        levels = age_labels)
    age_assort_df$c_age_group <- factor(age_assort_df$c_age_group,
                                        levels = age_labels)
    
  }
  
  ## assortativity by IMD quintile
  
  imd_assort_matr <- imd_assort_df %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = mean)) + 
    scale_fill_viridis() + 
    theme_bw() + facet_wrap(.~p_engreg) +
    labs(x = 'Participant IMD quintile', y = 'Contact IMD quintile', 
         fill = 'Assortativity\nby age group') + 
    theme(text = element_text(size = 12))
  
  age_assort_matr <- age_assort_df %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = mean)) + 
    scale_fill_viridis(#limits = c(0,NA), 
                       trans='pseudo_log') + 
    theme_bw() + facet_wrap(.~p_engreg) +
    labs(x = 'Participant age group', y = 'Contact age group', 
         fill = 'Assortativity\nby IMD quintile') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          text = element_text(size = 12))
  
  imd_assort_matr + age_assort_matr + plot_layout(nrow = 1)
  
  ggsave(gsub('summstats.png','assortativity_matrices.png',.args[3]), width = 20, height = 10)
  
  age_assort_matr + imd_assort_matr + mean_age_diff_plot + plot_layout(nrow = 1) +
    plot_annotation(tag_levels = 'a',
                    tag_prefix = '(', tag_suffix = ')')
  
  ggsave(gsub('.png','_regional.png',.args[3]), width = 30, height = 10)
  
  age_assort_matr
  
  ggsave(gsub('summstats.png','imd_assortativity_matrix.png',.args[3]), width = 10, height = 10)
  
}



## proportion of contacts intra-group

prop_in_group_df <- balanced_matr %>% 
  mutate(p_var = paste0(p_imd_q, '_', p_age_group),
         c_var = paste0(c_imd_q, '_', c_age_group),
         flag = p_var==c_var) %>% 
  group_by(bootstrap_index, p_var, flag) %>% 
  mutate(n_flag = sum(n)) %>% 
  ungroup() %>% 
  select(bootstrap_index, p_age_group, p_imd_q, flag, n_flag) %>% 
  unique() %>% 
  pivot_wider(names_from = flag, values_from = n_flag) %>% 
  mutate(prop_in_group = `TRUE`/(`TRUE` + `FALSE`))

prop_in_group_df$p_age_group <- factor(prop_in_group_df$p_age_group,
                                       levels = age_labels)

prop_in_group_plot <- prop_in_group_df %>% 
  group_by(p_age_group, p_imd_q) %>% 
  summarise(m = median(prop_in_group),
            l = quantile(prop_in_group, 0.025),
            u = quantile(prop_in_group, 0.975)) %>% 
  ggplot() + 
  geom_ribbon(aes(x = p_age_group, ymin = l, ymax=u, fill = as.factor(p_imd_q),
                  group = as.factor(p_imd_q)), alpha = 0.4) + 
  geom_line(aes(x = p_age_group, y = m, col = as.factor(p_imd_q),
                group = as.factor(p_imd_q)), lwd = 0.8) + 
  theme_bw() + labs(x = 'Age',y='Proportion of contacts in\nage group and IMD quintile',
                    col = 'IMD quintile', fill = 'IMD quintile') + 
  scale_color_manual(values = imd_quintile_colors) +
  scale_fill_manual(values = imd_quintile_colors) +
  theme(text = element_text(size = 14)) + ylim(c(0,NA))

prop_in_age_df <- balanced_matr %>% 
  mutate(flag = p_age_group==c_age_group) %>% 
  group_by(bootstrap_index, p_age_group, p_imd_q, flag) %>% 
  mutate(n_flag = sum(n)) %>% 
  ungroup() %>% 
  select(bootstrap_index, p_age_group, p_imd_q, flag, n_flag) %>% 
  unique() %>% 
  pivot_wider(names_from = flag, values_from = n_flag) %>% 
  mutate(prop_in_group = `TRUE`/(`TRUE` + `FALSE`))

prop_in_age_df$p_age_group <- factor(prop_in_age_df$p_age_group,
                                     levels = age_labels)

prop_in_age_plot <- prop_in_age_df %>% 
  group_by(p_age_group, p_imd_q) %>% 
  summarise(m = median(prop_in_group),
            l = quantile(prop_in_group, 0.025),
            u = quantile(prop_in_group, 0.975)) %>% 
  ggplot() + 
  geom_ribbon(aes(x = p_age_group, ymin = l, ymax=u, fill = as.factor(p_imd_q),
                  group = as.factor(p_imd_q)), alpha = 0.4) + 
  geom_line(aes(x = p_age_group, y = m, col = as.factor(p_imd_q),
                group = as.factor(p_imd_q)), lwd = 0.8) + 
  theme_bw() + labs(x = 'Age',y='Proportion of contacts\nin age group',
                    col = 'IMD quintile', fill = 'IMD quintile') + 
  scale_color_manual(values = imd_quintile_colors) +
  scale_fill_manual(values = imd_quintile_colors) +
  theme(text = element_text(size = 14)) + ylim(c(0,NA))

prop_in_imd_df <- balanced_matr %>% 
  mutate(flag = p_imd_q==c_imd_q) %>% 
  group_by(bootstrap_index, p_age_group, p_imd_q, flag) %>% 
  mutate(n_flag = sum(n)) %>% 
  ungroup() %>% 
  select(bootstrap_index, p_age_group, p_imd_q, flag, n_flag) %>% 
  unique() %>% 
  pivot_wider(names_from = flag, values_from = n_flag) %>% 
  mutate(prop_in_group = `TRUE`/(`TRUE` + `FALSE`))

prop_in_imd_df$p_age_group <- factor(prop_in_imd_df$p_age_group,
                                     levels = age_labels)

prop_in_imd_plot <- prop_in_imd_df %>% 
  group_by(p_age_group, p_imd_q) %>% 
  summarise(m = median(prop_in_group),
            l = quantile(prop_in_group, 0.025),
            u = quantile(prop_in_group, 0.975)) %>% 
  ggplot() + 
  geom_ribbon(aes(x = p_age_group, ymin = l, ymax=u, fill = as.factor(p_imd_q),
                  group = as.factor(p_imd_q)), alpha = 0.4) + 
  geom_line(aes(x = p_age_group, y = m, col = as.factor(p_imd_q),
                group = as.factor(p_imd_q)), lwd = 0.8) + 
  theme_bw() + labs(x = 'Age',y='Proportion of contacts\nin IMD quintile',
                    col = 'IMD quintile', fill = 'IMD quintile') + 
  scale_color_manual(values = imd_quintile_colors) +
  scale_fill_manual(values = imd_quintile_colors) +
  theme(text = element_text(size = 14)) + ylim(c(0,NA))

prop_in_group_plot + prop_in_age_plot + 
  prop_in_imd_plot +
  plot_layout(nrow=3, guides='collect')
