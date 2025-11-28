
## FORMAT SCHOOLS DATA ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(ggplot2)
library(viridis)
library(stringr)
library(RColorBrewer)

imd_year <- c(19, 25)[2]

## read in data ##

load(here::here('data','dfe',paste0('imd',imd_year),'cm_23.rdata'))
cm_23 <- cms 

load(here::here('data','dfe',paste0('imd',imd_year),'cm_24.rdata'))
cm_24 <- cms 

load(here::here('data','dfe',paste0('imd',imd_year),'cm_25.rdata'))
cm_25 <- cms 


if(imd_year == 19){
  
  load(here::here('data','dfe','imd19','cm_23_cities.rdata'))
  cm_23_cities <- cms 
  
  load(here::here('data','dfe','imd19','cm_24_cities.rdata'))
  cm_24_cities <- cms 
  
}


## function to check sums ##

check_sums <- function(dat, dat_name = ''){
  
  cols <- colnames(dat)
  group_cols <- cols[cols %notin% c('value','n_attr_tot','contact')]
  group_cols <- group_cols[str_sub(group_cols, -2, -1) != '_c']
  
  test <- dat %>% group_by(!!!syms(group_cols)) %>% 
    summarise(s = sum(value)) %>% 
    filter(abs(s - 1) > 1e-10) 
  
  if(nrow(test) > 0){cat('\n', dat_name, ' sums neq 1', sep = '')}
  
}

## function to turn "NA" into NA

function_NA_convert <- function(column){
  
  column[which(column == 'NA')] <- NA
  
  column

}

## function to check for NAs ##

check_NAs <- function(dat, dat_name = ''){
  
  for(col_name in names(dat)){
    
    nrow_na <- nrow(dat %>% filter(is.na(!!sym(col_name))))
    
    if(nrow_na > 0 & col_name %notin% c('value','n_attr_tot')){
      cat('\n', dat_name, ' has NA in column ', col_name, sep = '')
    }
    
  }
  
}

## functions to scale and balance the data ##

fcn_scale <- function(data_raw, dat_name = ''){
  
  dat <- data_raw
  
  # grouping columns
  cols <- colnames(dat)
  group_cols <- cols[cols %notin% c('value','n_attr_tot','contact')]
  group_cols <- group_cols[str_sub(group_cols, -2, -1) != '_c']
  
  y_var <- cols[str_sub(cols, -2, -1) == '_c']
  x_var <- gsub('_c','', y_var)
  group_cols_cont <- gsub(x_var, y_var, group_cols)
  
  # remove NA values
  dat <- dat %>% 
    drop_na()
  
  # rescale probabilities to sum to one
  dat <- dat %>% 
    group_by(!!!syms(group_cols)) %>% 
    mutate(sum_val = sum(value)) %>% 
    ungroup() %>% 
    mutate(value = value/sum_val) %>% 
    select(!sum_val)
  
  # check new sums add to 1
  check_sums(dat, paste0(dat_name, ' rescaled'))
  
  dat
  
}

fcn_balance <- function(data_raw, dat_name = ''){
  
  dat <- data_raw
  
  # grouping columns
  cols <- colnames(dat)
  group_cols <- cols[cols %notin% c('value','n_attr_tot','contact')]
  group_cols <- group_cols[str_sub(group_cols, -2, -1) != '_c']
  
  y_var <- cols[str_sub(cols, -2, -1) == '_c']
  x_var <- gsub('_c','', y_var)
  group_cols_cont <- gsub(x_var, y_var, group_cols)
  
  # make reciprocal
  dat_sizes <- dat %>% select(!!!syms(group_cols), n_attr_tot)
  dat_sizes <- unique(dat_sizes)
  colnames(dat_sizes) <- c(group_cols_cont, 'n_attr_tot_c')
  
  dat_t <- dat %>% 
    select(!!!syms(group_cols), !!sym(y_var), value)
  colnames(dat_t) <- c(group_cols_cont, x_var, 'value_t')
  
  dat_recip <- dat %>% 
    left_join(dat_sizes, by = group_cols_cont) %>% 
    left_join(dat_t, by = c(group_cols, y_var)) %>% 
    mutate(new_value = case_when(!is.na(n_attr_tot_c) & !is.na(value_t) ~ ((value*n_attr_tot) + (value_t*n_attr_tot_c))/(2*n_attr_tot),
                                 T ~ value))
  
  max_change <- round(max(abs(dat_recip$value - dat_recip$new_value)),3)
  
  if(max_change > 0.01){
    cat(paste0('\nMax. change in value after balancing (matrix ', dat_name, '): ', max_change), sep = '')
  }
  
  dat_recip <- dat_recip %>% 
    select(!!!syms(group_cols), !!sym(y_var), n_attr_tot, new_value) %>% 
    rename(value = new_value)
  
  dat_recip
  
}

## function to convert colnames to names ##

format_colname <- function(colname){
  
  out <- colname
  out <- gsub('Agp','Age group', out)
  out <- gsub('imd_ten', 'IMD Decile', out)
  out <- gsub('imd_five', 'IMD Quintile', out)
  
  if(grepl('_c', out)){
    out <- paste0('Contact ', gsub('_c','',out))
  }
  
  out
  
}

## function to plot data ##

plot_dfe_data <- function(dat_raw, dat_name = ''){
  
  dat <- copy(dat_raw)
  
  # find column names
  
  col_names <- colnames(dat)
  
  y_var <- col_names[str_sub(col_names, -2, -1) == '_c']
  x_var <- gsub('_c','', y_var)
  
  indices <- which(col_names %notin% c(x_var, y_var, 'value', 'n_attr_tot'))
  strata_exist <- length(indices > 0)
  if(strata_exist){
    strata <- col_names[indices]
    colnames(dat)[indices] <- paste('strata_', 1:length(indices), sep = '')
    indices_out <<- 1:length(indices)
  }else{indices <- 0; indices_out <<- 0}
  
  plot <- dat %>% 
    drop_na() %>% 
    ggplot() + 
    geom_tile(aes(x = !!sym(x_var), y = !!sym(y_var), fill = value)) + 
    theme_bw() +
    scale_fill_viridis(begin = 0, end = 1, breaks = seq(0, 1, by = 0.25)) +
    labs(x = format_colname(x_var), y = format_colname(y_var), fill = 'Probability') +
    ggtitle(dat_name) +
    theme(text = element_text(size = 16))
  
  if(strata_exist){
    if(length(strata) == 1){
      n_strata_1 <<- n_distinct(dat$strata_1)
        
      plot <- plot + 
        facet_grid(strata_1 ~ .)
    }
    if(length(strata) == 2){
      n_strata_1 <<- n_distinct(dat$strata_1)
      n_strata_2 <<- n_distinct(dat$strata_2)
      
      plot <- plot + 
        facet_grid(strata_1 ~ strata_2)
    }
  }
  
  plot
  
}

## function to plot data (per capita) ##

plot_dfe_data_per_capita <- function(dat_raw, dat_name = ''){
  
  dat <- copy(dat_raw)
  
  # find column names
  
  col_names <- colnames(dat)
  
  y_var <- col_names[str_sub(col_names, -2, -1) == '_c']
  x_var <- gsub('_c','', y_var)
  
  indices <- which(col_names %notin% c(x_var, y_var, 'value', 'n_attr_tot'))
  strata_exist <- length(indices > 0)
  if(strata_exist){
    strata <- col_names[indices]
    colnames(dat)[indices] <- paste('strata_', 1:length(indices), sep = '')
    part_columns <- c(x_var, paste('strata_', 1:length(indices), sep = ''))
    contact_columns <- c(y_var, paste('strata_', 1:length(indices), sep = ''))
    indices_out <<- 1:length(indices)
  }else{
    indices <- 0; indices_out <<- 0
    part_columns <- c(x_var)
    contact_columns <- c(y_var)
  }
  
  # add contact group sizes
  dat_sizes <- dat %>% select(!!!syms(part_columns), n_attr_tot)
  names(dat_sizes) <- c(contact_columns, 'n_attr_tot_c')
  dat_sizes <- unique(dat_sizes)
  dat_pc <- dat %>% 
    left_join(dat_sizes, by = contact_columns) %>% 
    group_by(!!!syms(part_columns)) %>% 
    mutate(tot_attr_tot = sum(n_attr_tot_c)) %>% 
    ungroup() %>% 
    mutate(prop_contacts = n_attr_tot_c/tot_attr_tot,
           rel_rate_contacts = (value - prop_contacts)/prop_contacts) %>% 
    drop_na()
  
  # scaling the legend
  vec <- c(-1, -0.8, -0.6, -0.4, -0.1, 0.1, 0.5, 1, 2, 3)
  break_vector <- sort(c(vec, -Inf, Inf))
  
  dat_pc <- dat_pc %>% 
    mutate(discrete_abs_rel_rate = cut(rel_rate_contacts, breaks = break_vector))
  
  values_brewer <- cut(sort(c(vec + 0.01, -1e6)), breaks = break_vector)
  
  missing_levels <- setdiff(values_brewer, dat_pc$discrete_abs_rel_rate)
  
  dat_pc <- dat_pc %>% 
    drop_na() %>%
    mutate(discrete_abs_rel_rate = factor(discrete_abs_rel_rate, levels = values_brewer))
  
  cols <- brewer.pal(brewer.pal.info['PiYG', "maxcolors"], 'PiYG')
  names(cols) <- values_brewer
  
  plot <- dat_pc %>% 
    ggplot() + 
    geom_tile(aes(x = !!sym(x_var), y = !!sym(y_var), fill = discrete_abs_rel_rate)) + 
    theme_bw() +
    scale_fill_manual(values = cols,
                      guide = guide_legend(reverse = T)) +
    labs(x = format_colname(x_var), y = format_colname(y_var), fill = '') +
    ggtitle(paste0(dat_name, ', relative to random mixing')) +
    theme(text = element_text(size = 14)); plot
  
  if(strata_exist){
    if(length(strata) == 1){
      n_strata_1 <<- n_distinct(dat$strata_1)
      
      plot <- plot + 
        facet_grid(strata_1 ~ .)
    }
    if(length(strata) == 2){
      n_strata_1 <<- n_distinct(dat$strata_1)
      n_strata_2 <<- n_distinct(dat$strata_2)
      
      plot <- plot + 
        facet_grid(strata_1 ~ strata_2)
    }
  }
  
  plot 
  
}

## function to inspect and plot ##

inspect_and_plot <- function(data_list, name, folder){
  
  if(grepl('Age1', name) & grepl('Age2', name)){return()}
  if(name == 'Counts'){return()}
  
  # select tibble
  data <- data_list[[name]]
  data <- data %>% 
    mutate_all(function_NA_convert)
  
  if(nrow(data %>% drop_na()) == 0){
    cat('\nAll NA in ', name, sep = '')
    return()
    }
  
  # check sums
  check_sums(data, name)
  
  # check for NAs
  #check_NAs(data, name)
  
  # remove NAs and rescale probabilities
  data <- fcn_scale(data, name)
  
  # balance UNLESS stratified by urban/rural
  balance <- !grepl('_UR_', name)
  
  # balance the matrices
  if(balance){
    data <- fcn_balance(data, name)
  }
  
  # plot 
  plot_dfe_data(data, name)
  
  # save
  height <- ifelse(1 %in% indices_out, 3*n_strata_1, 6)
  width <- ifelse(2 %in% indices_out, 3*n_strata_2, 7)
  if(!dir.exists(folder)){dir.create(folder)}
  ggsave(file.path(folder, paste0(name, '.png')),
         width = width, height = height)
  
  # plot 
  plot_dfe_data_per_capita(data, name)
  
  # save
  height <- ifelse(1 %in% indices_out, 3*n_strata_1, 6)
  width <- ifelse(2 %in% indices_out, 3*n_strata_2, 7)
  if(!dir.exists(paste0(folder,'/per_cap'))){dir.create(paste0(folder,'/per_cap'))}
  ggsave(file.path(folder, 'per_cap', paste0(name, '.png')),
         width = width, height = height)
  
  # save data
  data_folder <- gsub('figures','data',folder); if(!dir.exists(data_folder)){dir.create(data_folder)}
  if(name %in% c('cm_IMD5_Age1Region_class', 'cm_IMD5_Age1Region_school',
                 'cm_IMD5_Age1_class', 'cm_IMD5_Age1_school',
                 'cm_IMD5_Age2Region_class', 'cm_IMD5_Age2Region_school',
                 'cm_IMD5_Age2_class', 'cm_IMD5_Age2_school')){
    write_csv(data, file.path(data_folder, paste0(name, '.csv')))
  }
  
}

#### RUN DATA ANALYSIS ####

year_vec <- c(2023:2025)

for(year in year_vec){
  
  cat('\n\n################')
  cat('\n## ', year,' data: ##', sep = '')
  cat('\n################')
  
  subyr <- substr(year, 3, 4)
  
  folder <- here::here('output','figures','cont_matrs','dfe',paste0('imd',imd_year),subyr)
  
  for(name_x in names(get(paste0('cm_', subyr)))){
    
    inspect_and_plot(
      data_list = get(paste0('cm_', subyr)), 
      name = name_x, 
      folder)
    
  }
  
}

#### CITIES ANALYSIS ####

if(imd_year == 19){
  
  year_vec <- c(2023:2024)
  
  for(year in year_vec){
    
    cat('\n\n##################')
    cat('\n## ', year,' cities: ##', sep = '')
    cat('\n##################')
    
    subyr <- substr(year, 3, 4)
    
    folder <- here::here('output','figures','cont_matrs','dfe',paste0(subyr, '_cities'))
    
    for(name_x in names(get(paste0('cm_', subyr, '_cities')))){
      
      inspect_and_plot(
        data_list = get(paste0('cm_', subyr, '_cities')), 
        name = name_x, 
        folder)
      
    }
    
  }
  
}



  
