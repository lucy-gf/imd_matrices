
## PLOT MEAN CONTACTS BY IMD, AGE GROUP, GENDER ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(MASS, warn.conflicts = FALSE)
library(ggplot2)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","participants.rds"),
  "base",
  file.path("output", "data", "cont_matrs","base","mean_contacts","mean_contacts.csv")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

sens_analysis <- .args[2]

## make output folders

output_folder <- paste0("output/data/cont_matrs/",sens_analysis,"/mean_contacts")
if(!file.exists(output_folder)){dir.create(output_folder)}
output_folder_figs <- gsub('data','figures',output_folder)
if(!file.exists(output_folder_figs)){dir.create(output_folder_figs)}

## read in participant data

part <- readRDS(.args[1])

part_reconnect <- readRDS(file.path("data","reconnect","reconnect_part.rds"))

# attach gender, day of week
part <- part %>% left_join(part_reconnect %>% select(p_id, p_gender, day_week),
                           by = 'p_id') %>% 
  mutate(total_contacts = n_contacts + large_n)

# combination of variables

group_vars_list <- list(c('imd_quintile'),
                        c('imd_quintile','p_age_group'),
                        c('imd_quintile','p_gender'),
                        c('imd_quintile','p_age_group','p_gender'))

## function to fit data

fit_mean_contacts <- function(group_index){
  
  group_vars <- group_vars_list[[group_index]]
  
  group_vars_bs <- c(group_vars, 'bootstrap_index')
  
  ## remove 'other' gender if looking at gender
  if('p_gender' %in% group_vars){
    part <- part %>% filter(p_gender %in% c('Male','Female'))
  }
  
  part_dt <- data.table(part %>% select(!!!syms(group_vars_bs), total_contacts))
  
  # apply negative binomial function
  out <- part_dt[, lapply(.SD, neg_bin_fcn), by = group_vars_bs]
  
  # extract mean and shape
  out[, k := as.numeric(sub("^[^_]*_", "", total_contacts))]
  out[, n := as.numeric(sub("(.*)_.*", "\\1", total_contacts))]
  
  # confidence interval functions
  eti95L <- function(x) quantile(x, 0.025)
  eti95U <- function(x) quantile(x, 0.975)
  
  # aggregate over bootstrap indices
  out_agg_raw <- copy(out)
  out_agg_raw[, total_contacts := NULL][, bootstrap_index := NULL]
  out_agg <- rbind(out_agg_raw[, lapply(.SD, mean), by = group_vars][, measure := 'mean'],
                   out_agg_raw[, lapply(.SD, eti95L), by = group_vars][, measure := 'lower'],
                   out_agg_raw[, lapply(.SD, eti95U), by = group_vars][, measure := 'upper'])
  
  if('p_age_group' %in% group_vars){
    out_agg$p_age_group <- factor(out_agg$p_age_group,
                                  levels = age_labels)
    out_agg <- out_agg %>% arrange(p_age_group)
  }
  
  out_agg 
  
}

## function to read in data

read_dat <- function(i){
  data.table(read_csv(gsub('.csv', paste0('_', paste(group_vars_list[[i]],collapse = '_'), '.csv'),.args[3]),
                      show_col_types = F))
}

## function to plot

plot_mean_contacts <- function(out_agg, # dataframe
                               group_index){
  
  if('p_age_group' %in% group_vars_list[[group_index]]){
    out_agg$p_age_group <- factor(out_agg$p_age_group,
                                  levels = age_labels)
    out_agg <- out_agg %>% arrange(p_age_group)
  }
  
  plot_df <- out_agg %>% 
    select(!k) %>% 
    pivot_wider(names_from = measure, values_from = n) 
  
  if(group_index == 1){
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = as.factor(imd_quintile), 
                        col = as.factor(imd_quintile)), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = as.factor(imd_quintile), y = mean,
                     col = as.factor(imd_quintile)),
                 position = position_dodge(width = 0.9)) +
      scale_color_manual(values = imd_quintile_colors) + 
      scale_linetype_manual(values = c(1,2)) + 
      theme_bw() + 
      ylim(c(0, NA)) + 
      labs(col = 'IMD quintile', fill = 'IMD quintile',
           y = 'Mean contacts', x = '',
           linetype = 'Gender')
  }
  if(group_index == 2){
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = p_age_group,
                        group = as.factor(imd_quintile), 
                        col = as.factor(imd_quintile)), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = p_age_group, y = mean,
                     group = as.factor(imd_quintile), 
                     col = as.factor(imd_quintile)),
                 position = position_dodge(width = 0.9)) +
      scale_color_manual(values = imd_quintile_colors) + 
      scale_linetype_manual(values = c(1,2)) + 
      theme_bw() + 
      ylim(c(0, NA)) + 
      labs(col = 'IMD quintile', fill = 'IMD quintile',
           x = 'Mean contacts', y = '',
           linetype = 'Gender')
  }
  if(group_index == 3){
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = p_gender,
                        group = as.factor(imd_quintile), 
                        col = as.factor(imd_quintile)), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = p_gender, y = mean,
                     group = as.factor(imd_quintile), 
                     col = as.factor(imd_quintile)),
                 position = position_dodge(width = 0.9)) +
      scale_color_manual(values = imd_quintile_colors) + 
      scale_linetype_manual(values = c(1,2)) + 
      theme_bw() + 
      ylim(c(0, NA)) + 
      labs(col = 'IMD quintile', fill = 'IMD quintile',
           x = 'Mean contacts', y = '',
           linetype = 'Gender')
  }
  if(group_index == 4){
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = p_age_group,
                        group = interaction(as.factor(imd_quintile), p_gender), 
                        col = as.factor(imd_quintile), lty = p_gender), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = p_age_group, y = mean,
                     group = interaction(as.factor(imd_quintile), p_gender), 
                     col = as.factor(imd_quintile)),
                 position = position_dodge(width = 0.9)) +
      scale_color_manual(values = imd_quintile_colors) + 
      scale_linetype_manual(values = c(1,2)) + 
      theme_bw() + 
      ylim(c(0, NA)) + 
      labs(col = 'IMD quintile', fill = 'IMD quintile',
           y = 'Mean contacts', x = '',
           linetype = 'Gender')
  }
  
  p
  
}
  
## fit data, or read in 
read <- c(T, T, T, T)

data_list <- map(
  .x = 1:length(group_vars_list),
  .f = ifelse(read[.x], read_dat, fit_mean_contacts)
)

names(data_list) <- unlist(lapply(group_vars_list, function(x) paste(x,collapse = '_')))

## save any that were run
for(i in 1:length(read)){
  if(!read[i]){
    ## save data
    write_csv(data_list[[i]], gsub('.csv', paste0('_', paste(group_vars_list[[i]],collapse = '_'), '.csv'),.args[3]))
  }
}

## plot

plots <- map(
  .x = 1:length(data_list),
  .f = ~{plot_mean_contacts(data_list[[.x]], .x)}
)

widths <- c(6, 12, 7, 14)
heights <- c(4, 6, 5, 8)

for(i in 1:length(plots)){

  ggsave(plot = plots[[i]], filename = gsub('data','figures',gsub('.csv', paste0('_', paste(group_vars_list[[i]],collapse = '_'), '.png'),.args[3])),
         width = widths[i], height = heights[i])
  
}

## save dummy data table
write_csv(data.table(x=0), .args[3])
