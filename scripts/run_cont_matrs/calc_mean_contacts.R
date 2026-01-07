
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

## set grouping vars (eventually loop over these)

for(group_index in 1:4){
  
  if(group_index == 1){group_vars <- c('imd_quintile')}
  if(group_index == 2){group_vars <- c('imd_quintile','p_age_group')}
  if(group_index == 3){group_vars <- c('imd_quintile','p_gender')}
  if(group_index == 4){group_vars <- c('imd_quintile','p_age_group','p_gender')}
  
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
  }
  
  ## plot
  
  plot_df <- out_agg %>% 
    select(!k) %>% 
    pivot_wider(names_from = measure, values_from = n) 
  
  if('p_gender' %in% group_vars){
    plot_df <- plot_df %>% 
      mutate(yval = paste0(imd_quintile, '_', p_gender))
  }else{
    plot_df <- plot_df %>% 
      mutate(yval = imd_quintile)
  }
  
  p <- plot_df %>% 
    ggplot(aes(y = yval, col = as.factor(imd_quintile))) + 
    geom_point(aes(x = mean)) +
    scale_color_manual(values = imd_quintile_colors) + 
    scale_linetype_manual(values = c(1,2)) + 
    theme_bw() + 
    xlim(c(0, NA)) + 
    labs(col = 'IMD quintile', fill = 'IMD quintile',
         x = 'Mean contacts', y = '',
         linetype = 'Gender')
  
  if('p_age_group' %in% group_vars){
    p <- p + facet_wrap(p_age_group ~ ., scales = 'free') 
  }
  
  if('p_gender' %in% group_vars){
    p <- p + geom_errorbar(aes(xmin = lower, xmax = upper, lty = p_gender), 
                           width = 0.4)
  }else{
    p <- p + geom_errorbar(aes(xmin = lower, xmax = upper), 
                           width = 0.4)
  }
  
  p
  ggsave(gsub('data','figures',gsub('.csv', paste0('_', paste(group_vars,collapse = '_'), '.png'),.args[3])),
         width = 4*length(group_vars), height = 3*length(group_vars))
  
  ## save 
  write_csv(out_agg, gsub('.csv', paste0('_', paste(group_vars,collapse = '_'), '.csv'),.args[3]))

}

write_csv(data.table(x=0), .args[3])
