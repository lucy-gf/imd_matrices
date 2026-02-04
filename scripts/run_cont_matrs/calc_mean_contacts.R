
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
library(purrr)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","participants.rds"),
  "base",
  file.path("output", "data", "cont_matrs","base","mean_contacts","mean_contacts.csv")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))
source(here::here('scripts','setup','colors.R'))

sens_analysis <- .args[2]

colors <- if(sens_analysis == 'nhs_ages'){
  colors_p_age_group_nhs}else{
    colors_p_age_group
  }

## make output folders

output_folder <- paste0("output/data/cont_matrs/",sens_analysis,"/mean_contacts")
if(!file.exists(output_folder)){dir.create(output_folder)}
output_folder_figs <- gsub('data','figures',output_folder)
if(!file.exists(output_folder_figs)){dir.create(output_folder_figs)}

## read in participant data

part <- readRDS(.args[1])

part_reconnect <- readRDS(file.path("data","reconnect","reconnect_part.rds"))

# attach gender, day of week
part <- part %>% left_join(part_reconnect %>% 
                             select(p_id, p_gender, day_week, p_income, p_engreg),
                           by = 'p_id') %>% 
  mutate(total_contacts = n_contacts + large_n)

## if NHS age groups, change age groups
if(sens_analysis == 'nhs_ages'){
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
  
  part <- part %>% 
    mutate(p_age_group = cut(p_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F))
  
}

# confidence interval functions
eti95L <- function(x) quantile(x, 0.025)
eti95U <- function(x) quantile(x, 0.975)

# combination of variables

group_vars_list <- list(c('imd_quintile'),
                        c('imd_quintile','p_age_group'),
                        c('imd_quintile','p_gender'),
                        c('imd_quintile','p_age_group','p_gender'),
                        c('imd_quintile','p_engreg'))

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
  
  out_agg_raw <- copy(out)
  out_agg_raw[, total_contacts := NULL]
  
  if('p_age_group' %in% group_vars){
    out_agg_raw$p_age_group <- factor(out_agg_raw$p_age_group,
                                  levels = age_labels)
    out_agg_raw <- out_agg_raw %>% arrange(p_age_group)
  }
  
  out_agg_raw 
  
}

## function to read in data

read_dat <- function(i){
  data.table(read_csv(gsub('.csv', paste0('_', paste(group_vars_list[[i]],collapse = '_'), '.csv'),.args[3]),
                      show_col_types = F))
}

## function to plot

plot_mean_contacts <- function(out_agg_raw, # dataframe
                               group_index){
  
  group_vars <- group_vars_list[[group_index]]
  
  out_agg_raw <- data.table(out_agg_raw)
  out_agg_raw[, bootstrap_index := NULL]
  out_agg <- rbind(out_agg_raw[, lapply(.SD, mean), by = group_vars][, measure := 'mean'],
                   out_agg_raw[, lapply(.SD, eti95L), by = group_vars][, measure := 'lower'],
                   out_agg_raw[, lapply(.SD, eti95U), by = group_vars][, measure := 'upper'])
  
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
  if(group_index == 5){
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = p_engreg,
                        group = interaction(as.factor(imd_quintile), p_engreg), 
                        col = as.factor(imd_quintile)), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = p_engreg, y = mean,
                     group = interaction(as.factor(imd_quintile), p_engreg), 
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

## function to plot

plot_bootstrap_mean_contacts <- function(out_agg_raw, # dataframe
                                         group_index){
  
  out_agg <- copy(out_agg_raw)
  
  group_vars <- group_vars_list[[group_index]]
  
  if('p_age_group' %in% group_vars_list[[group_index]]){
    out_agg$p_age_group <- factor(out_agg$p_age_group,
                                  levels = age_labels)
    out_agg <- out_agg %>% arrange(p_age_group)
  }
  
  plot_df <- out_agg %>% 
    select(!k) 
  
  if(group_index == 1){
    p <- plot_df %>% 
      ggplot() + 
      geom_line(aes(x = imd_quintile, y = n, 
                    group = bootstrap_index),
                alpha = 0.02) +
      theme_bw() + 
      ylim(c(0, NA)) + 
      labs(x = 'IMD quintile', y = 'Mean contacts',
           linetype = 'Gender') +
      theme(legend.position = 'none')
  }
  if(group_index == 2){
    p <- plot_df %>% 
      ggplot() + 
      geom_line(aes(x = imd_quintile, y = n, 
                    color = p_age_group, group = bootstrap_index),
                alpha = 0.02) +
      theme_bw() + 
      facet_wrap(. ~ p_age_group, scales = 'free') +
      ylim(c(0, NA)) + 
      scale_color_manual(values = colors) + 
      labs(x = 'IMD quintile', y = 'Mean contacts',
           linetype = 'Gender') +
      theme(legend.position = 'none')
  }
  if(group_index == 3){
    p <- plot_df %>% 
      ggplot() + 
      geom_line(aes(x = imd_quintile, y = n, 
                    color = p_gender, group = bootstrap_index),
                alpha = 0.02) +
      theme_bw() + 
      facet_wrap(. ~ p_gender, scales = 'free') +
      ylim(c(0, NA)) + 
      scale_color_manual(values = gender_colors) + 
      labs(x = 'IMD quintile', y = 'Mean contacts',
           linetype = 'Gender') +
      theme(legend.position = 'none')
  }
  if(group_index == 4){
    p <- plot_df %>% 
      ggplot() + 
      geom_line(aes(x = imd_quintile, y = n, 
                    color = p_gender, group = interaction(bootstrap_index,p_gender)),
                alpha = 0.02) +
      theme_bw() + 
      facet_wrap(. ~ p_age_group, scales = 'free') +
      ylim(c(0, NA)) + guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      scale_color_manual(values = gender_colors) + 
      labs(x = 'IMD quintile', y = 'Mean contacts',
           linetype = 'Gender', color = 'Gender') 
  }
  if(group_index == 5){
    p <- plot_df %>% 
      ggplot() + 
      geom_line(aes(x = imd_quintile, y = n, 
                    color = p_engreg, group = bootstrap_index),
                alpha = 0.02) +
      theme_bw() + 
      facet_wrap(. ~ p_engreg, scales = 'free') +
      ylim(c(0, NA)) + 
      scale_color_manual(values = colors_p_engreg) + 
      labs(x = 'IMD quintile', y = 'Mean contacts',
           linetype = 'Gender') +
      theme(legend.position = 'none')
  }
  
  p
  
}

read_or_calculate <- function(x){
  
  if(read[x]){
    dt <- read_dat(x)
  }else(
    dt <- fit_mean_contacts(x)
  )
  
  return(data.table(dt))
  
}
  
## fit data, or read in 
read <- rep(T, length(group_vars_list))

data_list <- map(
  .x = 1:length(read), 
  .f = read_or_calculate
  )

names(data_list) <- unlist(lapply(group_vars_list, function(x) paste(x,collapse = '_')))

## save any that were run
for(i in 1:length(read)){
  if(!read[i]){
    write_csv(data.frame(data_list[[i]]), gsub('.csv', paste0('_', paste(group_vars_list[[i]],collapse = '_'), '.csv'),.args[3]))
  }
}

## save mean CI width by age group, across IMD

out_agg_raw <- data.table(data_list[[2]])[, bootstrap_index := NULL]
group_vars <- group_vars_list[[2]]
out_agg <- rbind(out_agg_raw[, lapply(.SD, mean), by = group_vars][, measure := 'mean'],
                 out_agg_raw[, lapply(.SD, eti95L), by = group_vars][, measure := 'lower'],
                 out_agg_raw[, lapply(.SD, eti95U), by = group_vars][, measure := 'upper'])
out_agg$p_age_group <- factor(out_agg$p_age_group,
                                levels = age_labels)
out_agg <- out_agg %>% arrange(p_age_group)
plot_df <- out_agg %>% 
  select(!k) %>% 
  pivot_wider(names_from = measure, values_from = n) 
write_csv(plot_df %>% mutate(width = upper - lower) %>% group_by(p_age_group) %>% summarise(mean_width = mean(width)),
          gsub('.csv', '_ci_width.csv',.args[3]))

## plot

plots <- map(
  .x = 1:length(data_list),
  .f = ~{plot_mean_contacts(data_list[[.x]], .x)}
)

widths <- c(6, 12, 7, 14, 10)
heights <- c(4, 6, 5, 8, 6)

for(i in 1:length(plots)){

  ggsave(plot = plots[[i]], filename = gsub('data','figures',gsub('.csv', paste0('_', paste(group_vars_list[[i]],collapse = '_'), '.png'),.args[3])),
         width = widths[i], height = heights[i])
  
}

plots <- map(
  .x = 1:length(data_list),
  .f = ~{plot_bootstrap_mean_contacts(data_list[[.x]], .x)}
)

widths <- c(6, 10, 8, 10)
heights <- c(5, 8, 5, 8)

for(i in 1:length(plots)){
  
  ggsave(plot = plots[[i]], filename = gsub('data','figures',gsub('.csv', paste0('_indiv_', paste(group_vars_list[[i]],collapse = '_'), '.png'),.args[3])),
         width = widths[i], height = heights[i])
  
}

## save dummy data table
write_csv(data.table(x=0), .args[3])
