
## PLOT MEAN CONTACTS BY IMD, AGE GROUP, GENDER ##

#### load packages ####
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(MASS, warn.conflicts = FALSE)
library(ggplot2)
library(purrr)
library(viridis)
library(patchwork, warn.conflicts = FALSE)

#### set arguments ####
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","participants.rds"),
  "nhs_ages",
  file.path("output", "data", "cont_matrs","nhs_ages","mean_contacts","mean_contacts.csv")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))
source(here::here('scripts','setup','colors.R'))

sens_analysis <- .args[2]

colors <- if(sens_analysis == 'nhs_ages'){
  colors_p_age_group_nhs}else{
    colors_p_age_group
  }

#### make output folders ####

output_folder <- paste0("output/data/cont_matrs/",sens_analysis,"/mean_contacts")
if(!file.exists(output_folder)){dir.create(output_folder)}
output_folder_figs <- gsub('data','figures',output_folder)
if(!file.exists(output_folder_figs)){dir.create(output_folder_figs)}

#### participant data ####

part <- readRDS(.args[1])

part_reconnect <- readRDS(file.path("data","reconnect","reconnect_part.rds"))

# attach gender, day of week
part <- part %>% left_join(part_reconnect %>% 
                             select(p_id, p_gender, day_week, p_income, p_engreg, p_broad_age),
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
eti50L <- function(x) quantile(x, 0.25)
eti50U <- function(x) quantile(x, 0.75)

#### combination of variables ####

group_vars_list <- list(c('imd_quintile'),
                        c('imd_quintile','p_age_group'),
                        c('imd_quintile','p_gender'),
                        c('imd_quintile','p_age_group','p_gender'),
                        c('imd_quintile','p_engreg'),
                        c('imd_quintile','p_income','p_age_group'),
                        c('imd_quintile','p_income','p_broad_age'),
                        c('imd_quintile','p_income','p_broad_age','p_engreg')
                        # c('imd_quintile','p_engreg','p_age_group')
                        )

#### FUNCTIONS ####
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
                   out_agg_raw[, lapply(.SD, eti95U), by = group_vars][, measure := 'upper'],
                   out_agg_raw[, lapply(.SD, eti50L), by = group_vars][, measure := 'lower50'],
                   out_agg_raw[, lapply(.SD, eti50U), by = group_vars][, measure := 'upper50'])
  
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
                    width = 0.4, position = position_dodge(width = 0.9), alpha = 1) + 
      geom_errorbar(aes(ymin = lower50, ymax = upper50, x = p_age_group,
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
           y = 'Mean contacts', x = 'Age group',
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
  if(group_index == 6){
    
    plot_df <- plot_df %>% filter(!grepl('Applic', p_income))
    
    plot_df$p_income <- factor(plot_df$p_income,
                               levels = c("Less than £20,000","£20,000 - £39,999",
                                          "£40,000 - £59,999","£60,000 - £100,000",
                                          "Over £100,000"))
  
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = as.factor(imd_quintile),
                        group = p_income, 
                        col = p_income), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = as.factor(imd_quintile), y = mean,
                     group = p_income, 
                     col = p_income),
                 position = position_dodge(width = 0.9)) +
      # scale_color_manual(values = imd_quintile_colors) + 
      facet_wrap(p_age_group ~ ., scales = 'free') + 
      theme_bw() + 
      ylim(c(0, NA)) + 
      labs(col = 'Household income', fill = 'Household income',
           y = 'Mean contacts', x = ''); p
  }
  if(group_index == 7){
    
    plot_df <- plot_df %>% filter(p_broad_age=='Adult')
    
    plot_df$p_income <- factor(plot_df$p_income,
                               levels = c("Less than £20,000","£20,000 - £39,999",
                                          "£40,000 - £59,999","£60,000 - £100,000",
                                          "Over £100,000"))
    
    income_cols <- c('#95190C','#D56AA0','#E3B505','#36C9C6','#044B7F')
    names(income_cols) <- c("Less than £20,000","£20,000 - £39,999",
                            "£40,000 - £59,999","£60,000 - £100,000",
                            "Over £100,000")
    
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = as.factor(imd_quintile),
                        group = p_income, 
                        col = p_income), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = as.factor(imd_quintile), y = mean,
                     group = p_income, 
                     col = p_income),
                 position = position_dodge(width = 0.9)) +
      scale_color_manual(values = income_cols) + 
      theme_bw() + 
      ylim(c(0, NA)) + 
      labs(col = 'Household income', fill = 'Household income',
           y = 'Mean contacts', x = 'IMD quintile'); p
    
  }
  if(group_index == 8){
    
    plot_df <- plot_df %>% filter(p_broad_age=='Adult')
    
    plot_df$p_income <- factor(plot_df$p_income,
                               levels = c("Less than £20,000","£20,000 - £39,999",
                                          "£40,000 - £59,999","£60,000 - £100,000",
                                          "Over £100,000"))
    
    income_cols <- c('#95190C','#D56AA0','#E3B505','#36C9C6','#044B7F')
    names(income_cols) <- c("Less than £20,000","£20,000 - £39,999",
                            "£40,000 - £59,999","£60,000 - £100,000",
                            "Over £100,000")
    
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = as.factor(imd_quintile),
                        group = p_income, 
                        col = p_income), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = as.factor(imd_quintile), y = mean,
                     group = p_income, 
                     col = p_income),
                 position = position_dodge(width = 0.9)) +
      scale_color_manual(values = income_cols) + 
      theme_bw() + 
      facet_wrap(p_engreg ~ ., scales = 'free') + 
      ylim(c(0, NA)) + 
      labs(col = 'Household income', fill = 'Household income',
           y = 'Mean contacts', x = 'IMD quintile'); p
    
  }
  if(group_index == 9){
    
    p <- plot_df %>% 
      ggplot() + 
      geom_errorbar(aes(ymin = lower, ymax = upper, x = p_engreg,
                        group = as.factor(imd_quintile), 
                        col = as.factor(imd_quintile)), 
                    width = 0.4, position = position_dodge(width = 0.9)) + 
      geom_point(aes(x = p_engreg, y = mean,
                     group = as.factor(imd_quintile), 
                     col = as.factor(imd_quintile)),
                 position = position_dodge(width = 0.9)) +
      scale_color_manual(values = imd_quintile_colors) + 
      theme_bw() + 
      facet_wrap(p_age_group ~ ., scales = 'free') + 
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
      ylim(c(0, NA)) + 
      labs(col = 'IMD quintile', fill = 'IMD quintile',
           y = 'Mean contacts', x = 'Region'); p
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
  
#### fit data, or read in ####
read <- rep(T, length(group_vars_list))
# read[length(read)] <- F

data_list <- map(
  .x = 1:length(read), 
  .f = read_or_calculate
  )

names(data_list) <- unlist(lapply(group_vars_list, function(x) paste(x,collapse = '_')))

#### save if needed ####
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

#### plot ####

plots <- map(
  .x = 1:length(data_list),
  .f = ~{plot_mean_contacts(data_list[[.x]], .x)}
)

widths <- c(6, 12, 7, 14, 10, 10, 10, 10, 20)
heights <- c(4, 6, 5, 8, 6, 6, 6, 6, 13)
if(length(widths)!=length(plots)){
  widths[(length(widths)+1):length(plots)] <- widths[length(widths)]
  }
if(length(heights)!=length(plots)){
  heights[(length(heights)+1):length(plots)] <- heights[length(heights)]}

for(i in 1:length(plots)){

  ggsave(plot = plots[[i]], filename = gsub('data','figures',gsub('.csv', paste0('_', paste(group_vars_list[[i]],collapse = '_'), '.png'),.args[3])),
         width = widths[i], height = heights[i])
  
}

## tile plot of household income vs IMD quintile

group_vars <- group_vars_list[[7]]
dt <- data.table(data_list[[7]])
dt[, bootstrap_index := NULL]
out_agg <- rbind(dt[, lapply(.SD, mean), by = group_vars][, measure := 'mean'],
                 dt[, lapply(.SD, eti95L), by = group_vars][, measure := 'lower'],
                 dt[, lapply(.SD, eti95U), by = group_vars][, measure := 'upper'],
                 dt[, lapply(.SD, eti50L), by = group_vars][, measure := 'lower50'],
                 dt[, lapply(.SD, eti50U), by = group_vars][, measure := 'upper50'])

plot_df <- out_agg %>% 
  select(!k) %>% 
  pivot_wider(names_from = measure, values_from = n) %>% 
  filter(p_broad_age=='Adult')

plot_df$p_income <- factor(plot_df$p_income,
                           levels = c("Less than £20,000","£20,000 - £39,999",
                                      "£40,000 - £59,999","£60,000 - £100,000",
                                      "Over £100,000"))

p <- plot_df %>% 
  ggplot() + 
  geom_tile(aes(x = as.factor(imd_quintile), y = p_income,
                fill = mean)) +
  geom_text(aes(x = as.factor(imd_quintile), y = p_income,
                label = paste0(round(mean,2), '\n(', 
                               round(lower,2), ' - ',
                               round(upper,2), ')'),
                col = (mean > 11.5))) +
  theme_bw() + guides(col="none") +
  scale_color_manual(values = c('white','black')) + 
  scale_fill_viridis(option = 'A', limits = c(5, 14)) +
  theme(text = element_text(size = 14)) + 
  labs(fill = 'Mean contacts',
       y = 'Household income', x = 'IMD quintile'); p

ggsave(filename = gsub('data','figures',gsub('.csv', paste0('_tile_', paste(group_vars,collapse = '_'), '.png'),.args[3])),
       width = 10, height = 8)

p <- plot_df %>% 
  ggplot() + 
  geom_errorbar(aes(ymin = lower, ymax = upper, group = as.factor(imd_quintile),
                    x = p_income, col = as.factor(imd_quintile)), 
                width = 0.4, position = position_dodge(width = 0.9), alpha = 1) + 
  geom_errorbar(aes(ymin = lower50, ymax = upper50, group = as.factor(imd_quintile),
                    x = p_income, col = as.factor(imd_quintile)),
                width = 0.4, position = position_dodge(width = 0.9)) +
  geom_point(aes(group = as.factor(imd_quintile), y = mean,
                 x = p_income, col = as.factor(imd_quintile)),
             position = position_dodge(width = 0.9)) +
  scale_color_manual(values = imd_quintile_colors) + 
  theme_bw() + 
  theme(axis.text.x=element_text(angle = 30, hjust = 1)) + 
  ylim(c(0, NA)) + 
  labs(col = 'IMD quintile', fill = 'IMD quintile',
       y = 'Mean contacts', x = 'Household income'); p

ggsave(filename = gsub('data','figures',gsub('.csv', paste0('_switch_', paste(group_vars,collapse = '_'), '.png'),.args[3])),
       width = 10, height = 8)

## saving final figure for paper, with proportions of participants in each IMD too

# weighting by 5-year age group
age_limits <- seq(5,75,5)
age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))

age_structure <- read_csv(file.path('data','imd_25','imd_ages_1.csv'), show_col_types=F)
demog_age <- age_structure %>% group_by(age_grp) %>% 
  summarise(pop = sum(pop)) %>% ungroup() %>% 
  mutate(prop = pop/sum(pop)) %>% rename(p_age_group = age_grp)

summ_part <- part %>% 
  mutate(p_age_group = cut(p_age,breaks = c(0,age_limits,Inf),
                           labels = age_labels,right = F)) %>% 
  group_by(p_age_group, bootstrap_index, imd_quintile) %>% 
  count() %>% group_by(p_age_group, bootstrap_index) %>% 
  mutate(p = prop.table(n)) %>% 
  left_join(demog_age, by = 'p_age_group') %>% 
  group_by(imd_quintile, bootstrap_index) %>% 
  summarise(p = weighted.mean(x=p, w=prop)) %>% 
  group_by(imd_quintile) %>% 
  summarise(m = mean(p), l = quantile(p, 0.025), u = quantile(p, 0.975)) %>% 
  mutate(neat = paste0(round(100*m,2), ' (', round(100*l,2), ' - ', round(100*u,2), ')'))

write_csv(summ_part, file.path('output','data','cont_matrs','weighted_imd_proportions.csv'))

imd_props <- summ_part %>% 
  ggplot() + 
  geom_errorbar(aes(x=imd_quintile, ymin=l, ymax=u, col=as.factor(imd_quintile)),
                width = 0.4) +
  geom_point(aes(x=imd_quintile, y=m, col=as.factor(imd_quintile)),
                 size = 2) + 
  scale_y_continuous(limits = c(0.15, 0.25), breaks = c(0.15,0.2,0.25)) + 
  theme_bw() + scale_color_manual(values = imd_quintile_colors) +
  labs(x = 'IMD quintile', y = 'Proportion of participants') +
  theme(legend.position='none'); imd_props

# layout <- "
# ABBB
# CCCC
# "
# 
# imd_props + p + theme(legend.position = 'none') + 
#   plots[[2]] + theme(legend.position = "inside",
#                                    legend.position.inside=c(.9,.75)) +
#   plot_layout(design = layout) + 
#   plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')

layout <- "
AAAAAABBBBBBBBBBBBBB
AAAAAABBBBBBBBBBBBBB
CCCCCCCCCCCCCCCCCCCD
CCCCCCCCCCCCCCCCCCCD
"

imd_props + p + theme(legend.position = 'none') + 
  plots[[2]] + guide_area() + theme(legend.position = "inside",
                     legend.position.inside=c(.9,.75)) +
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')

ggsave(filename = gsub('data','figures',gsub('.csv', paste0('_patch.png'),.args[3])),
       width = 10, height = 10)

  
## and region 
  
group_vars <- group_vars_list[[8]]
dt <- data.table(data_list[[8]])
dt[, bootstrap_index := NULL]
out_agg <- rbind(dt[, lapply(.SD, mean), by = group_vars][, measure := 'mean'],
                 dt[, lapply(.SD, eti95L), by = group_vars][, measure := 'lower'],
                 dt[, lapply(.SD, eti95U), by = group_vars][, measure := 'upper'])

plot_df <- out_agg %>% 
  select(!k) %>% 
  pivot_wider(names_from = measure, values_from = n) %>% 
  filter(p_broad_age=='Adult')

plot_df$p_income <- factor(plot_df$p_income,
                           levels = c("Less than £20,000","£20,000 - £39,999",
                                      "£40,000 - £59,999","£60,000 - £100,000",
                                      "Over £100,000"))

p <- plot_df %>% 
  ggplot() + 
  geom_errorbar(aes(ymin = lower, ymax = upper, group = as.factor(imd_quintile),
                    x = p_income, col = as.factor(imd_quintile)), 
                width = 0.4, position = position_dodge(width = 0.9)) + 
  geom_point(aes(group = as.factor(imd_quintile), y = mean,
                 x = p_income, col = as.factor(imd_quintile)),
             position = position_dodge(width = 0.9)) +
  scale_color_manual(values = imd_quintile_colors) + 
  theme_bw() + facet_wrap(p_engreg ~ ., scale = 'free') + 
  ylim(c(0, NA)) + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1)) + 
  labs(col = 'IMD quintile', fill = 'IMD quintile',
       y = 'Mean contacts', x = 'Household income'); p

ggsave(filename = gsub('data','figures',gsub('.csv', paste0('_switch_', paste(group_vars,collapse = '_'), '.png'),.args[3])),
       width = 14, height = 9)

#### save dummy data ####
write_csv(data.table(x=0), .args[3])






