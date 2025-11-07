
## PLOT CONTACT MATRICES ##

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
  file.path("output", "data", "cont_matrs","base","fitted_matrs_balanced.csv"),
  file.path("data", "census","imd_age.csv"),
  "base",
  file.path("output", "figures", "cont_matrs","base","fitted_matrs.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

if(!file.exists(gsub('/fitted_matrs.png','',.args[4]))){dir.create(gsub('/fitted_matrs.png','',.args[4]))}

#### READ IN DATA ####

balanced_matr <- data.table(read_csv(.args[1], show_col_types = F))

sens_analysis <- .args[3]

## age distribution 

if(sens_analysis == 'regional'){
  
  imd_age_raw <- data.table(read_csv(file.path("data","imd_25","imd_ages_1.csv"), show_col_types = F))
  
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
  imd_age <- data.table(read_csv(file.path("data","imd_25","imd_ages_1.csv"), show_col_types = F))
  
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
}

#### SUMMARISE ####

group_vars <- c('p_age_group', 'c_age_group', 'p_imd_q', 'c_imd_q')
if(sens_analysis == 'regional'){group_vars <- c(group_vars, 'p_engreg')}

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

#### PLOT ####

#### IMD x IMD ####

## balanced

join_vars_balanced <- c('p_imd_q', 'p_age_group')
if(sens_analysis == 'regional'){join_vars_balanced <- c(join_vars_balanced, 'p_engreg')}
group_vars_balanced <- c('bootstrap_index', 'p_imd_q', 'c_imd_q')
if(sens_analysis == 'regional'){group_vars_balanced <- c(group_vars_balanced, 'p_engreg')}
group_vars_balanced_no_bs <- group_vars_balanced[!grepl('bootstrap', group_vars_balanced)]

imd_mix <- balanced_matr %>% 
  left_join(imd_age %>% rename(p_imd_q = imd_q, p_age_group = age),
            by = join_vars_balanced) %>% 
  group_by(!!!syms(group_vars_balanced)) %>% 
  summarise(weighted_sum = sum(n*prop_imd)) %>% 
  group_by(!!!syms(group_vars_balanced_no_bs)) %>% 
  summarise(weighted_mean = mean(weighted_sum))

imd_mix_distr <- balanced_matr %>% 
  left_join(imd_age %>% rename(p_imd_q = imd_q, p_age_group = age),
            by = join_vars_balanced) %>% 
  group_by(!!!syms(group_vars_balanced)) %>% 
  summarise(weighted_sum = sum(n*prop_imd)) %>% 
  group_by(!!!syms(group_vars_balanced_no_bs)) %>% 
  mutate(weighted_mean = mean(weighted_sum))

imd_mix_plot <- imd_mix %>% 
  ggplot() + 
  geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = weighted_mean)) +
  geom_text(aes(x = p_imd_q, y = c_imd_q, label = format_number(weighted_mean), 
                col = (weighted_mean > 2.5)), size = 6) +
  theme_bw() + 
  scale_fill_viridis(option = 'A', limits = c(0,NA)) +
  scale_color_manual(values = c('white', 'black'), guide = 'none') +
  labs(
    x = 'Participant IMD quintile',
    y = 'Contact IMD quintile',
    fill = 'Mean daily\ncontacts'
  ) + 
  theme(#strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 18))

imd_mix_distr$c_imd_q <- factor(imd_mix_distr$c_imd_q, 
                                levels = rev(1:5))
imd_mix_distr_plot <- imd_mix_distr %>% 
  ggplot() + 
  geom_density(aes(x = weighted_sum, fill = weighted_mean)) +
  theme_bw() + 
  facet_grid(c_imd_q ~ p_imd_q, switch = 'both') + 
  scale_fill_viridis(option = 'A', limits = c(0,NA)) +
  labs(
    x = 'Participant IMD quintile',
    y = 'Contact IMD quintile',
    fill = 'Mean daily\ncontacts'
  ) + 
  theme(strip.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 14)); imd_mix_distr_plot
ggsave(gsub('.png','_imd_mix_distr.png',.args[4]), width = 11, height = 10)
    
if(sens_analysis == 'regional'){
  imd_mix_plot <- imd_mix_plot + facet_wrap(. ~ p_engreg)
  
  imd_mix_plot_pc <- imd_mix %>% 
    group_by(p_engreg, p_imd_q) %>% mutate(tot_p_imd_q = sum(weighted_mean)) %>% 
    left_join(imd_age %>% group_by(p_engreg, imd_q) %>% summarise(prop = sum(prop)) %>% rename(c_imd_q = imd_q),
              by = c('p_engreg','c_imd_q')) %>% 
    mutate(pc_val = round(100*((weighted_mean/tot_p_imd_q)/prop - 1))) %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = pc_val)) +
    geom_text(aes(x = p_imd_q, y = c_imd_q, label = paste0(pc_val, '%'), 
                  col = (pc_val > 2.5))) +
    theme_bw() + 
    facet_wrap(. ~ p_engreg) + 
    scale_fill_viridis(option = 'D', 
                       breaks = c(-30, 0, 30, 300),
                       labels = paste0(c(-30, 0, 30, 300),'%'),
                       transform = 'pseudo_log'
                       ) +
    scale_color_manual(values = c('white', 'black'), guide = 'none') +
    labs(
      x = 'Participant IMD quintile',
      y = 'Contact IMD quintile',
      fill = 'Percentage difference\nin contacts\ncompared to\nrandom mixing'
    ) + 
    theme(#strip.background = element_blank(),
      strip.placement = "outside",
      text = element_text(size = 14)); imd_mix_plot_pc
  
  ggsave(gsub('.png','_imd_mix_pc.png',.args[4]), width = 11, height = 10)
  
  imd_mix_plot + imd_mix_plot_pc + plot_layout(nrow=1) + 
    plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')
  ggsave(gsub('.png','_imd_mix_patch.png',.args[4]), width = 22, height = 10)
  
}

imd_mix_plot

ggsave(gsub('.png','_imd_mix.png',.args[4]), width = ifelse(sens_analysis == 'regional', 11, 12), height = 10)

if(sens_analysis %notin% c('balance_sett_spec', 'regional')){
  
  fitted <- suppressWarnings(data.table(read_csv(gsub('_balanced.csv','.csv',.args[1]), show_col_types = F)))
  fitted <- fitted[bootstrap_index != 'bootstrap_index']
  
  ## unbalanced 
  
  imd_mix_unb <- fitted %>% 
    filter(c_location == 'total') %>% 
    group_by(bootstrap_index, p_age_group, p_imd_q, c_age_group, c_imd_q) %>% 
    summarise(n = sum(n)) %>% ungroup() %>% 
    left_join(imd_age %>% rename(p_imd_q = imd_q, p_age_group = age),
              by = c('p_imd_q','p_age_group')) %>% 
    group_by(bootstrap_index, p_imd_q, c_imd_q) %>% 
    summarise(weighted_sum = sum(n*prop_imd)) %>% 
    group_by(p_imd_q, c_imd_q) %>% 
    summarise(weighted_mean = mean(weighted_sum))
  
  imdm <- imd_mix_unb %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = weighted_mean)) +
    geom_text(aes(x = p_imd_q, y = c_imd_q, label = format_number(weighted_mean), 
                  col = (weighted_mean > 2.5)), size = 6) +
    theme_bw() + 
    scale_fill_viridis(option = 'A', limits = c(0,NA)) +
    scale_color_manual(values = c('white', 'black'), guide = 'none') +
    labs(
      x = 'Participant IMD quintile',
      y = 'Contact IMD quintile',
      fill = 'Mean daily\ncontacts'
    ) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 18)); imdm
  
  ggsave(gsub('.png','_imd_mix_unbalanced.png',.args[4]), width = 12, height = 10)
  
  ## balanced without home
  
  balanced_matr_no_home <- balancing_fcn(
    data = fitted %>% filter(c_location %notin% c('home','total')),
    age_structure = imd_age,
    use_total = F
  ) 
  
  imd_mix_nh <- balanced_matr_no_home %>% 
    left_join(imd_age %>% rename(p_imd_q = imd_q, p_age_group = age),
              by = c('p_imd_q','p_age_group')) %>% 
    group_by(bootstrap_index, p_imd_q, c_imd_q) %>% 
    summarise(weighted_sum = sum(n*prop_imd)) %>% 
    group_by(p_imd_q, c_imd_q) %>% 
    summarise(weighted_mean = mean(weighted_sum))
  
  imdm_nh <- imd_mix_nh %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = weighted_mean)) +
    geom_text(aes(x = p_imd_q, y = c_imd_q, label = format_number(weighted_mean), 
                  col = (weighted_mean > 1.7)), size = 6) +
    theme_bw() + 
    scale_fill_viridis(option = 'A', limits = c(0,NA)) +
    scale_color_manual(values = c('white', 'black'), guide = 'none') +
    labs(
      x = 'Participant IMD quintile',
      y = 'Contact IMD quintile',
      fill = 'Mean daily\ncontacts'
    ) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 18)); imdm_nh 
  
  ggsave(gsub('.png','_imd_mix_no_home.png',.args[4]), width = 12, height = 10)
  
  imd_mix_plot + imdm_nh + plot_layout(nrow = 1) + 
    plot_annotation(tag_levels = 'a',
                    tag_prefix = '(',
                    tag_suffix = ')')
  ggsave(gsub('.png','_imd_mix_both.png',.args[4]), width = 22, height = 10)
  
}

if(sens_analysis != 'regional'){
  
  #### VARIATION ####
  
  agg %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = width)) +
    theme_bw() + 
    facet_grid(c_imd_q ~ p_imd_q, switch="both") +
    scale_fill_viridis(option = 'A') +
    labs(
      x = 'Participant IMD, age group',
      y = 'Contact IMD, age group',
      fill = 'Width of\n95% CI'
    ) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
  #### SAVE PNG ####
  
  ggsave(gsub('.png','_var.png',.args[4]), width = 16, height = 14)
  
  #### IMD DIFFS ####
  
  gen_pop <- balanced_matr %>% 
    group_by(p_age_group, c_age_group) %>% 
    summarise(med_n_gp = mean(n)) 
  
  diff <- agg %>% 
    left_join(gen_pop, by = c('p_age_group','c_age_group')) %>% 
    mutate(diff = med_n - med_n_gp)
  
  diff$p_age_group <- factor(diff$p_age_group,
                             levels = age_labels)
  diff$c_age_group <- factor(diff$c_age_group,
                             levels = age_labels)
  diff$c_imd_q <- factor(diff$c_imd_q,
                         levels = rev(as.character(1:5)))
  
  colorscale <- c('#045a8d', '#bdc9e1','#f1eef6','white','#fef0d9','#fdcc8a','#b30000')
  breaks <- sort(c(0, quantile(diff$diff, c(0.75, 0.9, 0.99)), 
                   -quantile(diff$diff, c(0.75, 0.9, 0.99))))
  max_stat <- max(diff$diff)
  
  diff %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = med_n - med_n_gp)) +
    theme_bw() + 
    facet_grid(c_imd_q ~ p_imd_q, switch="both") +
    scale_fill_gradientn(colors = colorscale, 
                         values = scales::rescale(breaks),
                         breaks = breaks,
                         na.value = "#e5e5e5", 
                         limits = c(- max_stat, max_stat),
                         trans = 'pseudo_log') +
    labs(
      x = 'Participant IMD, age group',
      y = 'Contact IMD, age group',
      fill = 'Diff. from av.'
    ) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position = 'none')
  
  #### SAVE PNG ####
  
  ggsave(gsub('.png','_diff.png',.args[4]), width = 14, height = 14)
  
  # with the age diagonal removed
  cm_limited <- agg %>% 
    mutate(med_n = case_when(p_age_group == c_age_group ~ NA,
                             T ~ med_n)) %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = med_n)) +
    theme_bw() + 
    facet_grid(c_imd_q ~ p_imd_q, switch="both") +
    scale_fill_viridis(na.value = "grey10") +
    labs(
      x = 'Participant IMD, age group',
      y = 'Contact IMD, age group',
      fill = 'Mean daily\ncontacts'
    ) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)); cm_limited
  
  ggsave(gsub('.png','_limited.png',.args[4]), width = 16, height = 14)

}

#### MEAN CONTACTS ####

if(sens_analysis != 'regional'){
  
  # age distribution plots
  
  plot_imd_age <- function(i){
    
    imd_age %>% 
      filter(imd_q == i) %>% 
      ggplot() + 
      geom_bar(aes(age, prop, fill = age),
               stat = 'identity', position = 'dodge', alpha = 0.7) +
      theme_void() +
      scale_fill_manual(values = heat.colors(30)[1:16]) +
      ylim(c(0,max(imd_age$prop))) + 
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.position = 'none')
    
  }
  
  plot_imd_age_flip <- function(i){
    plot_imd_age(i) + coord_flip()
  }
  
  imd_ages <- map(
    .x = 1:5,
    .f = plot_imd_age
  )
  imd_ages_flip <- map(
    .x = 1:5,
    .f = plot_imd_age_flip 
  )
  
  # plots
  
  cm <- agg %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = med_n)) +
    theme_bw() + 
    facet_grid(c_imd_q ~ p_imd_q, switch="both") +
    scale_fill_viridis() +
    labs(
      x = 'Participant IMD, age group',
      y = 'Contact IMD, age group',
      fill = 'Mean daily\ncontacts'
    ) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)); cm
  
  layout <- '
AABBCCDDEE#
KKKKKKKKKKJ
KKKKKKKKKKJ
KKKKKKKKKKI
KKKKKKKKKKI
KKKKKKKKKKH
KKKKKKKKKKH
KKKKKKKKKKG
KKKKKKKKKKG
KKKKKKKKKKF
KKKKKKKKKKF
'
  
  patchwork::wrap_plots(c(imd_ages, imd_ages_flip)) + cm + plot_layout(design = layout, guides = 'collect')
  
  #### SAVE PNG ####
  
  ggsave(.args[4], width = 16, height = 14)
  
}else{
  
  regional_cm_plot <- function(reg){
    
    agg_filt <- agg %>% filter(p_engreg == reg)
    imd_age_filt <- imd_age %>% filter(p_engreg == reg)
    
    # age distribution plots
    
    plot_imd_age <- function(i){
      
      imd_age_filt %>% 
        filter(imd_q == i) %>% 
        ggplot() + 
        geom_bar(aes(age, prop, fill = age),
                 stat = 'identity', position = 'dodge', alpha = 0.7) +
        theme_void() +
        scale_fill_manual(values = heat.colors(30)[1:16]) +
        ylim(c(0,max(imd_age_filt$prop))) + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              legend.position = 'none')
      
    }
    
    plot_imd_age_flip <- function(i){
      plot_imd_age(i) + coord_flip()
    }
    
    imd_ages <- map(
      .x = 1:5,
      .f = plot_imd_age
    )
    imd_ages_flip <- map(
      .x = 1:5,
      .f = plot_imd_age_flip 
    )
    
    # plots
    
    cm <- agg_filt %>% 
      ggplot() + 
      geom_tile(aes(x = p_age_group, y = c_age_group, fill = med_n)) +
      theme_bw() + 
      facet_grid(c_imd_q ~ p_imd_q, switch="both") +
      scale_fill_viridis() +
      labs(
        x = paste0(reg, ', Participant IMD, age group'),
        y = 'Contact IMD, age group',
        fill = 'Mean daily\ncontacts'
      ) + 
      theme(strip.background = element_blank(),
            strip.placement = "outside",
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
  
    layout <- '
AABBCCDDEE#
KKKKKKKKKKJ
KKKKKKKKKKJ
KKKKKKKKKKI
KKKKKKKKKKI
KKKKKKKKKKH
KKKKKKKKKKH
KKKKKKKKKKG
KKKKKKKKKKG
KKKKKKKKKKF
KKKKKKKKKKF
'
    
    p <- patchwork::wrap_plots(c(imd_ages, imd_ages_flip)) + cm + plot_layout(design = layout, guides = 'collect') +
      plot_annotation(title = reg)
    
    p
    
  }
  
  regional_cms <- map(
    .x = unique(agg$p_engreg),
    .f = regional_cm_plot
  )
  
  patchwork::wrap_plots(regional_cms)
  
  #### SAVE PNG ####
  
  ggsave(.args[4], width = 28, height = 24)
  
  
}




