# epidemic output figures - attack rates

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
  file.path('output','figures','epidem','base','attack_rate_bars.png')
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
  
  ## Set base levels for IMD and age
  base_imd_arr <- 5
  base_age_arr <- ifelse(sens_analysis != 'nhs_ages', '35-39', '35-49')
  
  age_colors <- if(sens_analysis != 'nhs_ages'){
    colors_p_age_group
  }else{
    colors_p_age_group_nhs
  }
  
  l95_func <- function(x){quantile(x, probs=0.025)}; u95_func <- function(x){quantile(x, probs=0.975)}
  
}

#### DEMOGRAPHY ####

{

  imd_age_raw <- data.table(read_csv(file.path("data","imd_25",
                                               paste0("imd_ages_",age_structure_num,".csv")), 
                                     show_col_types = F))
  
  demog_allreg <- imd_age_raw %>% 
    mutate(p_engreg = case_when(
      grepl('London',p_engreg) ~ 'Greater London',
      grepl('Yorkshire',p_engreg) ~ 'Yorkshire and the Humber',
      T ~ p_engreg
    ),
    IMD = as.character(imd_quintile),
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
  
  demog_allreg <- data.table(demog_allreg)
  
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

barchart_plot <- function(data_in, regional = F){
  
  vec <- c('imd','age')
  if(regional){ vec <- c(vec, 'p_engreg') }
  
  plot_dat <- data_in %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(median = median(attack_rate),
              l95 = l95_func(attack_rate),
              u95 = u95_func(attack_rate)) 
    
  p <- ggplot(plot_dat) + 
    geom_bar(aes(x = age, y = 1000*median, fill = as.factor(imd)),
             stat = 'identity', position = 'dodge') +
    geom_errorbar(aes(x = age, ymin = 1000*l95, ymax = 1000*u95, 
                      group = as.factor(imd)), 
                  width = 0.4, position = position_dodge(width = 0.9), alpha= 0.75) +
    theme_bw() +
    scale_fill_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Attack rate per 1000 population", x = "Age group", color = "IMD quintile", fill = 'IMD quintile')

  if(regional){
    
    p <- p +  facet_wrap(.~ p_engreg, scales = 'free') 
    
  }
  
  p
  
}

age_spec_infections <- function(data_in){
  
  p1 <- data_in %>% 
    group_by(age, imd) %>% 
    summarise(med_ar = median(attack_rate),
              l_ar = l95_func(attack_rate),
              u_ar = u95_func(attack_rate)) %>% 
    ggplot() +
    geom_ribbon(aes(x = age, ymin = 1000*l_ar, ymax = 1000*u_ar,
                    fill = as.factor(imd), group = as.factor(imd)),
                alpha = 0.25) +
    geom_line(aes(x = age, y = 1000*med_ar, col = as.factor(imd), group = as.factor(imd)),
                lwd = 0.8) +
    ylim(c(0,NA)) + 
    scale_color_manual(values = imd_quintile_colors) +
    scale_fill_manual(values = imd_quintile_colors) + 
    theme_bw() + 
    labs(x = 'Age group', y = 'Attack rate per 1000 population',
         col = 'IMD quintile', fill = 'IMD quintile') +
    theme(text=element_text(size=12)); p1
  
  p2 <- data_in %>% 
    group_by(age, imd) %>% 
    summarise(med_inf = median(infections),
              l_inf = l95_func(infections),
              u_inf = u95_func(infections)) %>% 
    ggplot() +
    geom_ribbon(aes(x = age, ymin = l_inf/1000, ymax = u_inf/1000,
                    fill = as.factor(imd), group = as.factor(imd)),
                alpha = 0.25) +
    geom_line(aes(x = age, y = med_inf/1000, col = as.factor(imd), group = as.factor(imd)),
              lwd = 0.8) +
    ylim(c(0,NA)) + 
    scale_color_manual(values = imd_quintile_colors) +
    scale_fill_manual(values = imd_quintile_colors) + 
    theme_bw() + 
    labs(x = 'Age group', y = 'Infections (thousands)',
         col = 'IMD quintile', fill = 'IMD quintile') +
    theme(text=element_text(size=12)); p2
  
  p1 + p2 + plot_layout(nrow = 2, guides = 'collect') + 
    plot_annotation(tag_levels = 'a',tag_prefix = '(', tag_suffix = ')')
  
}

imd_violin_plot <- function(data_in, regional = F){
  
  vec <- c('sim','imd')
  if(regional){vec <- c(vec, 'p_engreg')}
  vec_no_sim <- vec[!vec=='sim']
  
  p <- data_in %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(infections = sum(infections),
              pop = sum(pop)) %>% 
    ungroup() %>% mutate(attack_rate = infections/pop) %>% 
    group_by(!!!syms(vec_no_sim)) %>% 
    mutate(median = median(attack_rate)) %>% 
    ggplot() + 
    geom_violin(aes(x = imd, y = 1000*attack_rate, fill = imd, col = imd), alpha = 0.4)  +
    geom_point(aes(x = imd, y = 1000*median, col = imd), size = 4)  +
    theme_bw() +
    scale_fill_manual(values = imd_quintile_colors) + 
    scale_color_manual(values = imd_quintile_colors) + 
    ylim(c(0,NA)) + 
    theme(text=element_text(size=12),
          legend.position = 'none') +
    labs(y = "Attack rate per 1000 population", x = "Age group", color = "IMD quintile", fill = 'IMD quintile')
  
  if(regional){
    p <- p + 
      facet_wrap(.~p_engreg, scales = 'fixed')
  }
  
  p
  
}

rel_age_violin_plot <- function(data_in,
                                base_age,
                                regional = F){
  
  vec <- c('sim','age')
  if(regional){ vec <- c(vec, 'p_engreg') }
  vec_no_age <- vec[!vec=='age']
  
  age_ars <- data_in %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(infections = sum(infections),
              pop = sum(pop)) %>%
    mutate(attack_rate = infections/pop) %>% 
    select(!!!syms(vec), attack_rate) %>% ungroup()
  
  base_age_ars <- age_ars %>% 
    filter(age == base_age) %>% 
    rename(base_attack_rate = attack_rate) %>% 
    select(!age)
  
  rel_age_ars <- age_ars %>% 
    left_join(base_age_ars, by = vec_no_age) %>% 
    mutate(rel_ar = attack_rate/base_attack_rate)
  
  p <- rel_age_ars %>% 
    group_by(age) %>% 
    mutate(median = median(rel_ar)) %>% 
    ggplot(aes(x=age)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    geom_violin(aes(x = age, y = rel_ar, fill = age, col = age), alpha = 0.4)  +
    geom_point(aes(x = age, y = median, col = age), size = 4)  +
    theme_bw() +
    scale_fill_manual(values = colors_p_age_group) + 
    scale_color_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=12),
          legend.position = 'none') +
    labs(y = "Relative attack rate", x = 'Age')
  
  if(regional){
    p <- p + 
      facet_wrap(. ~ p_engreg, scales = 'free') 
  }
  
  p
  
}

rel_imd_violin_plot <- function(data_in,
                                base_imd,
                                regional = F){
  vec <- c('sim','imd')
  if(regional){ vec <- c(vec, 'p_engreg') }
  vec_no_imd <- vec[!vec=='imd']
  vec_no_sim <- vec[!vec=='sim']
  
  imd_ars <- data_in %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(infections = sum(infections),
              pop = sum(pop)) %>%
    mutate(attack_rate = infections/pop) %>% ungroup() %>% 
    select(!!!syms(vec), attack_rate)
  
  base_imd_ars <- imd_ars %>% 
    filter(imd == base_imd) %>% 
    rename(base_attack_rate = attack_rate) %>% 
    select(!imd)
  
  rel_imd_ars <- imd_ars %>% 
    left_join(base_imd_ars, by = vec_no_imd) %>% 
    mutate(rel_ar = attack_rate/base_attack_rate)
  
  p <- rel_imd_ars %>% 
    group_by(!!!syms(vec_no_sim)) %>% 
    mutate(median = median(rel_ar)) %>% 
    ggplot(aes(x=imd)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    geom_violin(aes(x = imd, y = rel_ar, fill = imd, col = imd), alpha = 0.4)  +
    geom_point(aes(x = imd, y = median, col = imd), size = 4)  +
    theme_bw() +
    scale_fill_manual(values = imd_quintile_colors) + 
    scale_color_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=12),
          legend.position = 'none') +
    labs(y = "Relative attack rate", x = 'IMD Quintile')
  
  if(regional){
    p <- p +
      facet_wrap(. ~ p_engreg)
  }
  
  p
  
}

age_standardised_rel_imd_violin_plot <- function(
    demog_in,
    data_in,
    base_imd,
    regional = F
    ){
  
  pop_vec <- c('age')
  if(regional){pop_vec <- c(pop_vec, 'p_engreg')}
  reg_vec <- if(!regional){c()}else{c('p_engreg')}
  
  ## standard population
  standard_pop <- demog_in %>% 
    rename(age = Age) %>% 
    group_by(!!!syms(pop_vec)) %>% 
    summarise(st_pop = sum(Population)) %>% 
    group_by(!!!syms(reg_vec)) %>% 
    mutate(st_total_pop = sum(st_pop),
           standard_prop = st_pop/st_total_pop) 
  
  vec <- c('sim','imd')
  if(regional){vec <- c(vec, 'p_engreg')}
  vec_no_imd <- vec[vec!='imd']
  vec_no_sim <- vec[vec!='sim']
  
  age_standardised_ars <- data_in %>% 
    left_join(standard_pop, by = pop_vec) %>% 
    mutate(imd_ar = infections/pop,
           infected = imd_ar*standard_prop) %>% 
    group_by(!!!syms(vec)) %>% 
    summarise(as_attack_rate = sum(infected)) %>% 
    ungroup()
  
  base_imd_ars <- age_standardised_ars %>% 
    filter(imd == base_imd) %>% 
    rename(base_as_attack_rate = as_attack_rate) %>% 
    select(!imd)
  
  rel_imd_ars_as <- age_standardised_ars %>% 
    left_join(base_imd_ars, by = vec_no_imd) %>% 
    mutate(rel_ar = as_attack_rate/base_as_attack_rate)
  
  p <- rel_imd_ars_as %>% 
    group_by(!!!syms(vec_no_sim)) %>% 
    mutate(median = median(rel_ar)) %>% 
    ggplot(aes(x=imd)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    geom_violin(aes(x = imd, y = rel_ar, fill = imd, col = imd), alpha = 0.4)  +
    geom_point(aes(x = imd, y = median, col = imd), size = 4)  +
    theme_bw() +
    scale_fill_manual(values = imd_quintile_colors) + 
    scale_color_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=12),
          legend.position = 'none') +
    labs(y = "Relative attack rate (age-standardised)", x = 'IMD Quintile')
  
  if(regional){
    p <- p +
      facet_wrap(.~ p_engreg)
  }
  
  p
  
}

#### NOT REGIONAL ####
## If NOT in regional sensitivity analysis

if(sens_analysis != 'regional'){
  
  ## read files
  infections <- data.table(readRDS(.args[1]))
  
  #### final size #### 
  
  barchart_plot(infections)
  ggsave(.args[3], dpi=600, device = "png", width = 12, height = 6)
  
  final_size_vio <- imd_violin_plot(infections); final_size_vio
  
  age_spec_infections(infections)
  ggsave(gsub('attack_rate_bars','imd_age_infs.png',.args[3]), dpi=600, device = "png", width = 12, height = 10)
  
  #### relative final size ########
  
  ## by age 
  arr_plot_age <- rel_age_violin_plot(infections, base_age_arr); arr_plot_age
  
  ggsave(gsub('attack_rate_bars','age_spec_rel_attack_rate',.args[3]), dpi=600, device = "png", width = 12, height = 6)
  
  ## by imd 
  arr_plot_imd <- rel_imd_violin_plot(infections, base_imd_arr); arr_plot_imd
  
  #### age-standardised ####
  
  arr_plot_imd_as <- age_standardised_rel_imd_violin_plot(
    demog,
    infections,
    base_imd_arr
  ); arr_plot_imd_as
  
  final_size_vio + (arr_plot_imd + arr_plot_imd_as + plot_layout(nrow = 2)) + 
    plot_layout(nrow = 1) + plot_annotation(tag_levels = 'a',
                                            tag_prefix = '(', tag_suffix = ')')
  
  ## save
  ggsave(gsub('attack_rate_bars','imd_attack_rates',.args[3]), dpi=600, device = "png", width = 12, height = 8)
  
}else{
  
  #### REGIONAL ####
  
  for(reg_sens_analysis in 1){ # 1:4){
    
    reg_sens_analysis_name <- c('R0_1.5','R0_1.1','R0_3','R0_variable')[reg_sens_analysis]
    
    # change input and output names
    input <- gsub('outputs', paste0('outputs_', reg_sens_analysis), .args[1])
    sens_analysis_sens_analysis <- paste0(sens_analysis, '/', reg_sens_analysis_name)
    outdir <- file.path('output','figures','epidem',sens_analysis_sens_analysis)
    if(!dir.exists(outdir)){dir.create(outdir)}
    
    ## read files
    infections <- data.table(readRDS(input))
    
    #### national-level results ####
    
    {
    nat_infections <- copy(infections)
    nat_infections[, c('p_engreg', 'attack_rate') := NULL]
    nat_infections <- nat_infections[, lapply(.SD, sum), by = c('sim','age','imd')]
    nat_infections[, attack_rate := infections/pop]
    
    barchart_plot(nat_infections)
    ggsave(file.path(outdir,'attack_rate_bars.png'), dpi=600, device = "png", width = 12, height = 6)
    ggsave(.args[3], dpi=600, device = "png", width = 12, height = 6)
    
    final_size_vio <- imd_violin_plot(nat_infections); final_size_vio
    
    age_spec_infections(nat_infections)
    ggsave(file.path(outdir,'imd_age_infs.png'), dpi=600, device = "png", width = 12, height = 10)
    
    ## by age 
    arr_plot_age <- rel_age_violin_plot(nat_infections, base_age_arr); arr_plot_age
    ggsave(file.path(outdir,'age_spec_rel_attack_rate.png'), dpi=600, device = "png", width = 12, height = 6)
    
    ## by imd 
    arr_plot_imd <- rel_imd_violin_plot(nat_infections, base_imd_arr) + ylim(c(0.7, 1.55)); arr_plot_imd
    
    ## age-standardised 
    
    arr_plot_imd_as <- age_standardised_rel_imd_violin_plot(
      demog,
      nat_infections,
      base_imd_arr
    ) + ylim(c(0.7, 1.55)); arr_plot_imd_as
    
    final_size_vio + (arr_plot_imd + arr_plot_imd_as + plot_layout(nrow = 2)) + 
      plot_layout(nrow = 1) + plot_annotation(tag_levels = 'a',
                                              tag_prefix = '(', tag_suffix = ')')
    
    ## save
    ggsave(file.path(outdir,'imd_attack_rates.png'), dpi=600, device = "png", width = 12, height = 8)
    }
    
    #### regional-level results ####
    
    barchart_plot(infections, regional = T)
    ggsave(file.path(outdir,'regional_attack_rate_bars.png'), dpi=600, device = "png", width = 20, height = 14)
    
    rel_age_violin_plot(infections, base_age_arr, regional = T)
    ggsave(file.path(outdir,'regional_age_spec_arrs.png'), dpi=600, device = "png", width = 20, height = 14)
    
    imd_region_population <- demog_allreg %>% 
      group_by(p_engreg, IMD) %>% summarise(pop = sum(Population)) %>% 
      ggplot() + 
      geom_bar(aes(x = as.factor(IMD), y = pop/1e6, fill = as.factor(IMD)),
               stat = 'identity', position = 'dodge') + 
      theme_bw() + 
      scale_fill_manual(values = imd_quintile_colors) +
      theme(text=element_text(size=14),
            legend.position = 'none',
            plot.title = element_text(size = 12),
            axis.text.y = element_text(color=1),
            axis.text.x = element_text(color=1)) + 
      labs(x = 'IMD quintile', y = 'Population (millions)') +
      facet_wrap(p_engreg ~ .); imd_region_population
    
    imd_age_region_population <- demog_allreg %>% 
      ggplot() + 
      geom_line(aes(x = Age, y = Population/1e3, col = as.factor(IMD),
                    group = as.factor(IMD)),
                lwd = 0.8) + 
      theme_bw() + 
      scale_color_manual(values = imd_quintile_colors) +
      theme(text=element_text(size=14),
            plot.title = element_text(size = 12),
            axis.text.y = element_text(color=1),
            axis.text.x = element_text(color=1)) + 
      labs(x = 'Age group', y = 'Population (thousands)', col = 'IMD quintile') +
      facet_wrap(p_engreg ~ ., scales = 'free'); imd_age_region_population
    ggsave(file.path(outdir,'imd_age_region_population.png'), dpi=600, device = "png", width = 25, height = 14)
    
    reg_final_size_vio <- imd_violin_plot(infections, regional = T); reg_final_size_vio
    
    rel_reg_final_size_vio <- rel_imd_violin_plot(infections, 
                                                  base_imd_arr,
                                                  regional = T); rel_reg_final_size_vio
    
    as_rel_reg_final_size_vio <- age_standardised_rel_imd_violin_plot(demog_allreg,
                                                                      infections,
                                                                      base_imd_arr,
                                                                      regional = T); as_rel_reg_final_size_vio
    
    imd_region_population + reg_final_size_vio + rel_reg_final_size_vio + as_rel_reg_final_size_vio +
      plot_layout(nrow = 2) + plot_annotation(tag_levels = 'a',
                                              tag_prefix = '(', tag_suffix = ')')
    
    ggsave(file.path(outdir,'final_size_imd_w_pop.png'), dpi=600, 
           device = "png", width = 16, height = 14)
    
    
  }
  
}  
  






