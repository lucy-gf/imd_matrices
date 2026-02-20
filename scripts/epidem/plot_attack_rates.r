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
  source(here::here('scripts','epidem','plot_epidem_functions.R'))
  
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


#### NOT REGIONAL ####
## If NOT in regional sensitivity analysis

if(sens_analysis != 'regional'){
  
  ## read files
  infections <- data.table(readRDS(.args[1]))
  
  #### final size #### 
    
  ## save csvs
  sav <- infections %>% group_by(imd, sim) %>% summarise(infections = sum(infections), pop=sum(pop)) %>% 
    mutate(ar = 1000*infections/pop) %>% 
    group_by(imd) %>% summarise(ar = median(ar)) %>% select(imd, ar)
  write_csv(sav, gsub('attack_rate_bars.png','med_final_size.csv',.args[3]))
  sav2 <- infections %>% group_by(imd, sim) %>% summarise(infections = sum(infections), pop=sum(pop)) %>% 
    mutate(ar = 1000*infections/pop) %>% ungroup
  sav3 <- sav2 %>% left_join(sav2 %>% filter(imd=='5') %>% select(sim, ar) %>% rename(base_ar = ar), by = 'sim') %>% 
    group_by(imd) %>% summarise(med_rate = median(ar/base_ar - 1),
                                l_rate = l95_func(ar/base_ar - 1),
                                u_rate = u95_func(ar/base_ar - 1)) %>% 
    ungroup() %>% mutate(neat = paste0(round(100*med_rate, 1),
                                       ' (', round(100*l_rate, 1), ' - ',
                                       round(100*u_rate, 1), ')'))
  write_csv(sav3, gsub('attack_rate_bars.png','med_final_arrs.csv',.args[3]))
  
  ## plot
  barchart_plot(infections)
  ggsave(.args[3], dpi=600, device = "png", width = 12, height = 6)
  
  final_size_vio <- imd_violin_plot(infections); final_size_vio
  
  age_spec_infections(infections)
  ggsave(gsub('attack_rate_bars','imd_age_infs',.args[3]), dpi=600, device = "png", width = 12, height = 10)
  
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
  
  for(reg_sens_analysis in 4){ # 1:4){
    
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
    
    ## save csvs
    sav <- nat_infections %>% group_by(imd, sim) %>% summarise(infections = sum(infections), pop=sum(pop)) %>% 
      mutate(ar = 1000*infections/pop) %>% 
      group_by(imd) %>% summarise(ar = median(ar)) %>% select(imd, ar)
    write_csv(sav, file.path(outdir,'med_final_size.csv'))
    sav2 <- nat_infections %>% group_by(imd, sim) %>% summarise(infections = sum(infections), pop=sum(pop)) %>% 
      mutate(ar = 1000*infections/pop) %>% ungroup
    sav3 <- sav2 %>% left_join(sav2 %>% filter(imd=='5') %>% select(sim, ar) %>% rename(base_ar = ar), by = 'sim') %>% 
      group_by(imd) %>% summarise(med_rate = median(ar/base_ar - 1),
                                  l_rate = l95_func(ar/base_ar - 1),
                                  u_rate = u95_func(ar/base_ar - 1)) %>% 
      ungroup() %>% mutate(neat = paste0(round(100*med_rate, 1),
                                         ' (', round(100*l_rate, 1), ' - ',
                                         round(100*u_rate, 1), ')'))
    write_csv(sav3, file.path(outdir,'med_final_arrs.csv'))
    
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
      theme_bw() + ylim(c(0,NA)) + 
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
  






