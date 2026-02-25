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
suppressPackageStartupMessages(library(ggnewscale))
options(dplyr.summarise.inform = FALSE) 

.args <- if (interactive()) c(
  file.path("output", "data", "epidem","base","epidemic_outputs.rds"),
  file.path("output", "data", "epidem","regional","epidemic_outputs.rds"),
  file.path('output','figures','epidem','merged_attack_rates.png')
) else commandArgs(trailingOnly = TRUE)

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
  age_structure_num <- 1
  
  ## Set base levels for IMD and age
  base_imd_arr <- 5
  base_age_arr <- '35-39'
  
  age_colors <- colors_p_age_group
  
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

## read files
infections_base <- data.table(readRDS(.args[1]))

reg_sens_analysis <- 4; reg_sens_analysis_name <- 'R0_variable'
input <- gsub('outputs', paste0('outputs_', reg_sens_analysis), .args[2])

## read files
infections_regional <- data.table(readRDS(input))
## make national
reg_infections <- copy(infections_regional)
reg_infections[, c('p_engreg', 'attack_rate') := NULL]
reg_infections <- reg_infections[, lapply(.SD, sum), by = c('sim','age','imd')]
reg_infections[, attack_rate := infections/pop]

combined_infections <- rbind(
  infections_base %>% mutate(model = 'National-level'),
  reg_infections %>% mutate(model = 'Regional-level')
)

final_size_vio <- imd_violin_plot(combined_infections,
                                  combined = T); final_size_vio
ggsave(gsub('merged_attack_rates.png','merged_final_size.png',.args[3]), dpi=600, device = "png", width = 10, height = 7)  

## by imd 
cat('Crude rel. attack rates:\n')
arr_plot_imd <- rel_imd_violin_plot(
  data_in = combined_infections, 
  base_imd = base_imd_arr,
  combined = T); arr_plot_imd

## age-standardised 
cat('\nAge-standardised rel. attack rates:\n')
arr_plot_imd_as <- age_standardised_rel_imd_violin_plot(
  demog,
  combined_infections,
  base_imd_arr,
  combined = T
) + theme(legend.position = 'none'); arr_plot_imd_as

## age-standardised by region
cat('\nAge-standardised rel. attack rates by region:\n')
arr_plot_imd_as_reg <- age_standardised_rel_imd_violin_plot(
  demog_allreg,
  infections_regional,
  base_imd_arr,
  regional = T,
  combined = F
); arr_plot_imd_as_reg


#### R0 by region ####
# source(paste0(source_dir,"/parsF_.r"))
# beta <- read_csv("output/data/epidem/base/beta.csv",show_col_types=F)
# beta_med <- median(beta$beta)
# reg_cm <- data.table(suppressWarnings(read_csv("output/data/cont_matrs/regional/fitted_matrs_balanced.csv", show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
# reg_cm$p_age_group <- factor(reg_cm$p_age_group, levels = age_labels)
# reg_cm$c_age_group <- factor(reg_cm$c_age_group, levels = age_labels)
# 
# r0_track_df <- data.frame()
# for(reg in unique(reg_cm$p_engreg)){
#   cm_base <- reg_cm[p_engreg==reg]
#   cm_base[, p_engreg := NULL]
#   cm <- cm_base[order(bootstrap_index, p_imd_q, p_age_group, c_imd_q, c_age_group)]
#   R0TRACK <- c()
#   for(bs_i in unique(cm$bootstrap_index)){
#     cm_i <- cm_base %>% filter(bootstrap_index == bs_i)
#     cm_i <- cm_i %>% 
#       mutate(p = paste0(p_imd_q, '_', p_age_group),
#              c = paste0(c_imd_q, '_', c_age_group)) %>% 
#       select(p,c,n) %>% pivot_wider(names_from = c, values_from = n) 
#     pvec <- cm$p 
#     cm_i <- cm_i %>% select(!p) %>% as.matrix()
#     betanew <- R0(pars, cm_in = cm_i, R0assumed = 1.5, printout = 0) 
#     R0TRACK <- c(R0TRACK, 1.5*beta_med/betanew)
#   }
#   r0_track_df <- rbind(r0_track_df,
#                        data.frame(region=reg,
#                                   bootstrap_index = unique(cm$bootstrap_index),
#                                   r0 = R0TRACK))
#   cat(reg, '-')
# }
# 
# write_csv(r0_track_df, "output/data/epidem/regional/regional_r0.csv")

beta <- read_csv("output/data/epidem/base/beta.csv",show_col_types=F)
r0_track_df <- read_csv("output/data/epidem/regional/regional_r0.csv", show_col_types=F)

false_legend <- imd_model_colors[c(3,8)]
names(false_legend) <- c('National-level', 'Regional-level')

cat('\nWarnings expected:\n')
national_beta_plot <- beta %>% 
  summarise(m = median(beta), 
            l = quantile(beta, 0.025), u = quantile(beta, 0.975)) %>% 
  mutate(region = 'National', model='National-level') %>% 
  rbind(data.table(m=10,l=10,u=10,region='National',model='Regional-level')) %>% 
  ggplot() + 
  geom_errorbar(aes(x = region, ymin = l, ymax = u),
                width = 0.4) +
  geom_point(aes(x = region, y = m),
             size = 2) +
  geom_ribbon(aes(x = region, ymin=10,ymax=10, fill=model, col=model), alpha = 0.4) +
  geom_point(aes(x = region, y=10, col=model), size = 4) +
  theme_bw() + ylim(c(0,0.25)) + 
  scale_fill_manual(values = false_legend) + 
  scale_color_manual(values = false_legend) + 
  theme(legend.position='bottom') +
  labs(x = '', y = 'Transmissibility', col = 'Model', fill = 'Model'); national_beta_plot

regional_r0_plot <- r0_track_df %>% 
  group_by(region) %>% 
  summarise(m = median(r0), 
            l = quantile(r0, 0.025), u = quantile(r0, 0.975)) %>% 
  ggplot() + 
  geom_errorbar(aes(x = region, ymin = l, ymax = u),
                width = 0.4) + 
  geom_point(aes(x = region, y = m),
             size = 2) +
  theme_bw() + ylim(c(0,NA)) + 
  # scale_color_manual(values = colors_p_engreg) +
  labs(x = '', y = 'R0') + 
  theme(#axis.text.x=element_text(angle = 30, hjust = 1),
        legend.position = 'none'); regional_r0_plot

#### patchwork ####

layout <- "
ABBBBBBBB
CCCCEEEEE
CCCCEEEEE
DDDDEEEEE
DDDDEEEEE
"

national_beta_plot + regional_r0_plot + arr_plot_imd + arr_plot_imd_as + 
  arr_plot_imd_as_reg + 
  plot_layout(design = layout) + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')')

## save
ggsave(.args[3], dpi=600, device = "png", width = 14, height = 10)  



