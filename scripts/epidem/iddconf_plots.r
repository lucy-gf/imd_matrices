# iddconf figures

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
  file.path("output", "data", "epidem","hom_mixing","epidemic_outputs.rds"),
  file.path('output','figures','epidem','iddconf_fig1.png')
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

infections_hom <- data.table(readRDS(.args[3]))

combined_infections <- rbind(
  infections_base %>% mutate(model = 'National-level'),
  reg_infections %>% mutate(model = 'Regional-level'),
  infections_hom %>% mutate(model = 'Homogeneous mixing')
)

vec <- c('sim','imd','model')
vec_no_sim <- vec[!vec=='sim']
vec_no_imd <- vec[!vec=='imd']
pop_vec <- c('age')
reg_vec <- c()

blank_cols <- imd_model_colors[c(1:5,11:15)]
blank_cols[1:5] <- 'NA'
blank_labels <- imd_model_labels[c(1:5,11:15)]
blank_labels[1:5] <- ''
imd_model_labels[1:5] <- gsub('National-level', 'IMD-stratified', imd_model_labels[1:5])

combined_infections$model <- factor(combined_infections$model, levels = unique(combined_infections$model))

p <- combined_infections %>% 
  filter(!grepl('egio',model)) %>% 
  group_by(!!!syms(vec)) %>% 
  summarise(infections = sum(infections),
            pop = sum(pop)) %>% 
  ungroup() %>% mutate(attack_rate = infections/pop) %>% 
  group_by(!!!syms(vec_no_sim)) %>% 
  mutate(median = median(attack_rate)) %>% 
  ggplot() + 
  theme_bw() + 
  ylim(c(0,NA)) + 
  theme(text=element_text(size=12)) +
  labs(y = "Attack rate per 1000 population", x = "IMD quintile", color = "IMD quintile", fill = 'IMD quintile') +
  geom_violin(aes(x = imd, y = 1000*attack_rate, fill = interaction(imd,model), col = interaction(imd,model), 
                  group = interaction(model,imd)), alpha = 0.4)  +
  geom_point(aes(x = imd, y = 1000*median, col = interaction(imd,model), group = model), 
             size = 4, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = imd_model_colors[c(1:5,11:15)],
                    labels = imd_model_labels[c(1:5,11:15)]) + 
  scale_color_manual(values = imd_model_colors[c(1:5,11:15)],
                     labels = imd_model_labels[c(1:5,11:15)]) + 
  labs(col = '',fill = ''); p

imd_ars <- combined_infections %>% 
  filter(!grepl('egio',model)) %>% 
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

rel_imd_ars$model <- factor(rel_imd_ars$model, levels = unique(combined_infections$model))

p1 <- rel_imd_ars %>% 
  group_by(!!!syms(vec_no_sim)) %>% 
  mutate(median = median(rel_ar)) %>% 
  ggplot(aes(x=imd)) + 
  geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
  theme_bw() +
  theme(legend.position = 'none',
        text=element_text(size=12)) +
  labs(y = "Relative attack rate", x = 'IMD quintile') + 
  geom_violin(aes(x = imd, y = rel_ar, fill = interaction(imd,model), 
                  col = interaction(imd,model), group = interaction(model,imd)), 
              alpha = 0.4, , scale='width')  +
  geom_point(aes(x = imd, y = median, col = interaction(imd,model), group = model), 
             size = 4, position = position_dodge(width = 0.9)) +
  ylim(c(0.7,1.3)) +
  scale_fill_manual(values = imd_model_colors[c(1:5,11:15)],
                    labels = imd_model_labels[c(1:5,11:15)]) + 
  scale_color_manual(values = imd_model_colors[c(1:5,11:15)],
                     labels = imd_model_labels[c(1:5,11:15)]) + 
  labs(col = '',fill = ''); p1

p1b <- rel_imd_ars %>% 
  group_by(!!!syms(vec_no_sim)) %>% 
  mutate(median = median(rel_ar)) %>% 
  ggplot(aes(x=imd)) + 
  geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
  theme_bw() +
  theme(legend.position = 'none',
        text=element_text(size=12)) +
  labs(y = "Relative attack rate", x = 'IMD quintile') + 
  geom_violin(aes(x = imd, y = rel_ar, fill = interaction(imd,model), 
                  col = interaction(imd,model), group = interaction(model,imd)), 
              alpha = 0.4, , scale='width')  +
  geom_point(aes(x = imd, y = median, col = interaction(imd,model), group = model), 
             size = 4, position = position_dodge(width = 0.9)) +
  ylim(c(0.7,1.3)) +
  scale_fill_manual(values = blank_cols,
                    labels = blank_labels) + 
  scale_color_manual(values = blank_cols,
                     labels = blank_labels) + 
  labs(col = '',fill = ''); p1b
  
## standard population
standard_pop <- demog %>% 
  rename(age = Age) %>% 
  group_by(!!!syms(pop_vec)) %>% 
  summarise(st_pop = sum(Population)) %>% 
  group_by(!!!syms(reg_vec)) %>% 
  mutate(st_total_pop = sum(st_pop),
         standard_prop = st_pop/st_total_pop) 
  
age_standardised_ars <- combined_infections %>% 
  filter(!grepl('egio',model)) %>% 
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

rel_imd_ars_as$model <- factor(rel_imd_ars_as$model, levels = unique(combined_infections$model))

p2 <- rel_imd_ars_as %>% 
  group_by(!!!syms(vec_no_sim)) %>% 
  mutate(median = median(rel_ar)) %>% 
  ggplot(aes(x=imd)) + 
  geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
  theme_bw() +
  theme(text=element_text(size=12)) +
  labs(y = "Relative attack rate (age-standardised)", x = 'IMD quintile') + 
    geom_violin(aes(x = imd, y = rel_ar, fill = interaction(imd,model), 
                    col = interaction(imd,model), group = interaction(model,imd)), 
                alpha = 0.4, scale='width')  +
    geom_point(aes(x = imd, y = median, col = interaction(imd,model), group = model), 
               size = 4, position = position_dodge(width = 0.9)) +
    ylim(c(0.7,1.3)) +
    scale_fill_manual(values = imd_model_colors[c(1:5,11:15)],
                      labels = imd_model_labels[c(1:5,11:15)]) + 
    scale_color_manual(values = imd_model_colors[c(1:5,11:15)],
                       labels = imd_model_labels[c(1:5,11:15)]) + 
    labs(col = '',fill = ''); p2

p2b <- rel_imd_ars_as %>% 
  group_by(!!!syms(vec_no_sim)) %>% 
  mutate(median = median(rel_ar)) %>% 
  ggplot(aes(x=imd)) + 
  geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
  theme_bw() +
  theme(text=element_text(size=12)) +
  labs(y = "Relative attack rate (age-standardised)", x = 'IMD quintile') + 
  geom_violin(aes(x = imd, y = rel_ar, fill = interaction(imd,model), 
                  col = interaction(imd,model), group = interaction(model,imd)), 
              alpha = 0.4, scale='width')  +
  geom_point(aes(x = imd, y = median, col = interaction(imd,model), group = model), 
             size = 4, position = position_dodge(width = 0.9)) +
  ylim(c(0.7,1.3)) +
  scale_fill_manual(values = blank_cols,
                    labels = blank_labels) + 
  scale_color_manual(values = blank_cols,
                     labels = blank_labels) + 
  labs(col = '',fill = ''); p2b

p1 + p2 + plot_layout(nrow = 2, guides = 'collect')
ggsave(.args[4], width = 9, height = 7)

p1b + p2b + plot_layout(nrow = 2, guides = 'collect')
ggsave(gsub('.png','_1.png',.args[4]), width = 9, height = 7)



### homogenous mixing contact matrix
cm_1000 <- read_csv(file.path("output", "data", "cont_matrs","base","fitted_matrs_balanced.csv"),
                    show_col_types = F)
cm <- cm_1000 %>% 
  group_by(p_age_group, p_imd_q, c_age_group, c_imd_q) %>% 
  summarise(n = mean(n))
cm$p_age_group <- factor(cm$p_age_group, levels = pars$ages)
cm$c_age_group <- factor(cm$c_age_group, levels = pars$ages)

# weighted sum over IMD quintiles by age groups
nimd <- n_distinct(cm$p_imd_q)
cm_plot <- cm %>% 
  left_join(demog %>% select(IMD, Age, Population) %>% 
              rename(p_imd_q = IMD,
                     p_age_group = Age),
            by = c('p_imd_q','p_age_group')) %>% 
  group_by(p_age_group, c_imd_q, c_age_group) %>% 
  mutate(age_spec_pop = sum(Population),
         age_spec_imd_prop = Population/age_spec_pop) %>% 
  ungroup() %>% 
  group_by(p_imd_q, p_age_group, age_spec_imd_prop, c_imd_q, c_age_group) %>% 
  summarise(n = sum(n)) %>% 
  group_by(p_age_group, c_age_group) %>% 
  mutate(weighted_mean = weighted.mean(x = n, w = age_spec_imd_prop)) %>% 
  group_by(p_imd_q, p_age_group, c_imd_q, c_age_group) %>% 
  summarise(n = age_spec_imd_prop*weighted_mean) %>% 
  ungroup() 

cm_plot %>% 
  ggplot() + 
  geom_tile(aes(x = p_age_group, y = c_age_group, fill = n)) +
  theme_bw() + 
  facet_grid(c_imd_q ~ p_imd_q, switch="both") +
  scale_fill_viridis() +
  labs(
    x = 'Participant IMD, age group',
    y = 'Contact IMD, age group',
    fill = 'Mean daily\ncontacts'
  ) + 
  scale_x_discrete(expand = expansion(c(0.00075, 0.00075))) + 
  scale_y_discrete(expand = expansion(c(0.00075, 0.00075))) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 18),
        axis.ticks = element_line(linewidth = 0),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12),
        axis.text.y = element_text(size = 12)) 

ggsave(gsub('.png','_hom_matrix.png',.args[4]), width = 14, height = 12)






