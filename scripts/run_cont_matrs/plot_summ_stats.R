
## PLOT CONTACT MATRIX SUMMARY STATISTICS ##

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
  file.path("output", "data", "cont_matrs","regional","imd_assortativity.csv"),
  "regional",
  file.path("output", "figures", "cont_matrs","regional","summstats.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))
source(here::here('scripts','setup','colors.R'))

if(!file.exists(gsub('/summstats.png','',.args[3]))){dir.create(gsub('/summstats.png','',.args[3]))}

#### READ IN DATA ####

m_a_d <- read_csv(gsub('imd_assortativity.csv','m_a_d.csv',.args[1]), show_col_types=F)
m_i_d <- read_csv(gsub('imd_assortativity.csv','m_i_d.csv',.args[1]), show_col_types=F)
m_i_d_no_abs <- read_csv(gsub('imd_assortativity.csv','m_i_d_no_abs.csv',.args[1]), show_col_types=F)
age_assortativity <- read_csv(gsub('imd_assortativity.csv','age_assortativity.csv',.args[1]), show_col_types=F)
imd_assortativity <- read_csv(.args[1], show_col_types=F)

sens_analysis <- .args[2]

## age distribution 

age_structure_num <- ifelse(!grepl('nhs_ages',sens_analysis), 1, 2)

if(grepl('nhs_ages',sens_analysis)){
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
}

imd_age_raw <- data.table(read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F))

if(grepl('regional',sens_analysis)){
  
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
  imd_age <- imd_age %>% arrange(imd_q, age)
  
}

#### PLOT ####

#### MEAN AGE DIFFERENCE ####
## (using midpoint of each age group), 
## 80 for 75+ age group
## (or 85 for 80+ age group in nhs_ages)

mean_age_diff_plot <- m_a_d %>% 
  ggplot() + 
  geom_tile(aes(x=p_i, y=c_i, fill=m)) +
  theme_bw() + scale_fill_viridis(option='A') +
  theme(text = element_text(size = 12)) +
  labs(x='Participant IMD quintile',
       y='Contact IMD quintile',
       fill = 'Mean age\ndifference')

if(grepl('regional',sens_analysis)){
  mean_age_diff_plot <- mean_age_diff_plot + facet_wrap(.~p_engreg)
}

mean_age_diff_plot + ggtitle("Mean age difference")

ggsave(gsub('summstats.png','mean_age_diff.png',.args[3]), width = 8, height = 7)

#### MEAN IMD DIFFERENCE ####

m_i_d$p_a <- factor(m_i_d$p_a, levels = age_labels)
m_i_d$c_a <- factor(m_i_d$c_a, levels = age_labels)

m_i_d_plot <- m_i_d %>% 
  ggplot() + 
  geom_tile(aes(x=p_a, y=c_a, fill=m)) +
  theme_bw() + scale_fill_viridis(option='A') +
  theme(text = element_text(size = 12)) +
  labs(x='Participant age group',
       y='Contact age group',
       fill = 'Mean absolute\nIMD difference') + 
  coord_fixed()

if(grepl('regional',sens_analysis)){
  m_i_d_plot <- m_i_d_plot + facet_wrap(.~p_engreg)
}

m_i_d_plot
ggsave(gsub('summstats.png','mean_imd_diff.png',.args[3]), width = 8.5, height = 7)

m_i_d_no_abs$p_a <- factor(m_i_d_no_abs$p_a, levels = age_labels)
m_i_d_no_abs$c_a <- factor(m_i_d_no_abs$c_a, levels = age_labels)

m_i_d_plot_no_abs <- m_i_d_no_abs %>% 
  ggplot() + 
  geom_tile(aes(x=p_a, y=c_a, fill=m)) +
  theme_bw() + 
  scale_fill_gradientn(colours = c("blue", "dodgerblue", "white", "orange", "red"),
                      rescaler = ~ scales::rescale_mid(.x, mid = 0)) +
  theme(text = element_text(size = 12)) +
  labs(x='Participant age group',
       y='Contact age group',
       fill = 'Mean IMD\ndifference') + 
  coord_fixed(); m_i_d_plot_no_abs

if(sens_analysis == 'regional'){
  m_i_d_plot_no_abs <- m_i_d_plot_no_abs + facet_wrap(.~p_engreg)
}

m_i_d_plot_no_abs
ggsave(gsub('summstats.png','mean_imd_diff_not_abs.png',.args[3]), width = 8, height = 7)


#### ASSORTATIVITY ####

age_assortativity$p_age_group <- factor(age_assortativity$p_age_group, levels = age_labels)
age_assortativity$c_age_group <- factor(age_assortativity$c_age_group, levels = age_labels)

if(!grepl('regional',sens_analysis)){
  
  #### NOT REGIONAL ####
  
  imd_assort_matr <- imd_assortativity %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = mean)) + 
    scale_fill_viridis() + 
    theme_bw() +
    labs(x = 'Participant IMD quintile', y = 'Contact IMD quintile', 
         fill = 'Assortativity\nby age group') + 
    theme(text = element_text(size = 12))
  
  age_assort_matr <- age_assortativity %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = mean)) + 
    scale_fill_viridis(limits = c(0,NA), trans='pseudo_log') + 
    theme_bw() +
    labs(x = 'Participant age group', y = 'Contact age group', 
         fill = 'Assortativity\nby IMD quintile') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          text = element_text(size = 12))
  
  imd_assort_matr + age_assort_matr + plot_layout(nrow = 1)
  
  ggsave(gsub('.png','_assortativity_matrices.png',.args[3]), width = 12, height = 5)
  
  age_assort_matr + imd_assort_matr + mean_age_diff_plot + plot_layout(nrow = 1) +
    plot_annotation(tag_levels = 'a',
                    tag_prefix = '(', tag_suffix = ')')
  
  ggsave(file = .args[3], width = 18, height = 5)
  
  age_assort_matr
  
  ggsave(gsub('summstats.png','imd_assortativity_matrix.png',.args[3]), width = 6, height = 5)
  
}else{
  
  #### REGIONAL ####
  
  imd_assort_matr <- imd_assortativity %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = mean)) + 
    scale_fill_viridis() + 
    theme_bw() + facet_wrap(.~p_engreg) +
    labs(x = 'Participant IMD quintile', y = 'Contact IMD quintile', 
         fill = 'Assortativity\nby age group') + 
    theme(text = element_text(size = 12))
  
  age_assort_matr <- age_assortativity %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = mean)) + 
    scale_fill_viridis(#limits = c(0,NA), 
                       trans='pseudo_log') + 
    theme_bw() + facet_wrap(.~p_engreg) +
    labs(x = 'Participant age group', y = 'Contact age group', 
         fill = 'Assortativity\nby IMD quintile') +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          text = element_text(size = 12))
  
  imd_assort_matr + age_assort_matr + plot_layout(nrow = 1)
  
  ggsave(gsub('summstats.png','assortativity_matrices.png',.args[3]), width = 20, height = 10)
  
  age_assort_matr + imd_assort_matr + mean_age_diff_plot + plot_layout(nrow = 1) +
    plot_annotation(tag_levels = 'a',
                    tag_prefix = '(', tag_suffix = ')')
  
  ggsave(gsub('.png','_regional.png',.args[3]), width = 30, height = 10)
  
  age_assort_matr
  
  ggsave(gsub('summstats.png','imd_assortativity_matrix.png',.args[3]), width = 10, height = 10)
  
}

# ## proportion of contacts intra-group
# 
# prop_in_group_df <- balanced_matr %>% 
#   mutate(p_var = paste0(p_imd_q, '_', p_age_group),
#          c_var = paste0(c_imd_q, '_', c_age_group),
#          flag = p_var==c_var) %>% 
#   group_by(bootstrap_index, p_var, flag) %>% 
#   mutate(n_flag = sum(n)) %>% 
#   ungroup() %>% 
#   select(bootstrap_index, p_age_group, p_imd_q, flag, n_flag) %>% 
#   unique() %>% 
#   pivot_wider(names_from = flag, values_from = n_flag) %>% 
#   mutate(prop_in_group = `TRUE`/(`TRUE` + `FALSE`))
# 
# prop_in_group_df$p_age_group <- factor(prop_in_group_df$p_age_group,
#                                        levels = age_labels)
# 
# prop_in_group_plot <- prop_in_group_df %>% 
#   group_by(p_age_group, p_imd_q) %>% 
#   summarise(m = median(prop_in_group),
#             l = quantile(prop_in_group, 0.025),
#             u = quantile(prop_in_group, 0.975)) %>% 
#   ggplot() + 
#   geom_ribbon(aes(x = p_age_group, ymin = l, ymax=u, fill = as.factor(p_imd_q),
#                   group = as.factor(p_imd_q)), alpha = 0.4) + 
#   geom_line(aes(x = p_age_group, y = m, col = as.factor(p_imd_q),
#                 group = as.factor(p_imd_q)), lwd = 0.8) + 
#   theme_bw() + labs(x = 'Age',y='Proportion of contacts in\nage group and IMD quintile',
#                     col = 'IMD quintile', fill = 'IMD quintile') + 
#   scale_color_manual(values = imd_quintile_colors) +
#   scale_fill_manual(values = imd_quintile_colors) +
#   theme(text = element_text(size = 14)) + ylim(c(0,NA))
# 
# prop_in_age_df <- balanced_matr %>% 
#   mutate(flag = p_age_group==c_age_group) %>% 
#   group_by(bootstrap_index, p_age_group, p_imd_q, flag) %>% 
#   mutate(n_flag = sum(n)) %>% 
#   ungroup() %>% 
#   select(bootstrap_index, p_age_group, p_imd_q, flag, n_flag) %>% 
#   unique() %>% 
#   pivot_wider(names_from = flag, values_from = n_flag) %>% 
#   mutate(prop_in_group = `TRUE`/(`TRUE` + `FALSE`))
# 
# prop_in_age_df$p_age_group <- factor(prop_in_age_df$p_age_group,
#                                      levels = age_labels)
# 
# prop_in_age_plot <- prop_in_age_df %>% 
#   group_by(p_age_group, p_imd_q) %>% 
#   summarise(m = median(prop_in_group),
#             l = quantile(prop_in_group, 0.025),
#             u = quantile(prop_in_group, 0.975)) %>% 
#   ggplot() + 
#   geom_ribbon(aes(x = p_age_group, ymin = l, ymax=u, fill = as.factor(p_imd_q),
#                   group = as.factor(p_imd_q)), alpha = 0.4) + 
#   geom_line(aes(x = p_age_group, y = m, col = as.factor(p_imd_q),
#                 group = as.factor(p_imd_q)), lwd = 0.8) + 
#   theme_bw() + labs(x = 'Age',y='Proportion of contacts\nin age group',
#                     col = 'IMD quintile', fill = 'IMD quintile') + 
#   scale_color_manual(values = imd_quintile_colors) +
#   scale_fill_manual(values = imd_quintile_colors) +
#   theme(text = element_text(size = 14)) + ylim(c(0,NA))
# 
# prop_in_imd_df <- balanced_matr %>% 
#   mutate(flag = p_imd_q==c_imd_q) %>% 
#   group_by(bootstrap_index, p_age_group, p_imd_q, flag) %>% 
#   mutate(n_flag = sum(n)) %>% 
#   ungroup() %>% 
#   select(bootstrap_index, p_age_group, p_imd_q, flag, n_flag) %>% 
#   unique() %>% 
#   pivot_wider(names_from = flag, values_from = n_flag) %>% 
#   mutate(prop_in_group = `TRUE`/(`TRUE` + `FALSE`))
# 
# prop_in_imd_df$p_age_group <- factor(prop_in_imd_df$p_age_group,
#                                      levels = age_labels)
# 
# prop_in_imd_plot <- prop_in_imd_df %>% 
#   group_by(p_age_group, p_imd_q) %>% 
#   summarise(m = median(prop_in_group),
#             l = quantile(prop_in_group, 0.025),
#             u = quantile(prop_in_group, 0.975)) %>% 
#   ggplot() + 
#   geom_ribbon(aes(x = p_age_group, ymin = l, ymax=u, fill = as.factor(p_imd_q),
#                   group = as.factor(p_imd_q)), alpha = 0.4) + 
#   geom_line(aes(x = p_age_group, y = m, col = as.factor(p_imd_q),
#                 group = as.factor(p_imd_q)), lwd = 0.8) + 
#   theme_bw() + labs(x = 'Age',y='Proportion of contacts\nin IMD quintile',
#                     col = 'IMD quintile', fill = 'IMD quintile') + 
#   scale_color_manual(values = imd_quintile_colors) +
#   scale_fill_manual(values = imd_quintile_colors) +
#   theme(text = element_text(size = 14)) + ylim(c(0,NA))
# 
# prop_in_group_plot + prop_in_age_plot + 
#   prop_in_imd_plot +
#   plot_layout(nrow=3, guides='collect')
