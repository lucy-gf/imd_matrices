## COMPARING THE AGE- AND IMD-SPECIFIC ATTACK RATE RESULTS ##
## ACROSS EACH OF THE SENSITIVITY ANALYSES USED ##

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

options(scipen = 9999)

## source colors etc.
source(here::here('scripts','assign_imd','assign_imd_fcns.R'))
source(here::here('scripts','setup','colors.R'))
source(here::here('scripts','epidem','plot_epidem_functions.R'))

## load in epidemic outputs
sens_analyses <- c('large_n_age', 'no_cap_100', 'balance_sett_spec')
read_epid_outputs <- function(str){
  readRDS(file.path("output", "data", "epidem", str, "epidemic_outputs.rds"))
}

epid_outputs <- map(
  .x = sens_analyses,
  .f = read_epid_outputs
)
names(epid_outputs) <- sens_analyses

## make into data frame
epid_outputs_l <- rbindlist(epid_outputs, idcol = "analysis")

## compared to base analysis
base_outputs <- read_epid_outputs("base")
setnames(base_outputs, 'attack_rate', 'base_attack_rate')
base_outputs[, c('infections','pop','imd_pop') := NULL]

## merge data
epid_outputs_l <- epid_outputs_l[base_outputs, on = c('sim','age','imd')]

epid_outputs_l[, relative_attack_rate := ((attack_rate/base_attack_rate) - 1)]
epid_outputs_l[, absolute_change := 1000*(attack_rate - base_attack_rate)]

epid_outputs_agg <- epid_outputs_l %>% 
  group_by(analysis, age, imd) %>% 
  summarise(mean_rel_ar = mean(relative_attack_rate),
            l95 = eti95L(relative_attack_rate),
            u95 = eti95U(relative_attack_rate),
            mean_abs_ar = mean(absolute_change),
            l95_abs = eti95L(absolute_change),
            u95_abs = eti95U(absolute_change))

## plot

p1 <- epid_outputs_agg %>% 
  ggplot() +
  geom_point(aes(x = age, y = mean_rel_ar, col = imd),
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = age, ymin = l95, ymax = u95, 
                    group = as.factor(imd), col = imd), 
                width = 0.4, position = position_dodge(width = 0.9), 
                alpha= 0.75) +
  theme_bw() +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
  facet_grid(analysis ~ ., scale = 'free',
             labeller = labeller(analysis = sens_analysis_names)) +
  scale_color_manual(values = imd_quintile_colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Age group', y = 'Percentage change in attack rate (compared to in base analysis)',
       col = 'IMD quintile')

p2 <- epid_outputs_agg %>% 
  ggplot() +
  geom_point(aes(x = age, y = mean_abs_ar, col = imd),
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = age, ymin = l95_abs, ymax = u95_abs, 
                    group = as.factor(imd), col = imd), 
                width = 0.4, position = position_dodge(width = 0.9), 
                alpha= 0.75) +
  theme_bw() +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
  facet_grid(analysis ~ ., scale = 'free',
             labeller = labeller(analysis = sens_analysis_names)) +
  scale_color_manual(values = imd_quintile_colors) +
  labs(x = 'Age group', y = 'Change in attack rate per 1000 population (compared to in base analysis)',
       col = 'IMD quintile')

p1 + p2 + plot_layout(nrow = 1, 
                      guides = 'collect')
ggsave(file.path('output','figures','epidem','sens_analyses_comparison.png'),
       width = 20, height = 10)

## distribution of total infections across groups

inf_distr <- epid_outputs_l %>% 
  select(!c(base_attack_rate, relative_attack_rate, absolute_change)) %>% 
  bind_rows(read_epid_outputs("base") %>% mutate(analysis = 'base')) %>% 
  group_by(analysis, sim) %>% 
  mutate(total_infections = sum(infections)) %>% 
  ungroup() %>% 
  mutate(prop_of_infections = infections/total_infections) %>% 
  select(analysis, sim, age, imd, prop_of_infections)

base_inf_distr <- inf_distr %>% filter(analysis == 'base') %>% 
  select(!analysis) %>% rename(base_prop_of_infections = prop_of_infections)

rel_inf_distr <- inf_distr %>% filter(analysis != 'base') %>% 
  left_join(base_inf_distr, by = c('sim','age','imd')) %>% 
  mutate(relative_prop = (prop_of_infections/base_prop_of_infections) - 1)

rel_inf_distr_agg <- rel_inf_distr %>% 
  group_by(analysis, age, imd) %>% 
  summarise(mean_rel_ar = mean(relative_prop),
            l95 = eti95L(relative_prop),
            u95 = eti95U(relative_prop))

## plot

p3 <- rel_inf_distr_agg %>% 
  ggplot() +
  geom_point(aes(x = age, y = mean_rel_ar, col = imd),
             position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(x = age, ymin = l95, ymax = u95, 
                    group = as.factor(imd), col = imd), 
                width = 0.4, position = position_dodge(width = 0.9), 
                alpha= 0.75) +
  theme_bw() +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.6) +
  facet_grid(analysis ~ ., scale = 'free',
             labeller = labeller(analysis = sens_analysis_names)) +
  scale_color_manual(values = imd_quintile_colors) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = 'Age group', y = 'Percentage change in proportion of infections (compared to in base analysis)',
       col = 'IMD quintile'); p3



