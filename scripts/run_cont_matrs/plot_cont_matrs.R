
## PLOT CONTACT MATRICES ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(ggplot2)
library(viridis, warn.conflicts = FALSE)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","fitted_matrs.rds"),
  file.path("output", "figures", "cont_matrs","fitted_matrs.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN FITTED DATA ####

fitted <- readRDS(.args[1])

#### PLOT ####

agg <- fitted %>% 
  group_by(bootstrap_index, p_age_group, c_age_group, p_imd_q, c_imd_q) %>% 
  summarise(n = sum(n)) %>% 
  group_by(p_age_group, c_age_group, p_imd_q, c_imd_q) %>% 
  summarise(med_n = mean(n),
            width = quantile(n, 0.975) - quantile(n, 0.025)) 

agg$p_age_group <- factor(agg$p_age_group,
                          levels = age_labels)
agg$c_age_group <- factor(agg$c_age_group,
                          levels = age_labels)
agg$c_imd_q <- factor(agg$c_imd_q,
                      levels = rev(as.character(1:5)))

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

ggsave(gsub('.png','_var.png',.args[2]), width = 16, height = 14)

#### IMD DIFFS ####

gen_pop <- fitted %>% 
  group_by(bootstrap_index, p_age_group, c_age_group, p_imd_q, c_imd_q) %>% 
  summarise(n = sum(n)) %>% 
  group_by(p_age_group, c_age_group, c_imd_q) %>% 
  summarise(med_n_gp = mean(n)) 

diff <- agg %>% 
  left_join(gen_pop, by = c('p_age_group','c_age_group','c_imd_q')) %>% 
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
  scale_fill_gradientn(colors = colorscale, values = scales::rescale(breaks), 
                       breaks = breaks,
                       na.value = "#e5e5e5", limits = c(- max_stat, max_stat),
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

ggsave(gsub('.png','_diff.png',.args[2]), width = 14, height = 14)

#### MEAN CONTACTS ####

agg %>% 
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
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#### SAVE PNG ####

ggsave(.args[2], width = 16, height = 14)
