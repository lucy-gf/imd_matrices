
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
  file.path("output", "data", "cont_matrs","fitted_matrs_balanced.csv"),
  file.path("data", "census","imd_age.csv"),
  file.path("output", "figures", "cont_matrs","fitted_matrs.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN DATA ####

balanced_matr <- data.table(read_csv(.args[1], show_col_types = F))

fitted <- data.table(read_csv(gsub('_balanced.csv','.csv',.args[1]), show_col_types = F))

imd_age <- data.table(read_csv(.args[2], show_col_types = F))
imd_age$age <- gsub('.{1}$','',gsub('----','-',gsub('-----','',gsub("\\D", "-", imd_age$age))))
imd_age <- imd_age %>% 
  mutate(age = case_when(age == '4' ~ '0-4',
                         age == '75' ~ '75+',
                         T ~ age)) %>% 
  group_by(imd_q) %>% mutate(tot_pop_imd = sum(population)) %>% 
  ungroup() %>% 
  mutate(tot_pop = sum(population),
         prop_imd = population/tot_pop_imd,
         prop = population/tot_pop)
imd_age$age <- factor(imd_age$age, levels = age_labels)

#### SUMMARISE ####

agg <- balanced_matr %>% 
  group_by(p_age_group, c_age_group, p_imd_q, c_imd_q) %>% 
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

## unbalanced 

imd_mix_unb <- fitted %>% 
  group_by(bootstrap_index, p_age_group, p_imd_q, c_age_group, c_imd_q) %>% 
  summarise(n = sum(n)) %>% ungroup() %>% 
  left_join(imd_age %>% rename(p_imd_q = imd_q, p_age_group = age),
            by = c('p_imd_q','p_age_group')) %>% 
  group_by(bootstrap_index, p_imd_q, c_imd_q) %>% 
  summarise(weighted_sum = sum(n*prop_imd)) %>% 
  group_by(p_imd_q, c_imd_q) %>% 
  summarise(weighted_mean = mean(weighted_sum))

imd_mix_unb %>% 
  ggplot() + 
  geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = weighted_mean)) +
  geom_text(aes(x = p_imd_q, y = c_imd_q, label = format_number(weighted_mean), 
                col = (weighted_mean > 2.5))) +
  theme_bw() + 
  scale_fill_viridis(option = 'A', limits = c(0,NA)) +
  scale_color_manual(values = c('white', 'black'), guide = 'none') +
  labs(
    x = 'Participant IMD',
    y = 'Contact IMD',
    fill = 'Mean daily\ncontacts'
  ) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 14)) 

ggsave(gsub('.png','_imd_mix_unbalanced.png',.args[3]), width = 12, height = 10)

## balanced

imd_mix <- balanced_matr %>% 
  left_join(imd_age %>% rename(p_imd_q = imd_q, p_age_group = age),
            by = c('p_imd_q','p_age_group')) %>% 
  group_by(bootstrap_index, p_imd_q, c_imd_q) %>% 
  summarise(weighted_sum = sum(n*prop_imd)) %>% 
  group_by(p_imd_q, c_imd_q) %>% 
  summarise(weighted_mean = mean(weighted_sum))

imd_mix %>% 
  ggplot() + 
  geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = weighted_mean)) +
  geom_text(aes(x = p_imd_q, y = c_imd_q, label = format_number(weighted_mean), 
                col = (weighted_mean > 2.5))) +
  theme_bw() + 
  scale_fill_viridis(option = 'A', limits = c(0,NA)) +
  scale_color_manual(values = c('white', 'black'), guide = 'none') +
  labs(
    x = 'Participant IMD',
    y = 'Contact IMD',
    fill = 'Mean daily\ncontacts'
  ) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 14)) 

ggsave(gsub('.png','_imd_mix.png',.args[3]), width = 12, height = 10)

## balanced without home

balanced_matr_no_home <- balancing_fcn(
  data = fitted %>% filter(c_location != 'home'),
  age_structure = imd_age
)

imd_mix_nh <- balanced_matr_no_home %>% 
  left_join(imd_age %>% rename(p_imd_q = imd_q, p_age_group = age),
            by = c('p_imd_q','p_age_group')) %>% 
  group_by(bootstrap_index, p_imd_q, c_imd_q) %>% 
  summarise(weighted_sum = sum(n*prop_imd)) %>% 
  group_by(p_imd_q, c_imd_q) %>% 
  summarise(weighted_mean = mean(weighted_sum))

imd_mix_nh %>% 
  ggplot() + 
  geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = weighted_mean)) +
  geom_text(aes(x = p_imd_q, y = c_imd_q, label = format_number(weighted_mean), 
                col = (weighted_mean > 2))) +
  theme_bw() + 
  scale_fill_viridis(option = 'A', limits = c(0,NA)) +
  scale_color_manual(values = c('white', 'black'), guide = 'none') +
  labs(
    x = 'Participant IMD',
    y = 'Contact IMD',
    fill = 'Mean daily\ncontacts'
  ) + 
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        text = element_text(size = 14)) 

ggsave(gsub('.png','_imd_mix_no_home.png',.args[3]), width = 12, height = 10)

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

ggsave(gsub('.png','_var.png',.args[3]), width = 16, height = 14)

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

ggsave(gsub('.png','_diff.png',.args[3]), width = 14, height = 14)

# ## IMD 1-1 vs IMD5-5
# imd11 <- balanced_matr %>%
#   filter(p_imd_q == 1, c_imd_q == 1) %>% 
#   group_by(p_age_group, c_age_group) %>% 
#   summarise(med_n_11 = mean(n)) 
# imd55 <- balanced_matr %>%
#   filter(p_imd_q == 5, c_imd_q == 5) %>% 
#   group_by(p_age_group, c_age_group) %>% 
#   summarise(med_n_55 = mean(n)) 
# 
# diff <- imd11 %>% 
#   left_join(imd55, by = c('p_age_group','c_age_group')) %>% 
#   mutate(diff = med_n_11 - med_n_55)
# 
# diff <- diff %>% mutate(diff = case_when(p_age_group==c_age_group ~ NA, T ~ diff))
# 
# diff$p_age_group <- factor(diff$p_age_group,
#                            levels = age_labels)
# diff$c_age_group <- factor(diff$c_age_group,
#                            levels = age_labels)
# 
# colorscale <- c('#045a8d', '#bdc9e1','#f1eef6','white','#fef0d9','#fdcc8a','#b30000')
# breaks <- round(unname(sort(c(0, quantile(diff$diff, c(0.75, 0.9, 0.99), na.rm=T), 
#                  -quantile(diff$diff, c(0.75, 0.9, 0.99), na.rm = T)))), 3)
# max_stat <- max(diff$diff)
# 
# diff %>% 
#   ggplot() + 
#   geom_tile(aes(x = p_age_group, y = c_age_group, fill = diff)) +
#   theme_bw() + 
#   # facet_grid(c_imd_q ~ p_imd_q, switch="both") +
#   scale_fill_gradientn(colors = colorscale, 
#                        # values = scales::rescale(breaks),
#                        breaks = breaks,
#                        na.value = "#e5e5e5", 
#                        limits = c(- max_stat, max_stat),
#                        trans = 'pseudo_log') +
#   labs(
#     x = 'Participant age group',
#     y = 'Contact age group',
#     fill = 'Difference'
#   ) + 
#   ggtitle('IMD 1-1 vs. IMD 5-5\nRed = more contacts in IMD 1-1') +
#   theme(strip.background = element_blank(),
#         strip.placement = "outside",
#         text = element_text(size = 14),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
#         legend.position = 'none')

#### MEAN CONTACTS ####

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
ggsave(gsub('.png','_limited.png',.args[3]), width = 16, height = 14)


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

ggsave(.args[3], width = 16, height = 14)









