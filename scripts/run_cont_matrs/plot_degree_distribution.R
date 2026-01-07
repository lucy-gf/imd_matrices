
## plot degree distribution ##

library(ggplot2)

.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","participants.rds"),
  "base",
  file.path("output", "figures", "cont_matrs","base","degree_distribution.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

participants <- readRDS(.args[1])
sens_analysis <- .args[2]

# change age groups if needed ##
if(sens_analysis == 'nhs_ages'){
  
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
  
  participants <- participants %>% 
    mutate(p_age_group = cut(p_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F))
  
  indiv_contacts <- indiv_contacts %>% 
    mutate(p_age_group = cut(p_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F),
           c_age_group = cut(c_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F))
  
}

# restrict large_n columns to max. 100, unless in 'no_cap_100' sens analysis
max_large_n <- 100

if(sens_analysis != 'no_cap_100'){
  participants <- participants %>% 
    mutate(across(contains('add_'), ~ case_when(. > max_large_n ~ max_large_n, T ~ .))) %>% 
    mutate(large_n = rowSums(across(contains('add_'))))
}

participants$p_age_group <- factor(participants$p_age_group, levels = age_labels)

participants %>% mutate(n_contacts = n_contacts + large_n) %>% 
  group_by(imd_quintile, p_age_group) %>%
  mutate(n_participants = length(unique(p_id))) %>%
  ungroup() %>%
  group_by(n_contacts, imd_quintile, p_age_group, n_participants) %>%
  summarise(people_w_n_contacts = length(unique(p_id))) %>%
  arrange(desc(n_contacts)) %>%
  group_by(imd_quintile, p_age_group, n_participants) %>%
  mutate(
    cumul_n = cumsum(people_w_n_contacts),
    prob_n = cumul_n / n_participants
  ) %>%
  filter(n_contacts > 0) %>% # for log scale
  ggplot(aes(x = n_contacts, y = prob_n, col = as.factor(imd_quintile), group = as.factor(imd_quintile))) +
  geom_line(lwd = 0.8) +
  # geom_point() + 
  scale_color_manual(values = imd_quintile_colors) + 
  theme_bw() + facet_wrap(p_age_group ~ .) +
  scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000)) +
  scale_y_log10(breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1)) +
  labs(x = "Contact degree",
       y = "Proportion of participants",
       col = 'IMD quintile')

ggsave(.args[3], width = 15, height = 10)

p_gender <- readRDS(file.path('data','reconnect','reconnect_part.rds'))

participants <- participants %>% 
  left_join(p_gender %>% select(p_id, p_gender), by = 'p_id')

bs_gender <- participants %>% mutate(n_contacts = n_contacts + large_n) %>% 
  filter(p_gender %in% c('Female','Male')) %>% 
  group_by(bootstrap_index, imd_quintile, p_age_group, p_gender) %>%
  mutate(n_participants = length(unique(p_id))) %>%
  ungroup() %>%
  group_by(bootstrap_index, n_contacts, imd_quintile, p_gender, p_age_group, n_participants) %>%
  summarise(people_w_n_contacts = length(unique(p_id))) %>%
  arrange(desc(n_contacts)) %>%
  group_by(bootstrap_index, imd_quintile, p_age_group, p_gender, n_participants) %>%
  mutate(
    cumul_n = cumsum(people_w_n_contacts),
    prob_n = cumul_n / n_participants
  ) %>%
  filter(n_contacts > 0) # for log scale

bs_gender_agg <- bs_gender %>% 
  group_by(imd_quintile, n_contacts, p_age_group, p_gender) %>% 
  summarise(prob_n = mean(prob_n))

bs_gender_agg %>% 
  ggplot(aes(x = n_contacts, y = prob_n, 
             col = as.factor(imd_quintile), 
             group = interaction(p_gender, as.factor(imd_quintile)),
             lty = p_gender)) +
  geom_line(lwd = 0.8) +
  # geom_point() + 
  scale_linetype_manual(values = c(1,2)) + 
  scale_color_manual(values = imd_quintile_colors) + 
  theme_bw() + facet_wrap(p_age_group ~ .) +
  scale_x_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000)) +
  scale_y_log10(breaks = c(0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 1)) +
  labs(x = "Contact degree",
       y = "Proportion of participants",
       col = 'IMD quintile',
       lty = 'Gender')

ggsave(gsub('.png','_gender.png',.args[3]), width = 15, height = 10)

