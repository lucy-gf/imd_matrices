
library(data.table)
library(ggplot2)

# data table of possible x1,...,x5

dt <- CJ(x1 = seq(0,1,0.01),
         x2 = seq(0,1,0.01),
         x3 = seq(0,1,0.01),
         x4 = seq(0,1,0.01))

dt <- dt[x1 + x2 + x3 + x4 <= 1, ]
dt <- dt[x1 >= x2, ]
dt <- dt[x2 >= x3, ]
dt <- dt[x3 >= x4, ]

dt[, x5 := round(1 - x1 - x2 - x3 - x4, 2)]
dt <- dt[x4 >= x5, ]

dt[, probabilistic := (x1^2 + x2^2 + x3^2 + x4^2 + x5^2)]
dt[, deterministic := x1]

ggplot(dt) + 
  geom_point(aes(x = deterministic, y = probabilistic), alpha = 0.3) + 
  geom_line(aes(x = deterministic, y = deterministic), lty = 2) + 
  theme_bw() + 
  labs(x = 'Proportion assigned correctly using deterministic method',
       y = 'Proportion assigned correctly using probabilistic method') +
  xlim(c(0.2, 1)) + ylim(c(0.2, 1)) +
  geom_line(data = dt[, lapply(.SD, mean), by = 'deterministic'], 
            aes(x = deterministic, y = probabilistic), 
            col = 'red', lty = 2, lwd = 1)

ggplot(dt) + 
  geom_point(aes(x = deterministic, y = probabilistic), alpha = 0.3) + 
  geom_line(aes(x = deterministic, y = deterministic), lty = 2) + 
  theme_bw() + 
  labs(x = 'Deterministic method',
       y = 'Probabilistic method') +
  xlim(c(0.2, 1)) + ylim(c(0.2, 1)) + 
  geom_vline(xintercept = 0.445, col = 'red', lty = 2) +
  geom_vline(xintercept = 0.488, col = 'darkblue', lty = 2)

# so you want x1 to be as high as possible, and then x2 to be as high as possible
ggplot(dt) + 
  geom_point(aes(x = deterministic, y = probabilistic, col = x2 - (1 - x1))) + 
  geom_line(aes(x = deterministic, y = deterministic), lty = 2) + 
  theme_bw() + 
  labs(x = 'Deterministic method',
       y = 'Probabilistic method') +
  xlim(c(0.2, 1)) + ylim(c(0.2, 1)) + 
  scale_color_viridis()

dt %>% pivot_longer(!contains('x')) %>% 
  ggplot() + 
  geom_density(aes(x = value, fill = name), alpha = 0.5) +
  theme_bw() + labs(fill = 'Method')

round(nrow(dt[probabilistic < 0.4,])/nrow(dt), 2)
round(nrow(dt[probabilistic < 0.4 & deterministic > 0.4,])/nrow(dt), 2)

ggplot(dt[probabilistic > 0.4, ]) + 
  geom_point(aes(x = deterministic, y = probabilistic, col = x2)) + 
  geom_line(aes(x = deterministic, y = deterministic), lty = 2) + 
  theme_bw() + 
  labs(x = 'Deterministic method',
       y = 'Probabilistic method') +
  xlim(c(0.2, 1)) + ylim(c(0.2, 1)) + 
  scale_color_viridis()

h1 <- hiqual_pcd1 %>% filter(population != 0) %>% 
  group_by(pcd1, age_nm, hiqual_nm_short, imd_quintile) %>% 
  summarise(population = sum(population)) %>% 
  group_by(pcd1, age_nm, hiqual_nm_short) %>% 
  mutate(tot_pop = sum(population)) %>% 
  mutate(prop = population/tot_pop) %>% 
  mutate(max_imd = which.max(prop), 
         max_prop = max(prop))
ggplot(h1) + 
  geom_histogram(aes(x = max_prop), bins = 100) + 
  theme_bw()

quantile(h1$max_prop, c(0,0.025,0.25,0.5,0.75,0.975,1))

n1 <- nssec_pcd1 %>% filter(population != 0) %>% 
  group_by(pcd1, age_nm, nssec_cd, imd_quintile) %>% 
  summarise(population = sum(population)) %>% 
  group_by(pcd1, age_nm, nssec_cd) %>% 
  mutate(tot_pop = sum(population)) %>% 
  mutate(prop = population/tot_pop) %>% 
  mutate(max_imd = which.max(prop), 
         max_prop = max(prop))
ggplot(n1) + 
  geom_histogram(aes(x = max_prop), bins = 100) + 
  theme_bw()

quantile(n1$max_prop, c(0,0.025,0.25,0.5,0.75,0.975,1))

pcd1 <- nssec_pcd1 %>% filter(population != 0) %>% 
  group_by(pcd1, imd_quintile) %>% 
  summarise(population = sum(population)) %>% 
  group_by(pcd1) %>% 
  mutate(tot_pop = sum(population)) %>% 
  mutate(prop = population/tot_pop) %>% 
  mutate(max_imd = which.max(prop), 
         max_prop = max(prop))
ggplot(pcd1) + 
  geom_histogram(aes(x = max_prop), bins = 100) + 
  theme_bw()

quantile(pcd1$max_prop, c(0,0.025,0.25,0.5,0.75,0.975,1))

hh1 <- hh_st %>% 
  left_join(unique(pcd_imd %>% select(pcd1, lsoa21cd, imd_quintile, urban_rural, eng_reg)),
            by = 'lsoa21cd', relationship = 'many-to-many') %>% 
  mutate(population = hh_size_cd*n_obs) %>% 
  filter(population != 0) %>% 
  group_by(pcd1, hh_size_nm, hh_tenure_nm, imd_quintile) %>% 
  summarise(population = sum(population)) %>% 
  group_by(pcd1, hh_size_nm, hh_tenure_nm) %>% 
  mutate(tot_pop = sum(population)) %>% 
  mutate(prop = population/tot_pop) %>% 
  mutate(max_imd = which.max(prop), 
         max_prop = max(prop))
ggplot(hh1) + 
  geom_histogram(aes(x = max_prop), bins = 100) + 
  theme_bw()

quantile(hh1$max_prop, c(0,0.025,0.25,0.5,0.75,0.975,1))

quantile(dt[deterministic %in% c(0.48, 0.49),]$probabilistic, c(0,0.025,0.25,0.5,0.75,0.975,1))

dt[deterministic %in% seq(0.4, 0.5, 0.01),] %>% 
  ggplot() + 
  geom_density(aes(x = probabilistic, fill = deterministic, group = deterministic), 
               alpha = 0.5) +
  theme_bw() 


mean_correct_hh <- hh1 %>% 
  pivot_wider(id_cols = c(pcd1, hh_size_nm, hh_tenure_nm, tot_pop, max_imd, max_prop),
              names_from = imd_quintile, values_from = prop) %>% 
  mutate(`1` = case_when(is.na(`1`) ~ 0, T ~ `1`),
         `2` = case_when(is.na(`2`) ~ 0, T ~ `2`),
         `3` = case_when(is.na(`3`) ~ 0, T ~ `3`),
         `4` = case_when(is.na(`4`) ~ 0, T ~ `4`),
         `5` = case_when(is.na(`5`) ~ 0, T ~ `5`)) %>% 
  mutate(probabilistic = `1`^2 + `2`^2 + `3`^2 + `4`^2 + `5`^2) %>% 
  mutate(n_corr_p = probabilistic*tot_pop,
         n_corr_d = max_prop*tot_pop)

sum(mean_correct_hh$n_corr_p)/sum(mean_correct_hh$tot_pop)
sum(mean_correct_hh$n_corr_d)/sum(mean_correct_hh$tot_pop)

mean_correct_nssec <- n1 %>% 
  pivot_wider(id_cols = c(pcd1, age_nm, nssec_cd, tot_pop, max_imd, max_prop),
              names_from = imd_quintile, values_from = prop) %>% 
  mutate(`1` = case_when(is.na(`1`) ~ 0, T ~ `1`),
         `2` = case_when(is.na(`2`) ~ 0, T ~ `2`),
         `3` = case_when(is.na(`3`) ~ 0, T ~ `3`),
         `4` = case_when(is.na(`4`) ~ 0, T ~ `4`),
         `5` = case_when(is.na(`5`) ~ 0, T ~ `5`)) %>% 
  mutate(probabilistic = `1`^2 + `2`^2 + `3`^2 + `4`^2 + `5`^2) %>% 
  mutate(n_corr_p = probabilistic*tot_pop,
         n_corr_d = max_prop*tot_pop)

sum(mean_correct_nssec$n_corr_p)/sum(mean_correct_nssec$tot_pop)
sum(mean_correct_nssec$n_corr_d)/sum(mean_correct_nssec$tot_pop)







