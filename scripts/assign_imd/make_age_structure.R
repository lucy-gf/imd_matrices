
## Make IMD- and region-specific age structure

## load packages
library(readr)
library(data.table)
library(tibble)
library(tidyr)
library(dplyr)
library(here)
library(readxl)
library(ggplot2)

here::here()

select <- dplyr::select

## load IMD (2025)

lsoa_imd <- data.table(read_xlsx(here::here('data','imd_25','imd_2025.xlsx'), sheet = 2) %>% 
                         select(starts_with('LSOA'),starts_with('Overall'),starts_with('Index')))
colnames(lsoa_imd) <- c('lsoa21cd','lsoa21nm','imd_rank','imd_decile')
lsoa_imd[, imd_quintile := ceiling(imd_decile/2)]

## load regions of England
# https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2021-to-bua-to-lad-to-region-december-2022-best-fit-lookup-in-ew-v2/about
lsoa_to_reg <- data.table(read_csv(here::here('data','ons','lsoa_to_region.csv'), show_col_types = F))[, c(1,11)]
colnames(lsoa_to_reg) <- c('lsoa21cd','p_engreg')

## load age-specific population in each LSOA
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
# using 2024 population, but can go back to 2019 with these datasets (vary `years` and `sheet_input`)
years <- c('20192022','20222024')[2]
sheet_input <- ifelse(years == '20192022', 8, 7)
lsoa_pop <- data.table(read_xlsx(here::here('data','ons',paste0('sapelsoasyoa', years, '.xlsx')), 
                                 sheet = sheet_input, skip = 3))
colnames(lsoa_pop) <- c('lad21cd','lad21nm','lsoa21cd','lsoa21nm','total',paste0('F_', 0:90), paste0('M_', 0:90))

## merge:

lsoa_dat <- lsoa_pop %>% 
  select(!starts_with('lad')) %>% 
  left_join(lsoa_imd %>% select(lsoa21cd, imd_quintile), by = 'lsoa21cd') %>% 
  left_join(lsoa_to_reg, by = 'lsoa21cd') %>% 
  filter(substr(lsoa21cd, 1, 1) == 'E')

## aggregate:

imd_dat <- data.table(lsoa_dat %>% select(!starts_with('lsoa')))
imd_dat <- imd_dat[, lapply(.SD, sum), by = c('p_engreg','imd_quintile')]

## age groupings:

ages_1 <- seq(0, 75, 5)
ages_1_names <- paste0(ages_1, '-', lead(ages_1) - 1)
ages_1_names[length(ages_1_names)] <- '75+'

ages_2 <- c(0,5,12,18,26,35,50,70,80)
ages_2_names <- paste0(ages_2, '-', lead(ages_2) - 1)
ages_2_names[length(ages_2_names)] <- '80+'

##

imd_dat_long <- imd_dat %>% 
  select(!total) %>% 
  pivot_longer(cols = !c('p_engreg', 'imd_quintile')) %>% 
  mutate(age = as.numeric(substr(name, 3, 4))) %>% 
  group_by(p_engreg, imd_quintile, age) %>% 
  summarise(pop = sum(value))

imd_dat_long %>% group_by(age) %>% 
  summarise(pop = sum(pop)) %>% 
  ggplot() +
  geom_line(aes(x = age, y = pop)) +
  theme_bw() + ylim(c(0,NA))

imd_dat_long %>% 
  group_by(p_engreg) %>%
  mutate(tot_pop = sum(pop)) %>%
  ggplot() +
  geom_line(aes(x = age, y = pop/tot_pop, 
                col = as.factor(imd_quintile), group = imd_quintile),
            lwd = 0.6) +
  facet_wrap(. ~ p_engreg) + 
  scale_x_continuous(breaks = 10*(0:9)) + 
  scale_color_manual(values = imd_quintile_colors) +
  labs(col = 'IMD', x = 'Age', y = 'Proportion of regional population') +
  theme_bw()
ggsave(here::here('output','figures','census','imd_region_age.png'),
       width = 12, height = 8)

imd_dat_long %>% 
  group_by(imd_quintile, age) %>% 
  summarise(pop = sum(pop)) %>% ungroup() %>% 
  mutate(tot_pop = sum(pop)) %>%
  ggplot() +
  geom_line(aes(x = age, y = pop/tot_pop, 
                col = as.factor(imd_quintile), group = imd_quintile),
            lwd = 0.6) +
  ylim(c(0,NA)) + 
  scale_x_continuous(breaks = 10*(0:9)) + 
  scale_color_manual(values = imd_quintile_colors) +
  labs(col = 'IMD', x = 'Age', y = 'Proportion of population') +
  theme_bw()
ggsave(here::here('output','figures','census','imd_age.png'),
       width = 10, height = 7)

imd_age_1 <- imd_dat_long %>% 
  mutate(age_grp = cut(age, c(ages_1, Inf), right = F, labels = ages_1_names)) %>% 
  group_by(p_engreg, imd_quintile, age_grp) %>% 
  summarise(pop = sum(pop))

imd_age_2 <- imd_dat_long %>% 
  mutate(age_grp = cut(age, c(ages_2, Inf), right = F, labels = ages_2_names)) %>% 
  group_by(p_engreg, imd_quintile, age_grp) %>% 
  summarise(pop = sum(pop))


## check sums

sum(imd_age_1$pop)
sum(imd_age_2$pop)

## save 

write_csv(imd_age_1, here::here('data','imd_25','imd_ages_1.csv'))
write_csv(imd_age_2, here::here('data','imd_25','imd_ages_2.csv'))






