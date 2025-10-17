
#### EXPLORATORY ####

## Median age and health prevalence by IMD quintile and region of England

#### LSOA DATA ####

LSOAs <- read_xlsx(file.path('data','ons','sapelsoasyoa20192022.xlsx'), sheet=8, skip=3)
# from: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

# data cleaning: removing Wales
LSOAs_E <- LSOAs %>% 
  filter(substr(`LSOA 2021 Code`, 1,1) == 'E') 
  
LSOAs_E_l <- LSOAs_E %>% 
  rename(lsoa21cd = `LSOA 2021 Code`,
         lsoa21nm = `LSOA 2021 Name`) %>% 
  select(starts_with('lsoa21'), starts_with('F'), starts_with('M')) %>% 
  pivot_longer(!c(lsoa21nm, lsoa21cd)) %>% 
  mutate(gender = substr(name, 1, 1),
         age = substr(name, 2, 6)) %>% 
  select(!c(gender, name)) %>% 
  group_by(lsoa21cd, lsoa21nm, age) %>% 
  summarise(pop = sum(value)) %>% 
  mutate(age = as.numeric(age)) %>% 
  arrange(lsoa21cd, age)

LSOAs_E_l <- data.table(LSOAs_E_l)

# loading IMD data

lsoa_imd <- data.table(read_xlsx(here::here('data','ons','File_2_-_IoD2019_Domains_of_Deprivation.xlsx'), sheet = 2) %>% 
                         select(starts_with('LSOA'),starts_with('Overall'),starts_with('Index'))) 
colnames(lsoa_imd) <- c('lsoa21cd','lsoa21nm','imd_rank','imd_decile')
lsoa_imd[, imd_quintile := ceiling(imd_decile/2)]
lsoa_imd[, lsoa21nm := NULL]
         
# attaching IMD data

LSOAs_E_l_i <- LSOAs_E_l[lsoa_imd, on = c('lsoa21cd')]

# loading regional data

lsoa_to_reg <- data.table(read_csv(here::here('data','ons','lsoa_to_region.csv'), show_col_types = F))[, c(1,11)]
colnames(lsoa_to_reg) <- c('lsoa21cd','eng_reg')

# attaching regional data

LSOAs_E_l_i_r <- LSOAs_E_l_i[lsoa_to_reg[lsoa21cd %in% unique(LSOAs_E_l_i$lsoa21cd)], on = c('lsoa21cd')]

#### Median age by IMD ####

median_age <- function(age_vec, pop_vec){
  
  cumvec <- cumsum(pop_vec) - sum(pop_vec)/2
  k <- min(which(cumvec > 0))
  age_vec[k]
  
}

med_age_imd_d <- LSOAs_E_l_i_r %>% 
  group_by(imd_decile, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(imd_decile) %>% 
  summarise(median_age = median_age(age, pop))

med_age_imd_q <- LSOAs_E_l_i_r %>% 
  group_by(imd_quintile, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(imd_quintile) %>% 
  summarise(median_age = median_age(age, pop))

#### Median age by region ####

med_age_region <- LSOAs_E_l_i_r %>% 
  group_by(eng_reg, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(eng_reg) %>% 
  summarise(median_age = median_age(age, pop))

#### Median age by IMD and region ####

med_age_imd_region <- LSOAs_E_l_i_r %>% 
  group_by(eng_reg, imd_quintile, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(eng_reg, imd_quintile) %>% 
  summarise(median_age = median_age(age, pop))

p1 <- med_age_imd_region %>% 
  ggplot() + 
  geom_line(aes(x = imd_quintile, y = median_age, col = eng_reg, group = eng_reg),
            lwd = 0.8) +
  geom_point(aes(x = imd_quintile, y = median_age, col = eng_reg, group = eng_reg),
             size = 3, shape = 1) +
  scale_color_manual(values = eng_reg_colors) +
  theme_bw() +
  ylim(c(30, 50)) +
  theme(text = element_text(size = 13)) +
  labs(x = 'IMD quintile', y = 'Median age', col = 'Region'); p1

#### Prop u18 by IMD ####

prop_u18 <- function(age_vec, pop_vec){
  
  k <- which(age_vec == 18)
  u18 <- sum(pop_vec[1:(k-1)])
  totpop <- sum(pop_vec)
  u18/totpop
  
}

prop_u18_imd_d <- LSOAs_E_l_i_r %>% 
  group_by(imd_decile, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(imd_decile) %>% 
  summarise(prop_u18 = prop_u18(age, pop))

prop_u18_imd_q <- LSOAs_E_l_i_r %>% 
  group_by(imd_quintile, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(imd_quintile) %>% 
  summarise(prop_u18 = prop_u18(age, pop))

#### Prop u18 by region ####

prop_u18_region <- LSOAs_E_l_i_r %>% 
  group_by(eng_reg, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(eng_reg) %>% 
  summarise(prop_u18 = prop_u18(age, pop))

#### Prop u18 by IMD and region ####

prop_u18_imd_region <- LSOAs_E_l_i_r %>% 
  group_by(eng_reg, imd_quintile, age) %>% 
  summarise(pop = sum(pop)) %>% 
  ungroup() %>% 
  group_by(eng_reg, imd_quintile) %>% 
  summarise(prop_u18 = prop_u18(age, pop))

p2 <- prop_u18_imd_region %>% 
  ggplot() + 
  geom_line(aes(x = imd_quintile, y = prop_u18, col = eng_reg, group = eng_reg),
            lwd = 0.8) +
  geom_point(aes(x = imd_quintile, y = prop_u18, col = eng_reg, group = eng_reg),
             size = 3, shape = 1) +
  theme_bw() +
  scale_color_manual(values = eng_reg_colors) +
  ylim(c(0.1, 0.3)) +
  theme(text = element_text(size = 13)) +
  labs(x = 'IMD quintile', y = 'Proportion aged under 18', col = 'Region'); p2

p1 + p2 + plot_layout(guides = 'collect', nrow = 2)

ggsave(file.path('output','figures','census','median_u18_region.png'),
       width = 8, height = 7)

