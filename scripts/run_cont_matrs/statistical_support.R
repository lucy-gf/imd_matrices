## HOW MANY PARTICIPANTS IN EACH MATRIX CELL?

library(tidyverse)
library(parallel)

sens_analyses <- c('base','regional')

read_part <- function(SA){readRDS(file.path("output", "data", "cont_matrs",SA,"participants.rds"))}

participants <- map(.x = sens_analyses, .f = read_part)
names(participants) <- sens_analyses

base_support <- participants$base %>% 
  group_by(bootstrap_index, p_age_group, imd_quintile) %>% 
  count() %>% 
  group_by(bootstrap_index, p_age_group) %>% complete(imd_quintile = 1:5, fill = list(n=0)) %>% 
  group_by(p_age_group, imd_quintile) %>% 
  summarise(support = paste0(median(n), ' (', min(n), '-', max(n), ')')) %>% 
  mutate(imd_quintile = paste0('IMD ', imd_quintile)) %>% 
  pivot_wider(names_from = imd_quintile, values_from = support)

regional_support_big <- participants$regional %>% 
  group_by(bootstrap_index, p_engreg, p_age_group, imd_quintile) %>% 
  count() 

regional_support <- CJ(bootstrap_index=unique(participants$regional$bootstrap_index),
                           p_engreg=unique(participants$regional$p_engreg),
                           p_age_group=unique(participants$regional$p_age_group),
                           imd_quintile=unique(participants$regional$imd_quintile)) %>% 
  left_join(regional_support_big, by = c('bootstrap_index','p_engreg','p_age_group','imd_quintile')) %>% 
  mutate(n = case_when(is.na(n) ~ 0, T ~ n)) %>% 
  group_by(p_engreg, p_age_group, imd_quintile) %>% 
  summarise(support = paste0(median(n), ' (', min(n), '-', max(n), ')')) %>% 
  mutate(imd_quintile = paste0('IMD ', imd_quintile)) %>% 
  pivot_wider(names_from = imd_quintile, values_from = support)

age_levels <- paste0(seq(0,75,by=5),
                     c(rep('-',15),'+'), c(seq(4,74,by=5),''))

base_support$p_age_group <- factor(base_support$p_age_group, levels = age_levels)
regional_support$p_age_group <- factor(regional_support$p_age_group, levels = age_levels)

base_support <- base_support %>% 
  arrange(p_age_group) %>% rename(Age = p_age_group)
regional_support <- regional_support %>% 
  arrange(p_engreg, p_age_group) %>% rename(Age = p_age_group,
                                            Region = p_engreg)

write_csv(base_support,
          file.path("output", "data", "cont_matrs", "base","base_support.csv"))

write_csv(regional_support,
          file.path("output", "data", "cont_matrs", "regional","regional_support.csv"))


