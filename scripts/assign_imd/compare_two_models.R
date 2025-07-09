
## COMPARE ASSIGNED IMD FOR SURVEY PARTICIPANTS ACROSS TWO MODELS ##

model_1 <- 'pcd1ageethn'
model_2 <- 'pcd1ethnnssec'

output_1 <- readRDS(here::here('output','data','assignment',paste0('connect_prob_',model_1,'.rds')))
output_2 <- readRDS(here::here('output','data','assignment',paste0('connect_prob_',model_2,'.rds')))

comp <- output_1 %>% 
  rename(imd_1 = imd_quintile) %>% 
  left_join(output_2 %>% 
              rename(imd_2 = imd_quintile) %>% 
              select(p_id, bootstrap, imd_2),
            by = c('p_id', 'bootstrap')) %>% 
  group_by(p_id, p_age, p_engreg, p_ethnicity, p_sec_input) %>% 
  summarise(mean_imd_1 = mean(imd_1),
            mean_imd_2 = mean(imd_2)) 

lm <- lm(data = comp, 
         mean_imd_2 ~ 0 + mean_imd_1)
summary(lm)

comp %>% 
  ggplot() +
  geom_point(aes(mean_imd_1, mean_imd_2),
             alpha = 0.3) + 
  geom_line(aes(mean_imd_1, mean_imd_1), 
            lty = 2, col = 'red') +
  theme_bw() + 
  labs(x = model_1, y = model_2)

comp %>% 
  ggplot() +
  geom_point(aes(p_age, mean_imd_1/mean_imd_2),
             alpha = 0.3) +
  theme_bw()

comp %>% 
  ggplot() +
  geom_density(aes(x = mean_imd_1/mean_imd_2),
               fill = 'purple', alpha = 0.2) + 
  theme_bw() + xlim(c(0,NA))

comp %>% 
  ggplot() +
  geom_density(aes(x = mean_imd_1/mean_imd_2, fill = p_ethnicity), 
               alpha = 0.4) + 
  theme_bw() + xlim(c(0,NA))

comp %>% 
  ggplot(aes(x = mean_imd_1/mean_imd_2,
             y = p_ethnicity, fill = p_ethnicity)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  theme_bw()

comp %>% 
  ggplot(aes(x = mean_imd_1/mean_imd_2,
             y = p_engreg, fill = p_engreg)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  theme_bw()

comp %>% 
  ggplot(aes(x = mean_imd_1/mean_imd_2,
             y = p_sec_input, fill = p_sec_input)) +
  geom_violin() +
  geom_boxplot(width=0.1, color="black", alpha=0.2) +
  theme_bw()





