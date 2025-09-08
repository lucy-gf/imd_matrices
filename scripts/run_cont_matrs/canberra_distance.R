
## canberra distances

library(phm)

agg$c_imd_q <- factor(agg$c_imd_q,
                      levels = (as.character(1:5)))
vec <- agg %>% 
  arrange(p_age_group, c_age_group, p_imd_q, c_imd_q)

canberra_d <- crossing(p_imd_q = 1:5, 
                       c_imd_q = 1:5,
                       p_imd_q_comp = 1:5, 
                       c_imd_q_comp = 1:5,
                       canberra_distance = -1)

for(p in 1:5){
  for(c in 1:5){
    for(p_comp in 1:5){
      for(c_comp in 1:5){
        
        c_d <- canberra((vec %>% filter(p_imd_q == p,c_imd_q == c))$med_n,
                        (vec %>% filter(p_imd_q == p_comp,c_imd_q == c_comp))$med_n)
        
        canberra_d[canberra_d$p_imd_q == p &
                     canberra_d$c_imd_q == c &
                     canberra_d$p_imd_q_comp == p_comp &
                     canberra_d$c_imd_q_comp == c_comp, ]$canberra_distance <- c_d
        
      }}}}

canberra_d <- canberra_d %>% 
  mutate(orig_matrix = paste0(p_imd_q, '_', c_imd_q),
         comp_matrix = paste0(p_imd_q_comp, '_', c_imd_q_comp))

canberra_d$p_imd_q_comp <- factor(canberra_d$p_imd_q_comp, levels = rev(1:5))

canberra_d %>% 
  ggplot() +
  geom_tile(aes(c_imd_q, c_imd_q_comp, fill = canberra_distance)) +
  theme_minimal() +
  facet_grid(p_imd_q_comp ~ p_imd_q,
             switch = 'both') +
  scale_fill_viridis(option = 'A', direction = -1) +
  theme(text = element_text(size = 16)) +
  labs(x = 'Matrix 1', y = 'Matrix 2', fill = 'Canberra distance')

ggsave(file.path("output", "figures", "cont_matrs","fitted_matrs_canberra_distance.png"), 
       width = 16, height = 14, bg = 'white')


