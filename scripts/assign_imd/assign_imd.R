
# make input for inference
hh_input <- hh_st %>%
  left_join(unique(pcd_imd %>% select(pcd1, lsoa21cd, imd_quintile, urban_rural, eng_reg)),
            by = 'lsoa21cd', relationship = 'many-to-many') %>%
  mutate(population = hh_size_cd*n_obs) %>% # scale up by household size for population sizes
  filter(hh_tenure_cd != -8,
         population != 0)

connect_input <- connect_part %>% 
  mutate(hh_tenure_nm = case_when(
    p_tenure == 'Owned outright' ~ "Owned: Owns outright",
    p_tenure == 'Rented from private landlord or letting agency' ~ "Private rented: Private landlord or letting agency",
    p_tenure == 'Rented from Council (Local Authority)' ~ "Social rented: Rents from council or Local Authority",
    p_tenure %in% c('Owned with a mortgage or loan',
                    'Shared ownership - with a mortgage and paying rent') ~ "Owned: Owns with a mortgage or loan or shared ownership",
    p_tenure == 'Rented from housing association, housing co-operative, charitable trust, or registered social landlord' ~ "Social rented: Other social rented",
    p_tenure %in% c('Living rent free',
                    'Tied accommodation (accommodation provided by employer of a household member)',
                    'Rented from a relative or friend of household member',
                    'Other private rented') ~ "Private rented: Other private rented or lives rent free",
  )) %>% 
  mutate(hh_size_nm = case_when(
    household_members == '0' ~ "1 person in household",
    household_members %in% as.character(1:4) ~ paste0(as.numeric(household_members) + 1, ' people in household'),
    as.numeric(household_members) > 4 ~ "6 or more people in household"
  ))

cat(nrow(connect_input))

connect_input <- connect_input %>% 
  filter(!is.na(hh_tenure_nm),
         !is.na(hh_size_nm),
         pcd1 %in% hh_input$pcd1)

cat(nrow(connect_input))

connect_input_tab <- connect_input %>% 
  group_by(pcd1, hh_tenure_nm, hh_size_nm) %>% 
  summarise(population = n()) 

connect_output <- infer_imd(data_input = connect_input_tab,
                            census_data = hh_input,
                            variables = c('pcd1','hh_size_nm','hh_tenure_nm'),
                            testing = F)

connect_output_l <- data.table(unnest(connect_output, 'imd_samples'))
connect_output_l[, sampled_imd_quintile := rep(1:5, nrow(connect_output_l)/5)]

vars_of_int_census <- c('pcd1','hh_size_nm','hh_tenure_nm','imd_quintile')
variables <- c('pcd1','hh_size_nm','hh_tenure_nm')
probs_out <- hh_input %>% group_by(!!!syms(vars_of_int_census)) %>% 
  summarise(n = sum(population)) %>% 
  group_by(!!!syms(variables)) %>% 
  mutate(n_tot = sum(n)) %>% 
  mutate(probability = n/n_tot) %>% 
  ungroup() 


### doing manually

n_bootstraps <- 100

connect_input <- data.table(connect_input)
connect_input_reps <- data.table()

for(i_row in 1:nrow(connect_input)){
  
  # filter down to probabilities
  dt <- data.table(probs_out)
  for(var in variables){
    dt <- dt[get(var) == unlist(unname(connect_input[i_row, ..var])),]
  }
  
  if(nrow(dt) > 1){
    sampled_imd <- sample(dt$imd_quintile, n_bootstraps, replace = T, prob = dt$probability)
  }else{
    if(nrow(dt) == 1){
      sampled_imd <- rep(dt$imd_quintile, n_bootstraps)
    }else{
      sampled_imd <- rep(0, n_bootstraps)
    }
  }
  
  connect_input_reps <- rbind(connect_input_reps,
                              connect_input[i_row,] %>% slice(rep(1, each = n_bootstraps)) %>% 
                                mutate(bootstrap = 1:n_bootstraps,
                                       imd_quintile = sampled_imd))
  
  if(i_row %% 100 == 0){cat(i_row, ' ', sep = '')}
  
}

connect_output <- connect_input_reps[imd_quintile != 0,]

prop.table(table(connect_output$imd_quintile, connect_output$p_income), margin = 2)

connect_output$p_income <- factor(connect_output$p_income,
                                  levels = c("Less than £20,000","£20,000 - £39,999","£40,000 - £59,999","£60,000 - £100,000", "Over £100,000", "Child (Not Applic.)"))

connect_output %>% 
  group_by(p_income, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = p_income, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors)

connect_output %>% 
  group_by(p_hiqual, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = p_hiqual, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

connect_output %>% 
  group_by(p_age_group, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = p_age_group, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

connect_output %>% 
  group_by(p_ethnicity, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = p_ethnicity, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

connect_output %>% 
  group_by(p_emp_1, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = p_emp_1, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

prop.table(table(connect_output$imd_quintile))

connect_output %>% 
  group_by(hh_size_nm, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = hh_size_nm, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

connect_output %>% 
  group_by(hh_tenure_nm, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = hh_tenure_nm, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

connect_output %>% 
  group_by(p_schltransport, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = p_schltransport, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

connect_output %>% 
  group_by(p_engreg, imd_quintile) %>% 
  count() %>% 
  drop_na() %>% 
  ggplot() + 
  geom_bar(aes(x = p_engreg, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

library(eq5d)

phrase_to_num <- function(string){
  z <- case_when(
    grepl(' no', string) ~ 1,
    grepl(' slight', string) ~ 2,
    grepl(' moderate', string) ~ 3,
    grepl(' severe', string) ~ 4,
    grepl(' extreme|unable', string) ~ 5)
  z
}

connect_output <- connect_output %>% mutate(score_5 = '',
                                            eq5d = 0)
for(id in unique(connect_output$p_id)){
  if(connect_output[p_id == id,]$p_adult_child[1] == 'Adult'){
    
    score_5 <- paste0(
      phrase_to_num(connect_output[p_id == id,]$p_dis_1[1]),
      phrase_to_num(connect_output[p_id == id,]$p_dis_2[1]),
      phrase_to_num(connect_output[p_id == id,]$p_dis_3[1]),
      phrase_to_num(connect_output[p_id == id,]$p_dis_4[1]),
      phrase_to_num(connect_output[p_id == id,]$p_dis_5[1])
    )
    
    score <- eq5d(scores=score_5, country="England", version="5L", type="VT")
    
    connect_output[p_id == id, score_5 := score_5]
    connect_output[p_id == id, eq5d := score]
  }
  if(id %% 100 == 0){cat(id, ' ', sep = '')}
}

connect_output <- connect_output %>% 
  mutate(eq5d_rounded = floor(eq5d*10)/10) %>% 
  mutate(eq5d_rounded = case_when(eq5d_rounded < 0 ~ -0.1,
                                  T ~ eq5d_rounded))

connect_output %>% 
  filter(p_adult_child == 'Adult') %>% 
  group_by(eq5d_rounded, imd_quintile) %>% 
  count() %>% 
  drop_na() %>% 
  ggplot() + 
  geom_bar(aes(x = eq5d_rounded, y = n, fill = as.factor(imd_quintile)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_x_continuous(breaks = seq(-0.1, 1, 0.1))

connect_output %>% 
  filter(p_adult_child == 'Adult',
         eq5d_rounded != 0) %>%
  group_by(eq5d_rounded, imd_quintile) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = as.factor(imd_quintile), y = n, fill = as.factor(eq5d_rounded)),
           position = 'fill', stat = 'identity') + 
  theme_bw() + labs(fill = 'IMD quintile') + 
  scale_fill_viridis(discrete = T) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

connect_output %>% 
  group_by(p_age_group, imd_quintile) %>% 
  summarise(mean = mean(n_contacts + large_n),
            lower = quantile(n_contacts + large_n, 0.025),
            upper = quantile(n_contacts + large_n, 0.975)) %>% 
  ggplot() + 
  # geom_ribbon(aes(x = p_age_group, ymin = lower, ymax = upper, 
  #                 fill = as.factor(imd_quintile), group = as.factor(imd_quintile)),
  #             alpha = 0.3) + 
  geom_line(aes(x = p_age_group, y = mean, 
                col = as.factor(imd_quintile), group = as.factor(imd_quintile)),
            lwd = 0.8) + 
  theme_bw() + labs(color = 'IMD quintile', fill = 'IMD quintile') + 
  scale_color_manual(values = imd_quintile_colors) +
  # scale_fill_manual(values = imd_quintile_colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 


connect_output %>% 
  group_by(imd_quintile, bootstrap) %>% 
  summarise(mean = mean(n_contacts + large_n)) %>% 
  group_by(imd_quintile) %>% 
  summarise(med = median(mean),
            lower = quantile(mean, 0.025),
            upper = quantile(mean, 0.975))












