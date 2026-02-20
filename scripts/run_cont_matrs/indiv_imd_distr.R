
## PLOTTING IMD DISTRIBUTION OF EACH PARTICIPANT ##

part <- readRDS(file.path("output","data","cont_matrs","base","participants.rds"))

part_reconnect <- readRDS(file.path("data","reconnect","reconnect_part.rds"))

# attach gender, day of week
part <- part %>% left_join(part_reconnect %>% 
                             select(p_id, p_gender, day_week, p_income, p_engreg, 
                                    p_broad_age, p_job),
                           by = 'p_id') %>% 
  mutate(total_contacts = n_contacts + large_n) 

na_to_0 <- function(x){
  vals <- which(is.na(x))
  x[vals] <- 0
  x
}

prop_id <- part %>% group_by(p_id, imd_quintile) %>% 
  count() %>% 
  group_by(p_id) %>% 
  mutate(p = 100*prop.table(n)) %>% select(!n) 

prop_id_w <- prop_id %>% 
  pivot_wider(values_from = p,
              names_from = imd_quintile) %>% 
  mutate(across(everything(), na_to_0))

second_func <- function(x){
  v <- max( x[x!=max(x)] )
  k <- which(x == v)
  if(length(k) > 1){k <- k[1]}
  k
}
third_func <- function(x){
  m <- max(x)
  v <- max( x[x != m] )
  k <- which(x == v)
  if(length(k) > 1){k <- k[1]}
  if(length(x[x %notin% c(m, v)])==0){return(0)}
  v3 <- max( x[x %notin% c(m, v)] )
  k3 <- which(x == v3)
  if(length(k3) > 1){k3 <- k3[1]}
  k3
}
fourth_func <- function(x){
  v <- min( x[x!=min(x)] )
  if(length(x[x!=min(x)]) == 1){return(0)}
  k <- which(x == v)
  if(length(k) > 1){k <- k[1]}
  k
}

prop_id_l <- prop_id_w %>% 
  pivot_longer(!c(p_id)) %>% 
  group_by(p_id) %>% 
  mutate(max_imd = which.max(value),
         val_max = max(value),
         second_imd = second_func(value),
         third_imd = third_func(value),
         fourth_imd = fourth_func(value),
         min_imd = which.min(value)) %>% 
  ungroup() %>% 
  mutate(ordered_imd = case_when(
    name == max_imd ~ 1,
    name == second_imd ~ 2,
    name == third_imd ~ 3,
    name == fourth_imd ~ 4,
    name == min_imd ~ 5
  )) %>% 
  arrange(max_imd, desc(val_max)) %>% 
  ungroup()

prop_id_l$p_id <- factor(prop_id_l$p_id,
                         levels = unique(prop_id_l$p_id))

n_in_row <- nrow(prop_id_l)/5
n_id_in_row <- n_in_row/5
n_id_in_imd <- unname(c(table(prop_id_l$max_imd)))/5

prop_id_l <- prop_id_l %>% ungroup() %>% 
  mutate(row = rep(1:5, each = n_in_row),
         id = rep(rep(1:n_id_in_row, each = 5),5), 
         imd_id = c(rep(1:n_id_in_imd[1], each=5),
                    rep(1:n_id_in_imd[2], each=5),
                    rep(1:n_id_in_imd[3], each=5),
                    rep(1:n_id_in_imd[4], each=5),
                    rep(1:n_id_in_imd[5], each=5)))

plot_imd <- function(data, imd){
  
  data_plot <- data %>% 
    filter(max_imd == imd) 
  
  v <- 1:5
  
  data_plot$name <- factor(data_plot$name,
                           levels = c(imd, v[v != imd]))
  
  p <- data_plot %>% 
    ggplot() + 
    geom_bar(aes(x = imd_id, y = value, fill = name),
             position = 'stack', stat='identity', width=1) +
    theme_bw() +
    labs(fill = 'IMD quintile', x = '', y = 'Percentage assigned') + 
    scale_fill_manual(values = imd_quintile_colors) + 
    facet_grid(max_imd ~ .) +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.border=element_blank(),
          panel.spacing.x=unit(0, "lines"))
  
  if(imd != 1){
    p <- p + theme(legend.position = 'none')
  }
  
  if(imd != 3){
    p <- p + labs(y = '')
  }
  
  p
  
}

plots <- map(
  .x = 1:5,
  .f = ~{plot_imd(prop_id_l, .x)}
)

patchwork::wrap_plots(plots) + 
  plot_layout(nrow = 5, guides = 'collect')

ggsave(file.path('output','figures','cont_matrs','base','imd_assignment.png'),
       width = 12, height = 10)

# prop_id_l <- prop_id_l %>%
#   # group_by(p_id) %>% 
#   arrange(p_id, desc(ordered_imd))
# 
# prop_id_l$ordered_imd <- factor(prop_id_l$ordered_imd,
#                                 levels = 1:5)
# 
# prop_id_l <- prop_id_l %>%
#   ungroup() %>% 
#   mutate(row = rep(1:5, each = n_in_row),
#          id = rep(rep(1:n_id_in_row, each = 5),5), 
#          imd_id = c(rep(1:n_id_in_imd[1], each=5),
#                     rep(1:n_id_in_imd[2], each=5),
#                     rep(1:n_id_in_imd[3], each=5),
#                     rep(1:n_id_in_imd[4], each=5),
#                     rep(1:n_id_in_imd[5], each=5)))
# 
# plot_imd <- function(data, imd){
#   
#   data_plot <- data %>% 
#     filter(max_imd == imd) 
#   
#   v <- 1:5
#   
#   data_plot$name <- factor(data_plot$name,
#                            levels = c(imd, v[v != imd]))
#   
#   p <- data_plot %>% 
#     ggplot() + 
#     geom_bar(aes(x = imd_id, y = value, group = ordered_imd, fill = name),
#              position = 'stack', stat='identity', width=1) +
#     theme_bw() +
#     labs(fill = 'IMD quintile', x = '', y = 'Percentage assigned') + 
#     scale_fill_manual(values = imd_quintile_colors) + 
#     facet_grid(max_imd ~ .) +
#     theme(strip.background = element_blank(),
#           strip.text = element_blank(),
#           axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank(),
#           panel.border=element_blank(),
#           panel.spacing.x=unit(0, "lines"))
#   
#   if(imd != 1){
#     p <- p + theme(legend.position = 'none')
#   }
#   
#   if(imd != 3){
#     p <- p + labs(y = '')
#   }
#   
#   p
#   
# }
# 
# plots <- map(
#   .x = 1:5,
#   .f = ~{plot_imd(prop_id_l, .x)}
# )
# 
# patchwork::wrap_plots(plots) + 
#   plot_layout(nrow = 5, guides = 'collect')
# 
# ggsave(file.path('output','figures','cont_matrs','base','imd_assignment.png'),
#        width = 12, height = 10)

prop_id_l %>% 
  ggplot() +
  geom_histogram(aes(x = val_max, fill = as.factor(max_imd)), bins = 80) + 
  theme_bw() + 
  scale_fill_manual(values = imd_quintile_colors) + 
  labs(x = 'Percentage of assignment to most frequent IMD',
       fill = 'IMD quintile')
  
ggsave(file.path('output','figures','cont_matrs','base','imd_assignment_hist.png'),
       width = 12, height = 10)


### IMD x HOUSEHOLD INCOME x NSSEC x ETHNICITY
  
part$p_income <- factor(part$p_income,
                           levels = c("Less than £20,000","£20,000 - £39,999",
                                      "£40,000 - £59,999","£60,000 - £100,000",
                                      "Over £100,000", "Child (Not Applic.)"))

part %>% 
  group_by(p_income, imd_quintile) %>% 
  count() %>% 
  group_by(p_income) %>% 
  mutate(p = prop.table(n)) %>% 
  ggplot() + 
  geom_bar(aes(x = p_income, y = p, fill = as.factor(imd_quintile)),
           stat = 'identity', position = 'stack') +
  scale_fill_manual(values = imd_quintile_colors) + 
  theme_bw() +
  labs(x = '', y = '', fill = 'IMD quintile')
  
part %>% 
  group_by(p_income, p_ethnicity, imd_quintile) %>% 
  count() %>% 
  group_by(p_income, p_ethnicity) %>% 
  mutate(p = prop.table(n)) %>% 
  ggplot() + 
  geom_bar(aes(x = p_income, y = p, fill = as.factor(imd_quintile)),
           stat = 'identity', position = 'stack') +
  scale_fill_manual(values = imd_quintile_colors) + 
  facet_grid(p_ethnicity ~ .) + 
  theme_bw() +
  labs(x = '', y = '', fill = 'IMD quintile')

part %>% 
  group_by(p_income, p_ethnicity, imd_quintile) %>% 
  count() %>% 
  group_by(p_income) %>% 
  mutate(p = prop.table(n)) %>% 
  ggplot() + 
  geom_bar(aes(x = p_income, y = p, fill = as.factor(imd_quintile), alpha = p_ethnicity),
           stat = 'identity', position = 'stack') +
  scale_fill_manual(values = imd_quintile_colors) + 
  scale_alpha_manual(values = c(0.6,0.7,0.8,0.9,1)) +
  theme_bw() +
  labs(x = '', y = '', fill = 'IMD quintile',alpha = 'Ethnic group')


## Black, over £100,000 

filt <- part %>% filter(p_ethnicity == 'Black', p_income == 'Over £100,000')

table((filt %>% select(p_id, p_job) %>% unique())$p_job)

table((filt %>% select(p_id, p_sec_input, p_job) %>% unique())$p_sec_input)

table((filt %>% select(p_id, p_sec_input, p_job) %>% unique())$p_sec_input,
      (filt %>% select(p_id, p_sec_input, p_job) %>% unique())$p_job)

round(100*prop.table(table(filt$p_sec_input, filt$imd_quintile), 1), 2)

table((filt %>% select(p_id, pcd1) %>% unique())$pcd1)

###

prop_id <- filt %>% group_by(p_id, p_sec_input, imd_quintile) %>% 
  count() %>% 
  group_by(p_id, p_sec_input) %>% 
  mutate(p = 100*prop.table(n)) %>% select(!n) 

prop_id_w <- prop_id %>% 
  pivot_wider(values_from = p,
              names_from = imd_quintile) %>% 
  mutate(across(everything(), na_to_0))

prop_id_l <- prop_id_w %>% 
  pivot_longer(!c(p_id, p_sec_input)) %>% 
  group_by(p_id, p_sec_input) %>% 
  mutate(max_imd = which.max(value),
         val_max = max(value)) %>% 
  ungroup() %>% 
  arrange(p_sec_input, max_imd, desc(val_max)) %>% 
  ungroup()

prop_id_l$p_id <- factor(prop_id_l$p_id,
                         levels = unique(prop_id_l$p_id))

prop_id_l %>% 
  ggplot() + 
  geom_bar(aes(x = p_id, y = value, fill = name),
           position = 'stack', stat='identity', width=1) +
  theme_bw() + 
  labs(fill = 'IMD quintile', x = '', y = 'Percentage assigned') + 
  facet_grid(p_sec_input ~ . ) + 
  scale_fill_manual(values = imd_quintile_colors) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border=element_blank(),
        panel.spacing.x=unit(0, "lines"))


## North East, over £100,000

filt <- part %>% filter(p_engreg == 'North East', p_income == 'Over £100,000')

table((filt %>% select(p_id, p_job) %>% unique())$p_job)

table((filt %>% select(p_id, p_sec_input, p_job) %>% unique())$p_sec_input)

table((filt %>% select(p_id, p_sec_input, p_job) %>% unique())$p_sec_input,
      (filt %>% select(p_id, p_sec_input, p_job) %>% unique())$p_job)

table((filt %>% select(p_id, p_sec_input, p_ethnicity) %>% unique())$p_sec_input,
      (filt %>% select(p_id, p_sec_input, p_ethnicity) %>% unique())$p_ethnicity)

round(100*prop.table(table(filt$p_sec_input, filt$imd_quintile), 1), 2)

table((filt %>% select(p_id, pcd1) %>% unique())$pcd1)

round(100*prop.table(table(filt$pcd1, filt$imd_quintile), 1), 2)

round(100*prop.table(table(filt$total_contacts, filt$imd_quintile), 1), 2)

prop_id <- filt %>% group_by(p_id, p_sec_input, imd_quintile, total_contacts, p_ethnicity) %>% 
  count() %>% 
  group_by(p_id, p_sec_input, total_contacts, p_ethnicity) %>% 
  mutate(p = 100*prop.table(n)) %>% select(!n) 

prop_id_w <- prop_id %>% 
  pivot_wider(values_from = p,
              names_from = imd_quintile) %>% 
  mutate(across(everything(), na_to_0))

prop_id_l <- prop_id_w %>% 
  pivot_longer(!c(p_id, p_sec_input, total_contacts, p_ethnicity)) %>% 
  group_by(p_id, p_sec_input, total_contacts, p_ethnicity) %>% 
  mutate(max_imd = which.max(value),
         val_max = max(value)) %>% 
  ungroup() %>% 
  arrange(p_sec_input, max_imd, desc(val_max)) %>% 
  ungroup()

prop_id_l$p_id <- factor(prop_id_l$p_id,
                         levels = unique(prop_id_l$p_id))

ggplot() + 
  geom_bar(data = prop_id_l,
           aes(x = p_id, y = value, fill = name),
           position = 'stack', stat='identity', width=1) +
  geom_text(data = prop_id_l %>% select(p_id, total_contacts) %>% unique(),
            aes(x = p_id, y = 90, label = total_contacts), col = 'white') + 
  theme_bw() + 
  labs(fill = 'IMD quintile', x = '', y = 'Percentage assigned') + 
  facet_grid(p_sec_input ~ .) +
  scale_fill_manual(values = imd_quintile_colors) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        panel.border=element_blank(),
        panel.spacing.x=unit(0, "lines"))


part %>% filter(p_age>17) %>% group_by(imd_quintile, p_age_group, teacher) %>% 
  count() %>% 
  ggplot() + 
  geom_bar(aes(x = teacher, y = n, fill = as.factor(imd_quintile)), 
           position='fill', stat='identity') + theme_bw() + 
  scale_fill_manual(values = imd_quintile_colors) + facet_wrap(p_age_group~.)

v <- part %>% filter(p_age > 17) %>% 
  group_by(bootstrap_index, imd_quintile, p_age_group, teacher) %>% 
  count() %>% 
  group_by(bootstrap_index, p_age_group, teacher) %>% 
  mutate(p = prop.table(n)) %>% arrange(bootstrap_index, teacher, p_age_group, imd_quintile) 

agg <- v %>% 
  group_by(bootstrap_index, p_age_group, teacher) %>% 
  complete(imd_quintile=1:5, fill = list(n=0,p=0)) %>% 
  group_by(imd_quintile, p_age_group, teacher) %>% 
  summarise(median = mean(p), 
            l95 = quantile(p, 0.025),
            u95 = quantile(p, 0.975))

agg %>% 
  ggplot() + 
  geom_ribbon(aes(x = imd_quintile, ymin = l95, ymax=u95, fill = teacher),
              alpha = 0.4) + 
  geom_line(aes(x = imd_quintile, y = median, col = teacher), lwd = 0.8) + 
  theme_bw() + 
  facet_wrap(p_age_group~.) +
  labs(x = 'IMD quintile', y = 'Proportion')








