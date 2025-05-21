
dt <- data.table(read_csv(here::here('data','ons','data-school-pupils-and-their-characteristics.csv'), show_col_types = F))

dt[, year := as.numeric(substr(time_period,1,4))]

dt %>% 
  filter(!la_name %like% 'City of London|Scilly') %>% 
  ggplot(aes(x = year, y = average_class_size, col = region_name, group = la_name)) + 
  geom_line() + 
  geom_point() +
  facet_grid(classtype ~ .) + theme_bw() + 
  scale_x_continuous(breaks = unique(dt$year)) +
  labs(col = 'Region', x = 'Year') + ylim(c(0,NA))

plot1 <- dt %>% 
  filter(!la_name %like% 'City of London|Scilly') %>% 
  group_by(year, region_name, classtype) %>% 
  summarise(average_class_size = sum(number_of_pupils)/sum(number_of_classes)) %>% 
  ggplot(aes(x = year, y = average_class_size, col = region_name, group = region_name)) + 
  geom_line() + 
  geom_point() +
  facet_grid(classtype ~ ., scales = 'free') + theme_bw() + 
  scale_x_continuous(breaks = unique(dt$year)) +
  labs(col = 'Region', x = 'Year') #+ ylim(c(0,NA))

plot2 <- dt %>% 
  filter(!la_name %like% 'City of London|Scilly') %>% 
  filter(year == 2023) %>% 
  ggplot(aes(x = region_name, y = average_class_size, fill = region_name)) + 
  geom_boxplot() + 
  facet_grid(classtype ~ ., scales = 'free') + theme_bw() + 
  labs(x = 'Region') + 
  theme(legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

plot2 + plot1 + 
  plot_layout(guides = 'collect', axes = 'collect', nrow = 1)



