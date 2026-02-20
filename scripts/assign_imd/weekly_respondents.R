## RECONNECT RESPONDENTS BY DAY/WEEK AND REGION ##

library(readr)
library(ggplot2)
library(patchwork)

last_monday <- function(dates){
  
  output <- c()
  
  for(date in dates){
    move_back <- 7 - which(weekdays(seq.Date(from = as.Date(date) - 6,
                                             to = as.Date(date),
                                             by = 1)) == 'Monday')
    
    monday <- as.Date(date) - move_back
    
    if(!weekdays(monday) == 'Monday'){warning('Monday move wrong')}
    
    output <- c(output, monday)
  }
  
  as.Date(output)
  
}

part <- readRDS(file.path("data", "reconnect", "reconnect_part.rds"))

p1 <- part %>% filter(!is.na(c_contact_date), !is.na(p_engreg)) %>% 
  group_by(c_contact_date, p_engreg) %>% 
  count() %>% group_by(p_engreg) %>% 
  complete(c_contact_date=seq.Date(as.Date("2024-11-21"), 
                                   as.Date("2025-03-17"), by=1), fill=list(n=0)) %>% 
  ungroup() %>% mutate(week = last_monday(c_contact_date)) %>% 
  group_by(p_engreg, week) %>% summarise(n=sum(n)) %>% 
  ggplot() + 
  geom_line(aes(x=week, y=n, col=p_engreg, group=p_engreg, lty=p_engreg),lwd=0.8) + 
  theme_bw() +
  scale_x_date(date_breaks = "1 week") +
  scale_color_manual(values = colors_p_engreg) +
  scale_linetype_manual(values = c(1,1,1,2,2,2,4,4,4)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x='Week', y='Weekly respondents',col='Region', lty='Region');p1

p2 <- part %>% filter(!is.na(c_contact_date), !is.na(p_engreg)) %>% 
  group_by(c_contact_date, p_engreg) %>% 
  count() %>% group_by(p_engreg) %>% 
  complete(c_contact_date=seq.Date(as.Date("2024-11-21"), 
                                   as.Date("2025-03-17"), by=1), fill=list(n=0)) %>% 
  ungroup() %>% mutate(week = last_monday(c_contact_date)) %>% 
  # group_by(p_engreg, week) %>% summarise(n=sum(n)) %>% 
  ggplot() + 
  geom_line(aes(x=c_contact_date, y=n, col=p_engreg, group=p_engreg,lty=p_engreg),lwd=0.8) + 
  theme_bw() +
  scale_x_date(date_breaks = "1 week") +
  scale_color_manual(values = colors_p_engreg) +
  scale_linetype_manual(values = c(1,1,1,2,2,2,4,4,4)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x='Week', y='Daily respondents',col='Region', lty='Region')

p1 + p2 + plot_layout(nrow=2, guides='collect')

ggsave(file.path('output','figures','assignment','weekly_respondents.png'), width = 13, height = 10)
