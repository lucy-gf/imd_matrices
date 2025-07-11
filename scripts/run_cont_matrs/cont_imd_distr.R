
## INDIVIDUAL CONTACT IMD DISTRIBUTION ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(viridis)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "contact_matrs","indiv_contacts.rds"),
  file.path("output", "data", "contact_matrs","cont_imd_distr.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN INDIVIDUAL CONTACTS #### 

indiv_contacts <- readRDS(.args[1])

#### TURN INTO DISTRIBUTION ####

indiv_contacts_imd_props <- indiv_contacts %>% 
  group_by(p_age_group, c_age_group, c_location, p_imd_q, c_imd_q) %>% 
  summarise(n_imd_p_c = n()) %>% 
  group_by(p_age_group, c_age_group, c_location, p_imd_q) %>% 
  mutate(n_imd_p = sum(n_imd_p_c)) %>% 
  mutate(prop = n_imd_p_c/n_imd_p) %>% 
  select(!c(n_imd_p_c, n_imd_p)) %>% 
  ungroup() %>% 
  complete(p_age_group, c_age_group, c_location, p_imd_q, c_imd_q,
           fill = list(prop = 0))

indiv_contacts_imd_props$p_age_group <- factor(indiv_contacts_imd_props$p_age_group,
                                               levels = rev(age_labels))

plot_imd_proportions <- function(location){
  
  indiv_contacts_imd_props %>% 
    filter(c_location == location) %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = prop, group = p_age_group)) +
    theme_bw() +
    facet_grid(p_age_group ~ c_age_group) +
    scale_fill_viridis(trans = 'pseudo_log') +
    ggtitle(location) + labs(fill = '')
  
}

plots <- map(
  .x = unique(indiv_contacts_imd_props$c_location),
  .f = plot_imd_proportions
  )

patchwork::wrap_plots(plots, nrow = 2,
                      guides = 'collect')

ggsave(file.path("output", "figures", "contact_matrs","cont_imd_distr.png"),
       width = 16, height = 16)


#### SAVE RDS ####

write_rds(indiv_contacts_imd_props, .args[2])

