
## INDIVIDUAL CONTACT IMD DISTRIBUTION ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(ggplot2)
suppressPackageStartupMessages(library(viridis))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","regional_nhs_ages","indiv_contacts.rds"),
  "regional_nhs_ages",
  file.path("output", "data", "cont_matrs","regional_nhs_ages","cont_imd_distr.rds")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN INDIVIDUAL CONTACTS #### 

indiv_contacts <- readRDS(.args[1])

## set sensitivity analysis ##
sens_analysis <- .args[2]

## if NHS age groups, change age groups
if(grepl('nhs_ages',sens_analysis)){
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
  
  indiv_contacts <- indiv_contacts %>% 
    mutate(p_age_group = cut(p_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F),
           c_age_group = cut(c_age,
                             breaks = c(0,age_limits,Inf),
                             labels = age_labels,
                             right = F))
  
}

#### TURN INTO DISTRIBUTION ####

indiv_contacts_imd_props_empirical <- if(grepl('regional',sens_analysis)){
  indiv_contacts %>% 
    group_by(p_engreg, p_age_group, c_age_group, c_location, p_imd_q, c_imd_q) %>% 
    summarise(n_imd_p_c = n()) %>% 
    group_by(p_engreg, p_age_group, c_age_group, c_location, p_imd_q) %>% 
    mutate(n_imd_p = sum(n_imd_p_c)) %>% 
    mutate(prop = n_imd_p_c/n_imd_p) %>% 
    select(!c(n_imd_p_c, n_imd_p)) %>% 
    ungroup() %>% 
    complete(p_engreg, p_age_group, c_age_group, c_location, p_imd_q, c_imd_q,
             fill = list(prop = 0))
}else{
  indiv_contacts %>% 
    group_by(p_age_group, c_age_group, c_location, p_imd_q, c_imd_q) %>% 
    summarise(n_imd_p_c = n()) %>% 
    group_by(p_age_group, c_age_group, c_location, p_imd_q) %>% 
    mutate(n_imd_p = sum(n_imd_p_c)) %>% 
    mutate(prop = n_imd_p_c/n_imd_p) %>% 
    select(!c(n_imd_p_c, n_imd_p)) %>% 
    ungroup() %>% 
    complete(p_age_group, c_age_group, c_location, p_imd_q, c_imd_q,
             fill = list(prop = 0))
}

## Add school distributions

year <- "25" 
imd_year <- ifelse(sens_analysis == 'old_imd', 19, 25) 
age_grouping <- ifelse(grepl('nhs_ages',sens_analysis), 2, 1) 

dfe_distr <- if(grepl('regional',sens_analysis)){
  data.table(read_csv(file.path("output", "data", "cont_matrs","dfe",paste0('imd',imd_year),year,
                                paste0("cm_IMD5_Age",age_grouping,"Region_class.csv")), show_col_types = F)) %>% 
    rename(p_engreg = Region) %>% 
    mutate(p_engreg = case_when(grepl('London', p_engreg) ~ 'Greater London',
                                grepl('Yorkshire', p_engreg) ~ 'Yorkshire and the Humber',
                                T ~ p_engreg))
}else{
  data.table(read_csv(file.path("output", "data", "cont_matrs","dfe",paste0('imd',imd_year),year,
                                paste0("cm_IMD5_Age",age_grouping,"_class.csv")), show_col_types = F)) 
} 

colnames(dfe_distr)[grepl('Agp', colnames(dfe_distr))] <- 'Agp'

dfe_distr <- dfe_distr %>% 
  mutate(p_age_group = paste0(gsub(",.*$", "", gsub('\\[','',Agp)),
                              '-',
                              as.numeric(gsub(".*,\\s*", "", gsub(')','',Agp))) - 1),
         c_age_group = p_age_group) %>% 
  select(!c(Agp, n_attr_tot)) %>% 
  rename(p_imd_q = imd_five,
         c_imd_q = imd_five_c,
         prop = value) %>% 
  mutate(c_location = 'School') %>% 
  mutate(p_age_group = case_when(p_age_group == '18-24' ~ '18-25', T ~ p_age_group),
         c_age_group = case_when(c_age_group == '18-24' ~ '18-25', T ~ c_age_group))

indiv_contacts_imd_props_no_school <- if(grepl('nhs_ages',sens_analysis)){
  
  indiv_contacts_imd_props_empirical %>% 
    filter(! (c_location == 'School' & 
                p_age_group == c_age_group & 
                p_age_group %in% c('0-4','5-11','12-17','18-25'))) 
  
}else{
  
  indiv_contacts_imd_props_empirical %>% 
    filter(! (c_location == 'School' & 
                p_age_group == c_age_group & 
                p_age_group %in% c('0-4','5-9','10-14','15-19'))) 
  
}

indiv_contacts_imd_props <- rbind(indiv_contacts_imd_props_no_school,
                                  dfe_distr)

# check this is the right number of rows

if(!grepl('regional',sens_analysis)){
  if(nrow(indiv_contacts_imd_props) != (n_distinct(indiv_contacts_imd_props_empirical$p_age_group)^2)*
     (n_distinct(indiv_contacts_imd_props_empirical$p_imd_q)^2)*
     (n_distinct(indiv_contacts_imd_props_empirical$c_location))){
    warning("N rows not correct in indiv_contacts_imd_props")
  }
}else{
  if(nrow(indiv_contacts_imd_props) != (n_distinct(indiv_contacts_imd_props_empirical$p_age_group)^2)*
     (n_distinct(indiv_contacts_imd_props_empirical$p_imd_q)^2)*
     (n_distinct(indiv_contacts_imd_props_empirical$p_engreg))*
     (n_distinct(indiv_contacts_imd_props_empirical$c_location))){
    warning("N rows not correct in indiv_contacts_imd_props")
  }
}

## plot

indiv_contacts_imd_props$p_age_group <- factor(indiv_contacts_imd_props$p_age_group,
                                               levels = rev(age_labels))

plot_imd_proportions <- function(filter_value, 
                                 sens_analysis){
  
  filt <- if(grepl('regional',sens_analysis)){
    indiv_contacts_imd_props %>% 
      filter(p_engreg == filter_value) %>% 
      group_by(p_engreg, p_age_group, c_age_group, p_imd_q, c_imd_q) %>% 
      summarise(prop = mean(prop))
  }else{
    indiv_contacts_imd_props %>% 
      filter(c_location == filter_value) 
  }
  
  filt %>% 
    ggplot() + 
    geom_tile(aes(x = p_imd_q, y = c_imd_q, fill = prop, group = p_age_group)) +
    theme_bw() +
    facet_grid(p_age_group ~ c_age_group, 
               switch = 'both') +
    labs(
      x = 'Participant IMD, age group',
      y = 'Contact IMD, age group',
      fill = 'Proportion'
    ) + 
    scale_fill_viridis(trans = 'pseudo_log', limits = c(0,1)) +
    ggtitle(filter_value) +
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 15))
  
}

plots <- map(
  .x = if(grepl('regional',sens_analysis)){unique(indiv_contacts_imd_props$p_engreg)}else{unique(indiv_contacts_imd_props$c_location)},
  .f = ~plot_imd_proportions(.x, sens_analysis)
  )

patchwork::wrap_plots(plots, nrow = ifelse(grepl('regional',sens_analysis),3,2),
                      guides = 'collect')

if(!file.exists(file.path("output", "figures", "cont_matrs", sens_analysis))){dir.create(file.path("output", "figures", "cont_matrs", sens_analysis))}
  
ggsave(file.path("output", "figures", "cont_matrs", sens_analysis,"cont_imd_distr.png"),
       width = 24, height = 24)

#### SAVE RDS #### 

if(!file.exists(file.path("output", "data", "cont_matrs", sens_analysis))){dir.create(file.path("output", "data", "cont_matrs", sens_analysis))}

write_rds(indiv_contacts_imd_props, .args[3])

