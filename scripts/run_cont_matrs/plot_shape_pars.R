
## PLOT SHAPE PARAMETERS ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(ggplot2)
library(viridis)
library(patchwork)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","fitted_matrs.csv"),
  "base",
  file.path("output", "figures", "cont_matrs","base","shape_pars.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

sens_analysis <- .args[2]

if(sens_analysis == 'nhs_ages'){
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
}

## plotting function ##

plot_shape_pars <- function(
    data,
    filter_locn = T
){
  
  grouping_cols <- c('p_age_group', 'p_imd_q', 'c_age_group', 'c_imd_q')
  if(filter_locn){grouping_cols <- c(grouping_cols, 'c_location')}
  
  data_agg <- data %>% 
    group_by(!!!syms(grouping_cols)) %>% 
    summarise(k = median(k))
  
  data_agg$p_age_group <- factor(data_agg$p_age_group,
                             levels = age_labels)
  data_agg$c_age_group <- factor(data_agg$c_age_group,
                             levels = age_labels)
  data_agg$c_imd_q <- factor(data_agg$c_imd_q,
                         levels = rev(as.character(1:5)))
    
  if(filter_locn){
    
    plot_func <- function(i){
      
      locn <- unique(data_agg$c_location)[i]
      
      data_agg %>%   
        filter(c_location == locn) %>% 
        mutate(k = case_when(k == 0 ~ NA, T ~ k)) %>% 
        ggplot() + 
        geom_tile(aes(p_age_group, c_age_group, fill = k)) +
        scale_fill_viridis(trans = 'pseudo_log',
                           na.value = "grey50") +
        theme_bw() + 
        facet_grid(c_imd_q ~ p_imd_q, switch="both") +
        labs(
          x = 'Participant IMD, age group',
          y = 'Contact IMD, age group',
          fill = 'Shape (k)',
          title = firstup(locn)
        ) + 
        theme(strip.background = element_blank(),
              strip.placement = "outside",
              text = element_text(size = 14),
              axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
      
    }
    
    plots_out <- map(
      .x = 1:5,
      .f = plot_func
    )
    
  }else{
    
    plots_out <- data_agg %>%   
      mutate(k = case_when(k == 0 ~ NA, T ~ k)) %>% 
      ggplot() + 
      geom_tile(aes(p_age_group, c_age_group, fill = k)) +
      scale_fill_viridis(trans = 'pseudo_log',
                         na.value = "grey50") +
      theme_bw() + 
      facet_grid(c_imd_q ~ p_imd_q, switch="both") +
      labs(
        x = 'Participant IMD, age group',
        y = 'Contact IMD, age group',
        fill = 'Shape (k)'
      ) + 
      theme(strip.background = element_blank(),
            strip.placement = "outside",
            text = element_text(size = 14),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
    
  }
  
  plots_out

}


#### READ IN DATA ####

if(.args[2] == 'regional'){
  
  for(reg in c('Greater London','East of England','South East','East Midlands',           
               'Yorkshire and the Humber', 'North West', 'North East','West Midlands','South West')){
    
    fitted <- data.table(suppressWarnings(read_csv(gsub('.csv',paste0('_', reg, '.csv'), .args[1]), show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
    
    plots <- plot_shape_pars(
      data = fitted
    )
    
    patchwork::wrap_plots(plots)
    
    ggsave(.args[3],
           width = 30, height = 20)
    
    ggsave(gsub('.png',paste0('_',reg,'.png'),.args[3]),
           width = 30, height = 20)

  }
  
}else{
  
  fitted <- data.table(suppressWarnings(read_csv(.args[1], show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
  
  plots <- plot_shape_pars(
    data = fitted
  )
  
  patchwork::wrap_plots(plots)
  
  width_value <- ifelse(sens_analysis != 'nhs_ages', 30, 36)
  
  ggsave(.args[3],
         width = width_value, height = 20)
  
}










