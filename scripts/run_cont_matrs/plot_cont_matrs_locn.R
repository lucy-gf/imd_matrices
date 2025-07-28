
## PLOT CONTACT MATRICES ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(ggplot2)
library(viridis, warn.conflicts = FALSE)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","fitted_matrs.rds"),
  file.path("output", "figures", "cont_matrs","fitted_matrs_locn.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN FITTED DATA ####

fitted <- readRDS(.args[1])

#### PLOT ####

agg <- fitted %>% 
  group_by(c_location, p_age_group, c_age_group, p_imd_q, c_imd_q) %>% 
  summarise(med_n = mean(n),
            width = quantile(n, 0.975) - quantile(n, 0.025)) 

agg$p_age_group <- factor(agg$p_age_group,
                          levels = age_labels)
agg$c_age_group <- factor(agg$c_age_group,
                          levels = age_labels)
agg$c_imd_q <- factor(agg$c_imd_q,
                      levels = rev(as.character(1:5)))

#### MEAN CONTACTS ####

plot_locn_cm <- function(locn){
  
  agg %>% 
    filter(c_location == locn) %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = med_n)) +
    theme_bw() + 
    facet_grid(c_imd_q ~ p_imd_q, switch = 'both') +
    scale_fill_viridis() +
    labs(
      x = 'Participant IMD, age group',
      y = 'Contact IMD, age group',
      fill = 'Mean daily\ncontacts'
    ) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    ggtitle(firstup(locn))

  }

plots <- map(
  .x = unique(agg$c_location),
  .f = plot_locn_cm
)

patchwork::wrap_plots(plots, nrow = 2)

#### SAVE PNG ####

ggsave(.args[2], width = 24, height = 22)



