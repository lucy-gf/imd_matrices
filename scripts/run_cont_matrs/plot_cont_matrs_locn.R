
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
suppressPackageStartupMessages(library(viridis, warn.conflicts = FALSE))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","fitted_matrs.csv"),
  file.path("output", "figures", "cont_matrs","location","fitted_matrs_locn.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

#### READ IN FITTED DATA ####

fitted <- data.table(suppressWarnings(read_csv(.args[1], show_col_types = F)))[bootstrap_index != 'bootstrap_index',]

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

#### IMD DIFFS ####

gen_pop <- agg %>% 
  group_by(c_location, p_age_group, c_age_group) %>% 
  filter(med_n > 0) %>% 
  summarise(med_n_gp = median(med_n)) 

diff <- agg %>% 
  left_join(gen_pop, by = c('c_location','p_age_group','c_age_group')) %>% 
  mutate(diff = case_when(med_n == 0 ~ 0,
                          T ~ med_n - med_n_gp))

diff$p_age_group <- factor(diff$p_age_group,
                           levels = age_labels)
diff$c_age_group <- factor(diff$c_age_group,
                           levels = age_labels)
diff$c_imd_q <- factor(diff$c_imd_q,
                       levels = rev(as.character(1:5)))

plot_locn_diff <- function(locn){
  
  diff_filt <- diff %>% 
    filter(c_location == locn) 
  
  colorscale <- c('#045a8d', '#bdc9e1','#f1eef6','white','#fef0d9','#fdcc8a','#b30000')
  breaks <- sort(c(0, quantile(diff_filt$diff, c(0.75, 0.9, 0.99)), 
                   -quantile(diff_filt$diff, c(0.75, 0.9, 0.99))))
  max_stat <- max(diff_filt$diff)
  
  diff_filt %>% 
    ggplot() + 
    geom_tile(aes(x = p_age_group, y = c_age_group, fill = diff)) +
    theme_bw() + 
    facet_grid(c_imd_q ~ p_imd_q, switch="both") +
    scale_fill_gradientn(colors = colorscale, 
                         breaks = breaks,
                         na.value = "#e5e5e5", 
                         limits = c(- max_stat, max_stat),
                         trans = 'pseudo_log') +
    labs(
      x = 'Participant IMD, age group',
      y = 'Contact IMD, age group',
      fill = 'Diff. from av.'
    ) + 
    theme(strip.background = element_blank(),
          strip.placement = "outside",
          text = element_text(size = 14),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.position = 'none') +
    ggtitle(firstup(locn))
  
}

diff_plots <- map(
  .x = unique(diff$c_location),
  .f = plot_locn_diff
)

suppressWarnings(patchwork::wrap_plots(diff_plots, nrow = 2))

#### SAVE PNG ####

suppressWarnings(ggsave(gsub('.png','_diff.png',.args[2]), width = 18, height = 18))

#### PLOT MEAN CONTACTS ####

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



