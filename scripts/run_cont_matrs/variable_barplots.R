
## BARPLOTS OF DISTRIBUTION OF VARIABLES BY IMD ##

# load packages
library(data.table)
library(readr)
library(readxl)
library(ggplot2)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork)

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","participants.rds"),
  file.path("data", "reconnect","reconnect_part.rds"),
  "base",
  file.path("output", "figures", "cont_matrs","base","variable_barplots.png")
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))

sens_analysis <- .args[3]

## read in sampled data
sampled_parts <- readRDS(.args[1])

## add reconnect data
reconnect_parts <- readRDS(.args[2])

participants <- sampled_parts %>% 
  left_join(reconnect_parts %>% 
              select(p_id, p_hiqual, p_income, p_engreg, p_emp_1, p_tenure),
            by = 'p_id')

participants$p_income <- factor(participants$p_income,
                                 levels = c("Less than £20,000","£20,000 - £39,999",
                                            "£40,000 - £59,999","£60,000 - £100,000", 
                                            "Over £100,000", "Child (Not Applic.)"))

participants$p_age_group <- factor(participants$p_age_group,
                                   levels = age_labels)

## barplot plotting function
barplot_function <- function(var){
    
  participants %>% 
    group_by(imd_quintile, !!sym(var)) %>% 
    count() %>% 
    drop_na() %>% 
    ggplot() + 
    geom_bar(aes(x = !!sym(var), y = n, fill = as.factor(imd_quintile)),
             position = 'fill', stat = 'identity') + 
    # geom_hline(yintercept = 0.2, lty = 2, alpha = 0.6) +
    # geom_hline(yintercept = 0.4, lty = 2, alpha = 0.6) +
    # geom_hline(yintercept = 0.6, lty = 2, alpha = 0.6) +
    # geom_hline(yintercept = 0.8, lty = 2, alpha = 0.6) +
    theme_bw() + labs(fill = 'IMD quintile', x = '', y = '') + 
    ggtitle(format_legend(var)) + 
    scale_y_continuous(expand = expansion(c(0.00075,0.00075)),
                       breaks = seq(0,1,by=0.2)) +
    scale_fill_manual(values = imd_quintile_colors) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          axis.ticks = element_line(linewidth = 0.25))
    
}

plots <- map(
  .x = c('p_age_group','p_ethnicity','p_income','p_engreg','p_hiqual','p_emp_1',
         'p_sec_input', 'p_tenure'),
  .f = barplot_function)

patchwork::wrap_plots(plots) + 
  plot_layout(guides = 'collect', nrow=2)

ggsave(.args[4], width = 16, height = 13)



