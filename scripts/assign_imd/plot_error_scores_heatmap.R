
## PLOT COMPARISONS OF MSE/WSI/CRPS ACROSS PREDICTOR COMBINATIONS ##

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(patchwork)
library(purrr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(ggplot2)
library(scoringutils)

# source colors
source(file.path("scripts", "setup", "colors.R"))
# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","wis","merged_scores.csv"),
  'wis',
  file.path("output", "figures", "assignment","wis_heatmap.png")
) else commandArgs(trailingOnly = TRUE)

## read in data

error_scores <- read_csv(.args[1], show_col_type = F) %>% 
  filter(imd_quintile != 'imd_quintile') %>%  # filter out the header rows from merging process
  mutate(stat = as.numeric(stat))

error_scores <- data.table(error_scores)

error_scores[, method := case_when(grepl('det_', model) ~ 'det',
                                   grepl('prob_', model) ~ 'prob')]
error_scores[, variable := gsub('p_|_nm', '', variable)]

## heatmap ##

error_scores_map <- error_scores %>% 
  filter(method != 'det',
         model != 'engreg')

heatmap_error_scores <- function(var){
  
  error_scores_filt <- error_scores_map %>% 
    filter(variable == var) 
  
  max_stat <- max(error_scores_filt$stat)
  
  if(pos_neg){
    error_scores_filt <- error_scores_filt %>% 
        mutate(stat = case_when(above_below == 'below' ~ -stat,
                                T ~ stat))
  }
  
  # widths of plots (number of categories)
  widths <<- c(widths, n_distinct(error_scores_filt$category))
  
  if(var == 'age_group'){
    error_scores_filt$category <- factor(error_scores_filt$category,
                                 levels = c('0-4','5-9',
                                            '10-14','15-19',
                                            '20-24','25-29',
                                            '30-34','35-39',
                                            '40-44','45-49',
                                            '50-54','55-59',
                                            '60-64','65-69',
                                            '70-74','75+'))
  }
  
  ## where to place MSE label
  # k <- if(var == 'age_group'){4}else{if(var == 'urban_rural'){1}else{2}}
  k <- 2
  
  colorscale <- c('#045a8d', '#bdc9e1','#f1eef6','white','#fef0d9','#fdcc8a','#b30000')
  breaks <- sort(c(0, quantile(error_scores_filt$stat, c(0.85, 0.9, 0.99)), 
                   -quantile(error_scores_filt$stat, c(0.85, 0.9, 0.99))))
  
  plot <- error_scores_filt %>% 
    ggplot() + 
    geom_tile(aes(x = category, y = imd_quintile, fill = stat)) + 
    theme_bw() +
    facet_grid(model~., scales = 'free') + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + 
    geom_label(data = error_scores_filt %>% group_by(model) %>% summarise(mean_stat = round(mean(abs(stat)),3)),
              aes(label = mean_stat),
              x = error_scores_filt$category[k], y = 5, alpha = 0.6) +
    labs(y = 'IMD quintile',
         fill = '', 
         x = '') + ggtitle(format_legend(var)) 
  
  if(pos_neg){
    plot <- plot + 
      scale_fill_gradientn(colors = colorscale, values = scales::rescale(breaks), 
                           breaks = breaks,
                           na.value = "#e5e5e5", limits = c(- max_stat, max_stat),
                           labels = round(abs(breaks), 2), trans = 'pseudo_log')
  }else{
    plot <- plot + 
      scale_fill_distiller(palette = "Greens", direction = 1,
                            limits = c(0, ifelse(merge_color, max(mses_map$square_err), NA))) 
  }
  
  plot
}

widths <- c()
merge_color <- F
pos_neg <- F
h_maps <- map(
  .x = unique(error_scores_map$var),
  .f = heatmap_error_scores
)

p <- patchwork::wrap_plots(h_maps, nrow = 1, widths = widths) + 
  plot_annotation(title = paste0(toupper(.args[2]), 
                                 ' in each category and IMD quintile, for each predictive model, ',
                                 'with total mean ', toupper(.args[2])))

if(merge_color){
  p <- p + plot_layout(guides = 'collect')
}

p

## save
plot_width <- ifelse(merge_color, 26, 32)
ggsave(.args[3], 
       width = plot_width, height = 14)






