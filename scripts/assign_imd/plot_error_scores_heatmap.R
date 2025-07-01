
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

# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))
# source colors
source(file.path("scripts", "setup", "colors.R"))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "assignment","mse","merged_scores.csv"),
  'mse',
  file.path("output", "figures", "assignment","eval_heatmap_mse.png")
) else commandArgs(trailingOnly = TRUE)

## read in data

error_scores <- read_csv(.args[1], show_col_type = F) %>% 
  filter(imd_quintile != 'imd_quintile') %>%  # filter out the header rows from merging process
  mutate(stat = as.numeric(stat))

error_scores <- data.table(error_scores)

error_scores[, method := case_when(grepl('det_', model) ~ 'det',
                                   grepl('prob_', model) ~ 'prob')]
error_scores[, variable := gsub('p_|_nm', '', variable)]

error_scores$predictors <- ''
for(i in 1:nrow(error_scores)){
  error_scores$predictors[i] <- gsub('det_','',gsub('prob_','',error_scores[i,]$model))
}

## heatmap ##

error_scores_map <- error_scores %>% 
  filter(method != 'det')

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
  
  if(var == 'age_grp'){
    error_scores_filt$category <- factor(error_scores_filt$category,
                                         levels = c('Aged 4 years and under', 'Aged 5 to 9 years',
                                                    'Aged 10 to 14 years', 'Aged 15 to 19 years',
                                                    'Aged 20 to 24 years', 'Aged 25 to 29 years',
                                                    'Aged 30 to 34 years', 'Aged 35 to 39 years',
                                                    'Aged 40 to 44 years', 'Aged 45 to 49 years',
                                                    'Aged 50 to 54 years', 'Aged 55 to 59 years',
                                                    'Aged 60 to 64 years', 'Aged 65 to 69 years',
                                                    'Aged 70 to 74 years', 'Aged 75+'))
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
    facet_grid(predictors~., scales = 'free') + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + 
    geom_label(data = error_scores_filt %>% group_by(model, predictors) %>% summarise(mean_stat = mean(abs(stat)),3),
              aes(label = format_number(mean_stat)),
              x = error_scores_filt$category[k], y = 5, alpha = 0.6) +
    labs(y = 'IMD quintile',
         fill = '', 
         x = '') 
  
  if(pos_neg){
    plot <- plot + 
      scale_fill_gradientn(colors = colorscale, values = scales::rescale(breaks), 
                           breaks = breaks,
                           na.value = "#e5e5e5", limits = c(- max_stat, max_stat),
                           labels = round(abs(breaks), 2), trans = 'pseudo_log')
  }else{
    plot <- plot + 
      scale_fill_distiller(palette = "Greens", direction = 1,
                            limits = c(0, ifelse(merge_color, max(mses_map$square_err), NA))) +
      ggtitle(format_legend(var)) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank()) 
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
                                 ' in each category and IMD quintile, for each predictive model, with total mean ', toupper(.args[2])))

if(merge_color){
  p <- p + plot_layout(guides = 'collect')
}

widths <- c()
merge_color <- F
pos_neg <- T
h_maps_posneg <- map(
  .x = unique(error_scores_map$var),
  .f = heatmap_error_scores
)

h_maps <- c(h_maps, h_maps_posneg)

q <- patchwork::wrap_plots(h_maps, nrow = 2, widths = widths) + 
  plot_annotation(title = paste0(toupper(.args[2]), 
                                 ' in each category and IMD quintile, for each predictive model, ',
                                 '\n with total mean ', toupper(.args[2])),
                  theme = theme(plot.title = element_text(size = 28)))

if(merge_color){
  q <- q + plot_layout(guides = 'collect')
}

q

## save
plot_width <- ifelse(merge_color, 26, 32)
ggsave(.args[3], 
       width = plot_width, height = 28)






