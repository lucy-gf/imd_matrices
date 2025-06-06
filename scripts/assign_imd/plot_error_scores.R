
## PLOT COMPARISONS OF MSE ACROSS PREDICTOR COMBINATIONS ##

summary_stat <- c('wis','mse')[1]

## read in all data

filenames <- list.files(here::here('output','data','exploratory','assignment',summary_stat), pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)

names <- gsub(here::here('output','data','exploratory','assignment',summary_stat), '', filenames)
names <- gsub('/|.csv|evaluation_','',names)

names(ldf) <- names

## merge

error_scores <- rbindlist(ldf, idcol = 'predictors')
error_scores[, method := case_when(grepl('det_', predictors) ~ 'det',
                           grepl('probab_', predictors) ~ 'probab')]
error_scores[, variable := gsub('p_|_nm', '', variable)]
error_scores[, predictors := gsub('[0-9]+', '', predictors)]
error_scores[, predictors := gsub('det_|probab_|.{1}$', '', predictors)]
error_scores[, predictors := gsub('__', '_', predictors)]

## plot

error_scores %>% 
  filter(method != 'det') %>%
  group_by(predictors, variable) %>% 
  summarise(mean_stat = mean(stat)) %>%
  ggplot() + 
  geom_point(aes(variable, mean_stat, col = predictors),
             position = position_dodge(width = 0.5), size = 3) + 
  theme_bw() + scale_color_brewer(palette = 'Set2') +
  labs(col = 'Predictors',
       y = toupper(summary_stat), 
       x = '')

## save
ggsave(here::here('output','figures','exploratory','assignment',paste0(toupper(summary_stat), '_scatter.png')),
       width = 10, height = 6)

## table

error_scores_best <- error_scores %>% 
  filter(method != 'det') %>% 
  group_by(predictors, variable) %>% 
  summarise(mean_stat = mean(stat)) %>% 
  arrange(variable, mean_stat) %>% 
  group_by(variable) %>% 
  mutate(best_predictor = predictors[1],
         second_best_predictor = predictors[2]) %>% 
  group_by(predictors) %>% 
  mutate(n_best = sum(best_predictor == predictors),
         n_second_best = sum(second_best_predictor == predictors)) %>% 
  arrange(predictors, variable) 

flext <- error_scores_best %>% 
  select(!c(best_predictor, second_best_predictor)) %>% 
  mutate(mean_stat = round(mean_stat, 4)) %>% 
  flextable() %>% 
  bold(i = error_scores_best$best_predictor == error_scores_best$predictors) %>% 
  italic(i = error_scores_best$best_predictor == error_scores_best$predictors |
           error_scores_best$second_best_predictor == error_scores_best$predictors) %>% 
  hline(part = "all") %>% 
  bg(bg = "grey70", part = "header") %>% 
  bg(i = (error_scores_best$predictors %in% c('eng_reg','pcd_age','pcd_age_nssec_hiqual')), 
     bg = 'grey91'); flext

save_as_docx(flext, 
             path = here::here('output','data','exploratory',
                               'assignment',paste0(summary_stat, '_ranked.docx')))


## heatmap ##

error_scores_map <- error_scores %>% 
  filter(method != 'det',
         predictors != 'eng_reg')

heatmap_error_scores <- function(var){
  
  error_scores_filt <- error_scores_map %>% 
    filter(variable == var) 
  
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
  
  error_scores_filt %>% 
    ggplot() + 
    geom_tile(aes(x = category, y = imd_quintile, fill = stat)) + 
    theme_bw() + scale_fill_distiller(palette = "Greens", direction = 1, limits = c(0, ifelse(merge_color, max(mses_map$square_err), NA))) + #c(0,NA)) + 
    facet_grid(predictors~., scales = 'free') + 
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) + 
    geom_label(data = error_scores_filt %>% group_by(predictors) %>% summarise(mse = round(mean(stat),3)),
              aes(label = mse),
              x = error_scores_filt$category[k], y = 5, alpha = 0.6) +
    labs(y = 'IMD quintile',
         fill = '', 
         x = '') + ggtitle(format_legend(var)) 
}

widths <- c()
merge_color <- F
h_maps <- map(
  .x = unique(mses_map$var),
  .f = heatmap_error_scores
)

p <- patchwork::wrap_plots(h_maps, nrow = 1, widths = widths) + 
  plot_annotation(title = paste0(toupper(summary_stat), 
                                 ' in each category and IMD quintile, for each predictive model, ',
                                 'with total mean ', toupper(summary_stat)))

if(merge_color){
  p <- p + plot_layout(guides = 'collect')
}

p

## save
plot_width <- ifelse(merge_color, 26, 32)
ggsave(here::here('output','figures','exploratory','assignment',paste0(toupper(summary_stat), '_heatmaps.png')),
       width = plot_width, height = 14)






