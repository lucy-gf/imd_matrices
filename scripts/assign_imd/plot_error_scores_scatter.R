
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
  file.path("output", "data", "assignment","wis","merged_scores.csv"),
  'wis',
  file.path("output", "figures", "assignment","eval_scatter_wis.png")
) else commandArgs(trailingOnly = TRUE)

## read in data 

error_scores <- suppressWarnings(read_csv(.args[1], show_col_type = F)) %>% 
  filter(imd_quintile != 'imd_quintile',
         !is.na(imd_quintile)) %>%  # filter out the header rows from merging process
  mutate(stat = as.numeric(stat))

error_scores <- data.table(error_scores)

error_scores[, method := case_when(grepl('det_', model) ~ 'det',
                           grepl('prob_', model) ~ 'prob')]
error_scores[, variable := gsub('p_|_nm', '', variable)]

error_scores$predictors <- ''
for(i in 1:nrow(error_scores)){
  error_scores$predictors[i] <- gsub('det_','',gsub('prob_','',error_scores[i,]$model))
}

# remove 'det' method if using WSI/CPRS (not appropriate for point estimates)

if(.args[2] != 'mse'){
  error_scores <- error_scores %>% 
    filter(method != 'det')
}

 
## plot 

nudge <- if(.args[2] == 'mse'){0.003}else{0.005}

plot_df <- error_scores %>% 
  filter(! predictors == 'engreg') %>%
  group_by(model, method, predictors, variable) %>% 
  summarise(mean_stat = mean(stat)) %>%
  group_by(variable) %>% 
  arrange(mean_stat) %>% 
  mutate(rank = 1:n()) 

## remove household tenure and urban/rural
plot_df <- plot_df %>%
  filter(! variable %like% 'engreg|tenure|urban')

plot_df %>% 
  ggplot() +  
  geom_point(aes(predictors, mean_stat, col = predictors, shape = method),
             size = 3) + 
  # geom_text(data = plot_df,
  #           aes(predictors, mean_stat, label = rank),
  #            size = 3, nudge_y = nudge) + 
  theme_bw() + 
  facet_grid(. ~ variable, switch ='x') +
  scale_color_manual(values = model_colors,
                     labels = model_names[names(model_names) %in% plot_df$predictors]) +
  scale_shape_manual(values = method_shapes, labels = method_names) +
  labs(col = 'Predictors',
       shape = 'Method',
       y = paste0('Mean ', toupper(.args[2])), 
       x = '') +
  ylim(c(0,NA)) +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside"
  )

## save
ggsave(.args[3],
       width = 14, height = 6)



 

