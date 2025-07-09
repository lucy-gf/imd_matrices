
#### MODEL SELECTION ####

# load packages
library(data.table)
library(readr)
library(dplyr, warn.conflicts = FALSE)
library(stats)
suppressPackageStartupMessages(library(car))
library(MASS, warn.conflicts = FALSE)
library(ggplot2)
suppressPackageStartupMessages(library(patchwork))
suppressPackageStartupMessages(library(tidyr))

# set arguments 
.args <- if (interactive()) c(
  file.path("data", "connect", "connect_part.rds"),
  file.path("data", "connect", "connect_contacts.rds"),
  file.path("output", "data", "assignment","wis","merged_scores.csv"),
  'wis',
  file.path("output", "figures", "assignment","model_scores_wis.png")
) else commandArgs(trailingOnly = TRUE)

# source functions
source(file.path("scripts", "assign_imd", "assign_imd_fcns.R"))
# source colors
source(file.path("scripts", "setup", "colors.R"))

# read in connect data 
connect_part <- readRDS(.args[1]) %>% 
  filter(p_country == 'England')

connect_cont <- readRDS(.args[2]) %>% 
  filter(p_id %in% unique(connect_part$p_id))

prop_u18 <- connect_cont %>% 
  group_by(p_id) %>% 
  summarise(n_u18 = sum(c_age < 18))

connect_part <- connect_part %>% 
  left_join(prop_u18, by ='p_id') %>% 
  mutate(n_u18 = case_when(is.na(n_u18) ~ 0, T ~ n_u18)) %>% 
  mutate(total_n_u18 = (n_u18 + add_u18_school + add_u18_work + add_u18_other)) %>% 
  mutate(prop_u18 = (n_u18 + add_u18_school + add_u18_work + add_u18_other)/(n_contacts + large_n)) #%>% 
  # mutate(p_sec_input = case_when(
  #   p_sec_input %in% as.character(1:7) ~ p_sec_input,
  #   T ~ 'NONE'
  # )) # only interested in the influence of NS-SEC 1:7

# read in model scores

scores <- read_csv(.args[3], show_col_types = F) %>% 
  filter(stat != 'stat') %>% 
  mutate(stat = as.numeric(stat))

# run models

# number of contacts

lm_n <- glm.nb(data = connect_part,
               formula = (n_contacts + large_n) ~ age_grp + p_engreg + p_ethnicity + hh_size_nm + hh_tenure_nm + p_hiqual + 
                          p_sec_input + p_urban_rural)

# proportion of contacts u18

lm_u <- glm(data = connect_part,
            formula = cbind(total_n_u18, (n_contacts + large_n - total_n_u18)) ~ age_grp + p_engreg + p_ethnicity + hh_size_nm + hh_tenure_nm + p_hiqual + 
                      p_sec_input + p_urban_rural,
            family = binomial)

#################################################### 

# run anova

anova_n <- data.frame(Anova(lm_n, type = 2)) %>% rename(pr = `Pr..Chisq.`) 
anova_u <- data.frame(Anova(lm_u, type = 2)) %>% rename(pr = `Pr..Chisq.`)

# ensure they align to the same variables
vars_n <- rownames(anova_n) 
vars_u <- rownames(anova_u) 
if(sum(vars_n == vars_u) == length(vars_n)){vars <- vars_n}else{warning('vars_n neq vars_u')}

anova_n <- anova_n %>% mutate(var = vars)
anova_u <- anova_u %>% mutate(var = vars)

# remove region of england 
# (want the regression models to adjust for it, but not interested in matching to it)
remove_vars <- c('p_engreg')
anova_n <- anova_n %>% filter(var %notin% remove_vars)
anova_u <- anova_u %>% filter(var %notin% remove_vars)

# normalize and invert weights - want more important variables to have weights closer to 1
w_n <- anova_n$pr
w_n_n <- -log(w_n)/sum(-log(w_n))
w_u <- anova_u$pr
w_u_n <- -log(w_u)/sum(-log(w_u))

# weighting factor: set α between 0 and 1 depending on relative importance of n contacts vs prop u18
alpha <- 0.75 # can do a sens. analysis to see if this matters (doesn't seem to)

weights <- alpha * w_n_n + (1 - alpha) * w_u_n 
if(!all.equal(sum(weights),c(1))){warning('Sum of weights neq 1')}
w_df <- data.frame(variable = setdiff(vars, remove_vars), 
                   weights_num_contacts = w_n_n, 
                   weights_prop_u18 = w_u_n,
                   weight = weights)

w_df %>% 
  pivot_longer(!variable) %>% 
  ggplot(aes(x = variable, y = value, col = name)) + 
  geom_point(size = 3) + 
  labs(x = 'Variable',
       y = 'Weight',
       col = '') +
  scale_color_brewer(palette = 'Paired') + 
  theme_bw() +
  ylim(c(0,NA)) +
  theme(text = element_text(size = 14))
ggsave(here::here('output','figures','assignment','variable_weights.png'),
       width = 12, height = 6)

# calculate cost for each candidate model

model_scoring <- scores %>% 
  filter(variable %notin% remove_vars) %>% 
  group_by(variable, model) %>% 
  summarise(mean_stat = mean(stat)) %>% 
  left_join(w_df, by = 'variable')

# compute cost 
model_scoring_agg <- model_scoring %>% 
  mutate(cost = mean_stat*weight) %>% 
  group_by(model) %>% 
  summarise(cost = sum(cost)) %>% 
  mutate(method = case_when(grepl('det_', model) ~ 'det',
                            grepl('prob_', model) ~ 'prob'))

model_scoring_agg$predictors <- ''
for(i in 1:nrow(model_scoring_agg)){
  model_scoring_agg$predictors[i] <- gsub('det_','',gsub('prob_','',model_scoring_agg[i,]$model))
}

# remove 'det' method if using WSI/CPRS (not appropriate for point estimates)
if(.args[4] != 'mse'){
  model_scoring_agg <- model_scoring_agg %>% 
    filter(method != 'det')
}

# best model
best_model_index <- which.min(model_scoring_agg$cost)
best_model <- model_scoring_agg[best_model_index, ]$predictors
best_model_simp <- model_names[best_model]

labels <- unlist(unname(lapply(
  sapply(
    lapply(
      gsub('det_','',gsub('prob_','',model_scoring_agg$model)), variables_from_name),
                            paste, collapse = '_'),
                     simp_labels)))

model_scoring_agg$labels <- labels

model_scoring_agg %>%
  ggplot() + 
  geom_point(aes(predictors, cost, col = predictors, shape = method),
             size = 3) + 
  theme_bw() + 
  geom_text(data = model_scoring_agg %>% filter(method != 'det'),
    aes(predictors, cost, 
                label = labels), angle = 90, nudge_y = -0.002, hjust = 'right') + 
  # facet_grid(. ~ variable, switch ='x') +
  scale_color_manual(values = model_colors,
                     labels = model_names) +
  scale_shape_manual(values = method_shapes, labels = method_names) +
  labs(col = 'Predictors',
       shape = 'Method',
       y = paste0('Cost, measured by ', toupper(.args[4])), 
       x = '') +
  ylim(c(0,NA)) +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    strip.background = element_blank(),
    strip.placement = "outside",
    text = element_text(size = 14)
  ) +
  ggtitle(paste0('Model scoring, measured by ', toupper(.args[4]),
                 ', best model: ', best_model_simp,
                 '\nWeights = ', alpha, '*w_n_contacts + ', 1-alpha, '*w_prop_u18'))

ggsave(.args[5], width = 14, height = 10)


model_scoring$predictors <- ''
for(i in 1:nrow(model_scoring)){
  model_scoring$predictors[i] <- gsub('det_','',gsub('prob_','',model_scoring[i,]$model))
}
labels <- unlist(unname(lapply(
  sapply(
    lapply(
      gsub('det_','',gsub('prob_','',model_scoring$model)), variables_from_name),
    paste, collapse = '_'),
  simp_labels)))
model_scoring$labels <- labels

p1 <- model_scoring %>%
  filter(!grepl('det',model)) %>% 
  ggplot() + 
  geom_bar(aes(labels, mean_stat, fill = variable),
           position = 'stack', stat = 'identity') + 
  theme_bw() + 
  scale_fill_manual(values = variable_colors,
                    labels = unlist(unname(lapply(names(variable_colors), simp_labels)))) +
  labs(fill = 'Variable',
       y = 'Scores', 
       x = '') +
  ylim(c(0,NA)) +
  theme(
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    text = element_text(size = 14)
  ) +
  ggtitle(paste0('Model scoring, measured by ', toupper(.args[4]),
                 ', best model: ', best_model_simp,
                 '\nWeights = ', alpha, '*w_n_contacts + ', 1-alpha, '*w_prop_u18'))

p2 <- model_scoring %>%
  filter(!grepl('det',model)) %>% 
  ggplot() + 
  geom_bar(aes(labels, mean_stat*weight, fill = variable),
           position = 'stack', stat = 'identity') + 
  theme_bw() + 
  scale_fill_manual(values = variable_colors,
                    labels = unlist(unname(lapply(names(variable_colors), simp_labels)))) +
  labs(fill = 'Variable',
       y = 'Weighted scores (cost)', 
       x = '') +
  ylim(c(0,NA)) +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) 

p1 + p2 + plot_layout(guides = 'collect', nrow = 2)

ggsave(paste0(gsub('.png', '', .args[5]), '_bar.png'), width = 11, height = 10)







