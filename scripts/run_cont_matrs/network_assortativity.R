
#### NETWORK ASSORTATIVITY ####

# load packages
library(data.table)
library(readr)
library(readxl)
library(tidyr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(purrr, warn.conflicts = FALSE)
library(patchwork, warn.conflicts = FALSE)
library(RColorBrewer, warn.conflicts = FALSE)
library(ggplot2)
suppressPackageStartupMessages(library(viridis, warn.conflicts = FALSE))
suppressPackageStartupMessages(library(network))
suppressPackageStartupMessages(library(assortnet))
suppressPackageStartupMessages(library(igraph))

# set arguments
.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","fitted_matrs_balanced.csv"),
  "base",
  "unknown"
) else commandArgs(trailingOnly = TRUE)

source(here::here('scripts','run_cont_matrs','cont_matr_fcns.R'))
source(here::here('scripts','setup','colors.R'))

#### READ IN DATA ####

balanced_matr <- data.table(read_csv(.args[1], show_col_types = F))

sens_analysis <- .args[2]

## age distribution 

if(sens_analysis == 'regional'){
  
  imd_age_raw <- data.table(read_csv(file.path("data","imd_25","imd_ages_1.csv"), show_col_types = F))
  
  imd_age <- imd_age_raw %>% 
    mutate(p_engreg = case_when(
      grepl('London',p_engreg) ~ 'Greater London',
      grepl('Yorkshire',p_engreg) ~ 'Yorkshire and the Humber',
      T ~ p_engreg
    ),
    imd_q = imd_quintile,
    population = pop,
    age = age_grp) %>% 
    select(p_engreg, imd_q, age, population) %>% 
    group_by(p_engreg, imd_q, age) %>% 
    summarise(population = sum(population)) %>% ungroup() %>% 
    group_by(p_engreg, imd_q) %>% 
    mutate(tot_pop_imd = sum(population)) %>% 
    ungroup() %>% 
    group_by(p_engreg) %>% mutate(tot_pop = sum(population)) %>% ungroup() %>% 
    mutate(prop_imd = population/tot_pop_imd,
           prop = population/tot_pop)
  
  imd_age$age <- factor(imd_age$age, levels = age_labels)
  
}else{
  
  age_structure_num <- ifelse(sens_analysis != 'nhs_ages', 1, 2)
  
  if(sens_analysis == 'nhs_ages'){
    age_limits <- c(5,12,18,26,35,50,70,80)
    age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
  }
  
  imd_age_raw <- data.table(read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F))
  
  imd_age <- imd_age_raw %>% 
    mutate(
      imd_q = imd_quintile,
      population = pop,
      age = age_grp) %>% 
    select(imd_q, age, population) %>% 
    group_by(imd_q, age) %>% 
    summarise(population = sum(population)) %>% ungroup() %>% 
    group_by(imd_q) %>% 
    mutate(tot_pop_imd = sum(population)) %>% 
    ungroup() %>% 
    mutate(tot_pop = sum(population)) %>% ungroup() %>% 
    mutate(prop_imd = population/tot_pop_imd,
           prop = population/tot_pop)
  
  imd_age$age <- factor(imd_age$age, levels = age_labels)
  imd_age <- imd_age %>% arrange(imd_q, age)
  
}

#### SUMMARISE ####

group_vars <- c('p_age_group', 'c_age_group', 'p_imd_q', 'c_imd_q')
if(sens_analysis == 'regional'){group_vars <- c(group_vars, 'p_engreg')}

agg <- balanced_matr %>% 
  group_by(!!!syms(group_vars)) %>% 
  summarise(med_n = mean(n),
            width = quantile(n, 0.975) - quantile(n, 0.025)) 

agg$p_age_group <- factor(agg$p_age_group,
                          levels = age_labels)
agg$c_age_group <- factor(agg$c_age_group,
                          levels = age_labels)
agg$c_imd_q <- factor(agg$c_imd_q,
                      levels = rev(as.character(1:5)))

agg <- agg %>% 
  arrange(p_imd_q, p_age_group, c_age_group, c_imd_q)

#### AS MATRIX ####

matr <- agg %>% ungroup() %>% 
  mutate(p_var = paste0(p_imd_q, '_', p_age_group),
         c_var = paste0(c_imd_q, '_', c_age_group)) %>% 
  select(p_var, c_var, med_n)
  
var_labels <- paste0(rep(1:5, each=16), '_', rep(age_labels, 5))  
matr$p_var <- factor(matr$p_var, levels = var_labels)
matr$c_var <- factor(matr$c_var, levels = var_labels)
  
matr_w <- matr %>%   
  arrange(p_var, c_var) %>% 
  pivot_wider(names_from = c_var, values_from = med_n) %>% 
  select(!p_var)
  
matr_w_m <- as.matrix(matr_w, nrow = length(var_labels))

# plot :) 
heatmap(matr_w_m, Colv = NA, Rowv = NA, scale="column")

#### PER CAPITA ####
matr_pc <- matr %>% 
  left_join(imd_age %>% mutate(c_var = paste0(imd_q, '_', age)) %>% 
              select(c_var, population), by = 'c_var') %>% 
  mutate(pc1mill = 1e6*med_n/population)

matr_pc$p_var <- factor(matr_pc$p_var, levels = var_labels)
matr_pc$c_var <- factor(matr_pc$c_var, levels = var_labels)

matr_pc_w <- matr_pc %>%   
  arrange(p_var, c_var) %>% 
  select(p_var, c_var, pc1mill) %>% 
  pivot_wider(names_from = c_var, values_from = pc1mill) %>% 
  select(!p_var)

matr_pc_w_m <- as.matrix(matr_pc_w, nrow = length(var_labels))

heatmap(matr_pc_w_m, Colv = NA, Rowv = NA, scale="column")

# heatmap(matr_pc_w_m - t(matr_pc_w_m), Colv = NA, Rowv = NA, scale="column")
## slightly different i think it's because it's the median of 1000 matrices,
## i.e. for each of the 1000 matrices, M = t(M) ?

#### MATRIX -> NETWORK ####

network <- as.network.matrix(matr_w_m, 
                             matrix.type = 'adjacency',
                             directed = F
                             )

plot.network.default(network) 

#### ASSORTATIVITY ####

out <- assortment.discrete(matr_w_m, types=var_labels, weighted = T)

heatmap(out$mixing_matrix, Colv = NA, Rowv = NA, scale="column")


#### USING IGRAPH ####

# using per capita matrix as it must be symmetric to use "undirected" mode
g <- graph_from_adjacency_matrix(matr_pc_w_m,
                                 mode = "undirected",
                                 weighted = TRUE,
                                 diag = FALSE)

strength_vals <- strength(g, weights = E(g)$weight)

assortativity(g, values = strength_vals,
              directed = FALSE)

assortativity_nominal(g, types = V(g),
                      directed = FALSE)

A <- as.matrix(matr_pc_w_m)
x <- strength_vals 

mu <- weighted.mean(x, rowSums(A))

# initialize node contribution
node_contrib <- numeric(length(x))

for (i in 1:length(x)) {
  for (j in 1:length(x)) {
    node_contrib[i] <- node_contrib[i] +
      A[i,j] * (x[i] - mu) * (x[j] - mu)
  }
}

node_contrib <- node_contrib / sum(node_contrib)

plot(node_contrib, type = 'l')

v1 <- strength(g)
v2 <- eigen_centrality(g, weights = E(g)$weight)$vector
v3 <- betweenness(g, weights = 1/E(g)$weight)

plot(v1, type = 'l')
plot(v2, type = 'l')
plot(v3, type = 'l')

strength_df <- data.frame(
  name = var_labels,
  imd = rep(1:5, each=16),
  age = substr(var_labels, 3, 10),
  strength = v1
)
strength_df$age <- factor(strength_df$age, levels = age_labels)

tile_plot <- strength_df %>% 
  ggplot() + 
  geom_tile(aes(x = age, y = as.factor(imd), fill=strength)) + 
  theme_bw() + 
  labs(x = 'Age group', y = 'IMD quintile', fill = 'Strength') + 
  theme(text = element_text(size=14)) + 
  scale_fill_viridis()

line_plot <- strength_df %>% 
  ggplot() + 
  geom_line(aes(x = age, col = as.factor(imd), y = strength, group = as.factor(imd)),
            lwd = 0.8) + 
  theme_bw() + 
  labs(x = 'Age group', col = 'IMD quintile', y = 'Strength') + 
  theme(text = element_text(size=14)) + 
  scale_color_manual(values = imd_quintile_colors)

tile_plot + line_plot + plot_layout(nrow = 2)

## permutation test for statistical significance
assort_obs <- assortativity(g, strength_vals)

perm_vals <- replicate(1000, {
  assortativity(g, sample(strength_vals))
})

p_value <- mean(abs(perm_vals) >= abs(assort_obs))

## attempt 2 ##

g <- graph_from_adjacency_matrix(matr_pc_w_m,
                                 mode = "undirected",
                                 weighted = TRUE,
                                 diag = FALSE)

# Add node attributes
V(g)$age <- rep(1:16, 5) 
V(g)$imd <- rep(1:5, each = 16)

# IMD assortativity
assortativity(g, values = as.factor(V(g)$imd), directed = FALSE)
## = -0.01, which means disassortative mixing (preferentially cross-IMD)??? makes no sense

assortativity(g, values = as.numeric(V(g)$age), directed = FALSE)
## same value as before ? what is the point of values

node_strength <- strength(g, weights = E(g)$weight)
prop_contacts <- node_strength / sum(node_strength)
## apparently this is "What fraction of all mixing is driven by each subgroup?"
plot(prop_contacts, type='l')

prop_df <- data.frame(
  name = var_labels,
  imd = rep(1:5, each=16),
  age = substr(var_labels, 3, 10),
  strength = prop_contacts
)
prop_df$age <- factor(prop_df$age, levels = age_labels)

tile_plot <- prop_df %>% 
  ggplot() + 
  geom_tile(aes(x = age, y = as.factor(imd), fill=strength)) + 
  theme_bw() + 
  labs(x = 'Age group', y = 'IMD quintile', fill = 'Prop') + 
  theme(text = element_text(size=14)) + 
  scale_fill_viridis()

line_plot <- prop_df %>% 
  ggplot() + 
  geom_line(aes(x = age, col = as.factor(imd), y = strength, group = as.factor(imd)),
            lwd = 0.8) + 
  theme_bw() + 
  labs(x = 'Age group', col = 'IMD quintile', y = 'Prop') + 
  theme(text = element_text(size=14)) + 
  scale_color_manual(values = imd_quintile_colors)

tile_plot + line_plot + plot_layout(nrow = 2)

## exactly the same as strength before, but normalised

#### FOR EACH BOOTSTRAP ####

strength_df_1000 <- data.frame()

for(i in 1:1000){
  
  ## filter contact matrix
  
  matr <- balanced_matr %>% 
    filter(bootstrap_index == i) %>% 
    select(!bootstrap_index)
  
  matr$p_age_group <- factor(matr$p_age_group,levels = age_labels)
  matr$c_age_group <- factor(matr$c_age_group,levels = age_labels)
  matr$c_imd_q <- factor(matr$c_imd_q,levels = rev(as.character(1:5)))
  
  matr <- matr %>% 
    arrange(p_imd_q, p_age_group, c_age_group, c_imd_q) %>% 
    ungroup() %>% 
    mutate(p_var = paste0(p_imd_q, '_', p_age_group),
           c_var = paste0(c_imd_q, '_', c_age_group)) %>% 
    select(p_var, c_var, n)
  
  var_labels <- paste0(rep(1:5, each=16), '_', rep(age_labels, 5))  
  matr$p_var <- factor(matr$p_var, levels = var_labels)
  matr$c_var <- factor(matr$c_var, levels = var_labels)
  
  ## per capita
  matr_pc <- matr %>% 
    left_join(imd_age %>% mutate(c_var = paste0(imd_q, '_', age)) %>% 
                select(c_var, population), by = 'c_var') %>% 
    mutate(pc1mill = 1e6*n/population)
  
  matr_pc$p_var <- factor(matr_pc$p_var, levels = var_labels)
  matr_pc$c_var <- factor(matr_pc$c_var, levels = var_labels)
  
  matr_pc_w <- matr_pc %>%   
    arrange(p_var, c_var) %>% 
    select(p_var, c_var, pc1mill) %>% 
    pivot_wider(names_from = c_var, values_from = pc1mill) %>% 
    select(!p_var)
  
  matr_pc_w_m <- as.matrix(matr_pc_w, nrow = length(var_labels))
  
  g <- graph_from_adjacency_matrix(matr_pc_w_m,
                                   mode = "undirected",
                                   weighted = TRUE,
                                   diag = FALSE)
  
  strength_vals <- strength(g, weights = E(g)$weight)

  strength_df <- data.frame(
    bootstrap = i,
    name = var_labels,
    imd = rep(1:5, each=16),
    age = substr(var_labels, 3, 10),
    strength = strength_vals
  )
 
  strength_df_1000 <- rbind(strength_df_1000,
                            strength_df)
  
  if(i %% 50 == 0){cat(i, ', ', sep='')}
  
}

strength_df_1000$age <- factor(strength_df_1000$age, levels = age_labels)

strength_df_agg <- strength_df_1000 %>%
#   group_by(bootstrap) %>% 
#   mutate(tot = sum(strength)) %>% 
#   ungroup() %>% mutate(strength = strength/tot) %>% 
  group_by(imd, age) %>% 
  summarise(m = mean(strength),
            l = quantile(strength, 0.025),
            u = quantile(strength, 0.975))

line_plot <- strength_df_agg %>% 
  ggplot() + 
  geom_ribbon(aes(x = age, fill = as.factor(imd), ymin=l, ymax=u, group = as.factor(imd)),
              alpha = 0.3) + 
  geom_line(aes(x = age, col = as.factor(imd), y = m, group = as.factor(imd)),
            lwd = 1) + 
  theme_bw() + 
  labs(x = 'Age group', col = 'IMD quintile', fill = 'IMD quintile', y = 'Strength') + 
  theme(text = element_text(size=14)) + 
  scale_fill_manual(values = imd_quintile_colors) + 
  scale_color_manual(values = imd_quintile_colors); line_plot







