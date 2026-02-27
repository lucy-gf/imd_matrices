
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
  
age_labels_orig <- age_labels
var_labels <- paste0(rep(1:5, each=16), '_', rep(age_labels, 5))  
matr$p_var <- factor(matr$p_var, levels = var_labels)
matr$c_var <- factor(matr$c_var, levels = var_labels)
  
matr_w <- matr %>%   
  arrange(p_var, c_var) %>% 
  pivot_wider(names_from = c_var, values_from = med_n) %>% 
  select(!p_var)
  
matr_w_m <- as.matrix(matr_w, nrow = length(var_labels))

# plot :) 
heatmap(matr_w_m, Colv = NA, Rowv = NA, scale="none")

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

assortativity(g, values = as.factor(V(g)$age), types = V(g)$age, directed = FALSE)
assortativity(g, values = as.factor(V(g)$imd), types = V(g)$imd, directed = FALSE)

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


#### USING ASSORTNET ####

imd_labels <- rep(1:5, each = 16)
age_labels <- rep(1:16, 5)

out_var <- assortment.discrete(matr_pc_w_m, types=var_labels, weighted = T)
out_imd <- assortment.discrete(matr_pc_w_m, types=imd_labels, weighted = T)
out_age <- assortment.discrete(matr_pc_w_m, types=age_labels, weighted = T)

## assortativity values, with mixing 
## defined across different variables
out_var$r # = 0.07886954
out_imd$r # = 0.2215598
out_age$r # = 0.1815582

## edge weight matrices

var_weights <- data.frame(out_var$mixing_matrix) %>% 
  mutate(var1 = rownames(out_var$mixing_matrix)) %>% 
  pivot_longer(!var1) %>% 
  rename(var2 = name) %>% 
  mutate(var2 = gsub('X','',gsub('[.]','-',var2))) %>% 
  mutate(var2 = gsub('75-','75+',var2)) 

var_weights$var1 <- factor(var_weights$var1, levels = c(var_labels,'bi'))
var_weights$var2 <- factor(var_weights$var2, levels = c(var_labels,'ai'))

imd_weights <- data.frame(out_imd$mixing_matrix) %>% 
  mutate(var1 = rownames(out_imd$mixing_matrix)) %>% 
  pivot_longer(!var1) %>% 
  rename(var2 = name) %>% 
  mutate(var2 = gsub('X','',var2))

age_weights <- data.frame(out_age$mixing_matrix) %>% 
  mutate(var1 = rownames(out_age$mixing_matrix)) %>% 
  pivot_longer(!var1) %>% 
  rename(var2 = name) %>% 
  mutate(var2 = gsub('X','',var2))

## plot

var_weights %>% 
  filter(var1 %notin% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>% 
  ggplot() +
  geom_tile(aes(x = var1, y = var2, fill = value)) +
  scale_fill_viridis() + 
  theme_bw() +
  theme(text=element_text(size = 14))

imd_weights %>% 
  filter(var1 %notin% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>% 
  ggplot() +
  geom_tile(aes(x = var1, y = var2, fill = value)) +
  scale_fill_viridis() + 
  theme_bw() +
  theme(text=element_text(size = 14))

age_weights_plot <- age_weights %>% 
  filter(var1 %notin% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>%
  mutate(across(c('var1','var2'), as.numeric)) %>% 
  mutate(var1 = rep(age_labels_orig, each = 16),
         var2 = rep(age_labels_orig, 16)) 
age_weights_plot$var1 <- factor(age_weights_plot$var1, levels = age_labels_orig)
age_weights_plot$var2 <- factor(age_weights_plot$var2, levels = age_labels_orig)

age_weights_plot %>% 
  ggplot() +
  geom_tile(aes(x = var1, y = var2, fill = value)) +
  scale_fill_viridis() + 
  theme_bw() +
  theme(text=element_text(size = 14))

## plot colsums

var_weights_line_plot <- var_weights %>% 
  filter(var1 %in% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>% 
  mutate(imd = imd_labels, age = rep(age_labels_orig, 5))

var_weights_line_plot$age <- factor(var_weights_line_plot$age, levels = age_labels_orig) 
  
var_weights_line_plot %>% 
  ggplot() +
  geom_line(aes(x = age, y = value, group = as.factor(imd), 
                color = as.factor(imd)), lwd = 0.8) +
  theme_bw() + ylim(c(0,NA)) + 
  labs(col = 'IMD') + 
  scale_color_manual(values = imd_quintile_colors) + 
  theme(text=element_text(size = 14))

imd_weights %>% 
  filter(var1 %in% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>% 
  ggplot() +
  geom_line(aes(x = var2, y = value, group = var1), lwd = 0.8) +
  theme_bw() + ylim(c(0,NA)) + 
  labs(x = 'IMD') + 
  theme(text=element_text(size = 14))

age_weights_line_plot <- age_weights %>% 
  filter(var1 %in% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>% 
  mutate(age = age_labels_orig)
age_weights_line_plot$age <- factor(age_weights_line_plot$age, levels = age_labels_orig)

age_weights_line_plot %>% 
  ggplot() +
  geom_line(aes(x = age, y = value, group = var1), lwd = 0.8) +
  theme_bw() + ylim(c(0,NA)) + 
  theme(text=element_text(size = 14))

#### FOR EACH BOOTSTRAP ####

var_weights_1000 <- data.frame(); imd_weights_1000 <- data.frame(); age_weights_1000 <- data.frame() 

for(i in 1:1000){
  
  ## filter contact matrix
  
  matr <- balanced_matr %>% 
    filter(bootstrap_index == i) %>% 
    select(!bootstrap_index)
  
  matr$p_age_group <- factor(matr$p_age_group,levels = age_labels_orig)
  matr$c_age_group <- factor(matr$c_age_group,levels = age_labels_orig)
  matr$c_imd_q <- factor(matr$c_imd_q,levels = rev(as.character(1:5)))
  
  matr <- matr %>% 
    arrange(p_imd_q, p_age_group, c_age_group, c_imd_q) %>% 
    ungroup() %>% 
    mutate(p_var = paste0(p_imd_q, '_', p_age_group),
           c_var = paste0(c_imd_q, '_', c_age_group)) %>% 
    select(p_var, c_var, n)
  
  var_labels <- paste0(rep(1:5, each=16), '_', rep(age_labels_orig, 5))  
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
  
  out_var <- assortment.discrete(matr_pc_w_m, types=var_labels, weighted = T)
  out_imd <- assortment.discrete(matr_pc_w_m, types=imd_labels, weighted = T)
  out_age <- assortment.discrete(matr_pc_w_m, types=age_labels, weighted = T)
  
  ## assortativity values, with mixing 
  ## defined across different variables
  out_var$r # = 0.07886954
  out_imd$r # = 0.2215598
  out_age$r # = 0.1815582
  
  ## edge weight matrices
  
  var_weights <- data.frame(out_var$mixing_matrix) %>% 
    mutate(var1 = rownames(out_var$mixing_matrix)) %>% 
    pivot_longer(!var1) %>% 
    rename(var2 = name) %>% 
    mutate(var2 = gsub('X','',gsub('[.]','-',var2))) %>% 
    mutate(var2 = gsub('75-','75+',var2)) 
  
  var_weights$var1 <- factor(var_weights$var1, levels = c(var_labels,'bi'))
  var_weights$var2 <- factor(var_weights$var2, levels = c(var_labels,'ai'))
  
  imd_weights <- data.frame(out_imd$mixing_matrix) %>% 
    mutate(var1 = rownames(out_imd$mixing_matrix)) %>% 
    pivot_longer(!var1) %>% 
    rename(var2 = name) %>% 
    mutate(var2 = gsub('X','',var2))
  
  age_weights <- data.frame(out_age$mixing_matrix) %>% 
    mutate(var1 = rownames(out_age$mixing_matrix)) %>% 
    pivot_longer(!var1) %>% 
    rename(var2 = name) %>% 
    mutate(var2 = gsub('X','',var2))
  
  var_weights_1000 <- rbind(var_weights_1000,
                            var_weights %>% mutate(bootstrap = i))
  imd_weights_1000 <- rbind(imd_weights_1000,
                            imd_weights %>% mutate(bootstrap = i))
  age_weights_1000 <- rbind(age_weights_1000,
                            age_weights %>% mutate(bootstrap = i))
  
  if(i %% 50 == 0){cat(i, ', ', sep='')}
  
}

## plot colsums

var_weights_line_plot <- var_weights_1000 %>% 
  filter(var1 %in% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>% 
  mutate(imd = rep(imd_labels,1000), age = rep(rep(age_labels_orig, 5),1000)) %>% 
  group_by(imd, age) %>% 
  summarise(m = median(value),
            l = quantile(value, 0.025),
            u = quantile(value, 0.975))

var_weights_line_plot$age <- factor(var_weights_line_plot$age, levels = age_labels_orig) 

var_weights_line_plot %>% 
  ggplot() +
  geom_ribbon(aes(x = age, ymin = l, ymax = u, group = as.factor(imd), 
                fill = as.factor(imd)), alpha = 0.4) +
  geom_line(aes(x = age, y = m, group = as.factor(imd), 
                color = as.factor(imd)), lwd = 0.8) +
  theme_bw() + ylim(c(0,NA)) + 
  labs(col = 'IMD', fill = 'IMD') + 
  scale_color_manual(values = imd_quintile_colors) + 
  scale_fill_manual(values = imd_quintile_colors) + 
  theme(text=element_text(size = 14))

imd_weights_1000 %>% 
  filter(var1 %in% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>% 
  group_by(var2) %>% 
  summarise(m = median(value),
            l = quantile(value, 0.025),
            u = quantile(value, 0.975)) %>% 
  ggplot() +
  geom_ribbon(aes(x = var2, ymin=l, ymax=u, group=1), alpha = 0.4) +
  geom_line(aes(x = var2, y = m, group=1), lwd = 0.8) +
  theme_bw() + ylim(c(0,NA)) + 
  labs(x = 'IMD') + 
  theme(text=element_text(size = 14))

age_weights_line_plot <- age_weights_1000 %>% 
  filter(var1 %in% c('ai','bi'),
         var2 %notin% c('ai','bi')) %>% 
  mutate(age = rep(age_labels_orig,1000)) %>% 
  group_by(age) %>% 
  summarise(m = median(value),
            l = quantile(value, 0.025),
            u = quantile(value, 0.975)) 

age_weights_line_plot$age <- factor(age_weights_line_plot$age, levels = age_labels_orig)

age_weights_line_plot %>% 
  ggplot() +
  geom_ribbon(aes(x = age, ymin=l, ymax=u, group=1), alpha = 0.4) +
  geom_line(aes(x = age, y = m, group = 1), lwd = 0.8) +
  theme_bw() + ylim(c(0,NA)) + 
  theme(text=element_text(size = 14))

#### TESTING AGAINST MATRIX COLSUMS ####

test_df_1000 <- data.frame()

for(i in 1:1000){
  
  ## filter contact matrix
  
  matr <- balanced_matr %>% 
    filter(bootstrap_index == i) %>% 
    select(!bootstrap_index)
  
  matr$p_age_group <- factor(matr$p_age_group,levels = age_labels_orig)
  matr$c_age_group <- factor(matr$c_age_group,levels = age_labels_orig)
  matr$c_imd_q <- factor(matr$c_imd_q,levels = rev(as.character(1:5)))
  
  matr <- matr %>% 
    arrange(p_imd_q, p_age_group, c_age_group, c_imd_q) %>% 
    ungroup() %>% 
    mutate(p_var = paste0(p_imd_q, '_', p_age_group),
           c_var = paste0(c_imd_q, '_', c_age_group)) %>% 
    select(p_var, c_var, n)
  
  var_labels <- paste0(rep(1:5, each=16), '_', rep(age_labels_orig, 5))  
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
  
  values <- unname(colSums(matr_pc_w_m)/sum(colSums(matr_pc_w_m)))
  
  test_df <- data.frame(
    bootstrap = i,
    var = var_labels,
    value = values
    )
  
  test_df_1000 <- rbind(test_df_1000, test_df)
  
  if(i %% 50 == 0){cat(i, ', ', sep='')}
  
}

test_line_plot <- test_df_1000 %>% 
  mutate(imd = rep(imd_labels,1000), age = rep(rep(age_labels_orig, 5),1000)) %>% 
  group_by(imd, age) %>% 
  summarise(m = median(value),
            l = quantile(value, 0.025),
            u = quantile(value, 0.975))

test_line_plot$age <- factor(test_line_plot$age, levels = age_labels_orig) 

test_line_plot %>% 
  ggplot() +
  geom_ribbon(aes(x = age, ymin = l, ymax = u, group = as.factor(imd), 
                  fill = as.factor(imd)), alpha = 0.4) +
  geom_line(aes(x = age, y = m, group = as.factor(imd), 
                color = as.factor(imd)), lwd = 0.8) +
  theme_bw() + ylim(c(0,NA)) + 
  labs(col = 'IMD', fill = 'IMD') + 
  scale_color_manual(values = imd_quintile_colors) + 
  scale_fill_manual(values = imd_quintile_colors) + 
  theme(text=element_text(size = 14))







