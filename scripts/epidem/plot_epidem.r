# epidemic output figures

suppressPackageStartupMessages(require(bench))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(ggplot2))
suppressPackageStartupMessages(require(ggtext))
suppressPackageStartupMessages(require(gridExtra))
suppressPackageStartupMessages(require(Rcpp))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(data.table))
suppressPackageStartupMessages(require(patchwork))
suppressPackageStartupMessages(require(viridis))
options(dplyr.summarise.inform = FALSE)

.args <- if (interactive()) c(
  file.path("output", "data", "epidem","base","byall.rds"),
  "base",
  file.path('output','figures','epidem','base','attack_rate_bars.png')
) else commandArgs(trailingOnly = TRUE)

if(!file.exists(gsub('/attack_rate_bars.png','',.args[3]))){dir.create(gsub('/attack_rate_bars.png','',.args[3]))}

sens_analysis <- .args[2]

# source colors etc.
source(here::here('scripts','assign_imd','assign_imd_fcns.R'))
source(here::here('scripts','setup','colors.R'))

### Basic setting
source_dir <- "scripts/epidem"
source(here::here(source_dir,'setup.r')) #repo

### Diseases cycle
pset$Disease <- "Influenza"

## Parameters
source(paste0(source_dir,"/parsF_.r"))

## Set base levels for IMD and age
base_imd_arr <- 5
base_age_arr <- '35-39'

# set seed
set.seed(120)

## read files
byw <- readRDS(gsub('all','w',.args[1]))
byaw <- readRDS(gsub('all','aw',.args[1]))
byall <- readRDS(.args[1])

## Figures

ar=1 #aspect ratio

l95_func <- function(x){quantile(x, probs=0.025)}; u95_func <- function(x){quantile(x, probs=0.975)}

## If NOT in regional sensitivity analysis

if(sens_analysis != 'regional'){
  
  ## fig 1 overall
  data1000 <- byw[, c('sim','time','Iw','Uw')][, Infe := 10^3*(Iw + Uw)/sum(Na)][, c('sim','time','Infe')]
  data <- rbind(
    data1000[, lapply(.SD, median), by = 'time'][, meas := 'median'],
    data1000[, lapply(.SD, l95_func), by = 'time'][, meas := 'l95'],
    data1000[, lapply(.SD, u95_func), by = 'time'][, meas := 'u95']
  ); data[, sim := NULL]
  data <- dcast.data.table(data, time ~ meas, value.var = 'Infe')
  
  p1 <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95), alpha=0.25, fill = 'darkgreen')  +
    geom_line(aes(y = median), lwd=0.8, col = 'darkgreen')  +
    theme_bw() +
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day"); p1
  
  ## fig 1b cumulative
  data1000 <- byw[, c('sim','time','Iw','Uw')][, Infe := 10^3*(Iw + Uw)/sum(Na)][, c('sim','time','Infe')]
  data1000cum <- data1000[, lapply(.SD, cumsum), by = c('sim')][, time := data1000$time]
  data <- rbind(
    data1000cum[, lapply(.SD, median), by = 'time'][, meas := 'median'],
    data1000cum[, lapply(.SD, l95_func), by = 'time'][, meas := 'l95'],
    data1000cum[, lapply(.SD, u95_func), by = 'time'][, meas := 'u95']
  ); data[, sim := NULL]
  data <- dcast.data.table(data, time ~ meas, value.var = 'Infe')
  
  p1b <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95), alpha=0.25, fill = 'darkgreen')  +
    geom_line(aes(y = median), lwd=0.8, col = 'darkgreen')  +
    theme_bw() +
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Cumulative infections per 1000 population", x = "Day"); p1b
  
  ## fig 2 by imd
  data_imd1000 <- byw[, c('sim','time','IUw_s1','IUw_s2','IUw_s3','IUw_s4','IUw_s5')]
  for(a in 1:5){data_imd1000[, (paste0('IUw_s', a))] <- 1e3*data_imd1000[, get(paste0('IUw_s', a))]/Ns[a]}
  data <- rbind(
    data_imd1000[, lapply(.SD, median), by = 'time'][, meas := 'median'],
    data_imd1000[, lapply(.SD, l95_func), by = 'time'][, meas := 'l95'],
    data_imd1000[, lapply(.SD, u95_func), by = 'time'][, meas := 'u95']
  ); data[, sim := NULL]
  data <- melt.data.table(data, id.vars = c('time','meas'))
  data[, imd := substr(variable,6,6)]
  data <- dcast.data.table(data, time + imd ~ meas, value.var = 'value')
  
  p2 <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = imd), alpha=0.25)  +
    geom_line(aes(y = median, col = imd), lwd=0.8)  +
    theme_bw() +
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "IMD", fill = 'IMD'); p2
  
  p2facet <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = imd), alpha=0.25)  +
    geom_line(aes(y = median, col = imd), lwd=0.8)  +
    theme_bw() +
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) + 
    facet_grid(.~imd) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "IMD", fill = 'IMD'); p2facet
  
  ## fig 2b by imd
  data_imd1000 <- byw[, c('sim','time','IUw_s1','IUw_s2','IUw_s3','IUw_s4','IUw_s5')]
  for(a in 1:5){data_imd1000[, (paste0('IUw_s', a))] <- 1e3*data_imd1000[, get(paste0('IUw_s', a))]/Ns[a]}
  data_imd1000cum <- data_imd1000[, lapply(.SD, cumsum), by = c('sim')][, time := data_imd1000$time]
  data <- rbind(
    data_imd1000cum[, lapply(.SD, median), by = 'time'][, meas := 'median'],
    data_imd1000cum[, lapply(.SD, l95_func), by = 'time'][, meas := 'l95'],
    data_imd1000cum[, lapply(.SD, u95_func), by = 'time'][, meas := 'u95']
  ); data[, sim := NULL]
  data <- melt.data.table(data, id.vars = c('time','meas'))
  data[, imd := substr(variable,6,6)]
  data <- dcast.data.table(data, time + imd ~ meas, value.var = 'value')
  
  p2b <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = imd), alpha=0.25)  +
    geom_line(aes(y = median, col = imd), lwd=0.8)  +
    theme_bw() +
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Cumulative infections per 1000 population", x = "Day", color = "IMD", fill = 'IMD'); p2b
  
  imd_final_size <- data %>% 
    filter(time == max(time)) %>% 
    ggplot(aes(x=imd)) + 
    geom_errorbar(aes(ymin = l95, ymax = u95, col = imd), width = 0.2)  +
    geom_point(aes(y = median, col = imd), size = 3)  +
    theme_bw() +
    scale_color_manual(values = imd_quintile_colors) + 
    # scale_fill_manual(values = imd_quintile_colors) + 
    ylim(c(0,800)) + 
    theme(text=element_text(size=14),
          legend.position = 'none',
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Final size per 1000 population", x = "IMD Quintile", color = "IMD", fill = 'IMD'); imd_final_size
  
  p2bfacet <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = imd), alpha=0.25)  +
    geom_line(aes(y = median, col = imd), lwd=0.8)  +
    theme_bw() +
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) + 
    facet_grid(.~imd) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Cumulative infections per 1000 population", x = "Day", color = "IMD", fill = 'IMD'); p2bfacet
  
  data_imd1000 <- byw[, c('sim','time','IUw_s1','IUw_s2','IUw_s3','IUw_s4','IUw_s5')]
  for(a in 1:5){data_imd1000[, (paste0('IUw_s', a))] <- 1e3*data_imd1000[, get(paste0('IUw_s', a))]/Ns[a]}
  data_imd1000sum <- data_imd1000[, lapply(.SD, sum), by = c('sim')][, time := NULL]
  X <- base_imd_arr
  data_melt <- melt.data.table(data_imd1000sum, id.vars = c('sim'))
  data_melt[, imd := substr(variable, 6, 6)][, variable := NULL]
  data_imdX <- data_melt[imd == X][, imd_X_value := value]
  data <- data_melt[data_imdX[, c('sim','imd_X_value')], on = c('sim')]
  data[, arr := value/imd_X_value]
  data_min <- data[, c('imd','arr')]
  data_agg <- rbind(
    data_min[, lapply(.SD, median), by = c('imd')][, meas := 'median'],
    data_min[, lapply(.SD, l95_func), by = c('imd')][, meas := 'l95'],
    data_min[, lapply(.SD, u95_func), by = c('imd')][, meas := 'u95']
  )
  data <- dcast.data.table(data_agg, imd ~ meas, value.var = 'arr')
  
  imd_final_size_arr <- data %>% 
    ggplot(aes(x=imd)) + 
    geom_hline(col = 1, lty = 2, alpha = 0.3, yintercept = 1) +
    geom_errorbar(aes(ymin = l95, ymax = u95, col = imd), width = 0.2)  +
    geom_point(aes(y = median, col = imd), size = 3)  +
    theme_bw() +
    ylim(c(0.8, 1.2)) +
    scale_color_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=14),
          legend.position = 'none',
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Relative final size", x = "IMD Quintile", color = "IMD", fill = 'IMD'); imd_final_size_arr
  
  imd_final_size + imd_final_size_arr + plot_layout(nrow = 1) 
  
  ## save
  ggsave(here::here('output','figures','epidem',sens_analysis,'final_size_imd.png'), dpi=600, 
         device = "png", width = 12, height = 5)
  
  ## fig 3 by age
  data_age1000 <- byaw[, c('sim',paste0('IUw_a', 1:16))]
  for(a in 1:16){data_age1000[, (paste0('IUw_a', a))] <- 1e3*data_age1000[, get(paste0('IUw_a', a))]/Na[a]} 
  data_age1000[, time := rep(min(byw$time):max(byw$time), max(data_age1000$sim))]
  data <- rbind(
    data_age1000[, lapply(.SD, median), by = 'time'][, meas := 'median'],
    data_age1000[, lapply(.SD, l95_func), by = 'time'][, meas := 'l95'],
    data_age1000[, lapply(.SD, u95_func), by = 'time'][, meas := 'u95']
  ); data[, sim := NULL]
  data <- melt.data.table(data, id.vars = c('time','meas'))
  data[, age := rep(pars$ages, each = nrow(data)/(length(pars$ages)))]
  data <- dcast.data.table(data, time + age ~ meas, value.var = 'value')
  data$age <- factor(data$age, levels = pars$ages)
  
  p3 <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = age), alpha=0.25)  +
    geom_line(aes(y = median, col = age), lwd=0.8)  +
    theme_bw() +
    scale_color_manual(values = colors_p_age_group) + 
    scale_fill_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "Age", fill = 'Age');p3
  
  p3facet <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = age), alpha=0.25)  +
    geom_line(aes(y = median, col = age), lwd=0.8)  +
    theme_bw() +
    scale_color_manual(values = colors_p_age_group) + 
    scale_fill_manual(values = colors_p_age_group) + 
    facet_wrap(age ~ .) +
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "Age", fill = 'Age');p3facet
  
  ## fig 3b by age
  data_age1000 <- byaw[, c('sim',paste0('IUw_a', 1:16))]
  for(a in 1:16){data_age1000[, (paste0('IUw_a', a))] <- 1e3*data_age1000[, get(paste0('IUw_a', a))]/Na[a]}
  data_age1000[, time := data_imd1000$time]
  data_age1000cum <- data_age1000[, lapply(.SD, cumsum), by = c('sim')][, time := data_age1000$time]
  data <- rbind(
    data_age1000cum[, lapply(.SD, median), by = 'time'][, meas := 'median'],
    data_age1000cum[, lapply(.SD, l95_func), by = 'time'][, meas := 'l95'],
    data_age1000cum[, lapply(.SD, u95_func), by = 'time'][, meas := 'u95']
  ); data[, sim := NULL]
  data <- melt.data.table(data, id.vars = c('time','meas'))
  data[, age := rep(pars$ages, each = nrow(data)/(length(pars$ages)))]
  data <- dcast.data.table(data, time + age ~ meas, value.var = 'value')
  data$age <- factor(data$age, levels = pars$ages)
  
  p3b <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = age), alpha=0.25)  +
    geom_line(aes(y = median, col = age), lwd=0.8)  +
    theme_bw() +
    scale_color_manual(values = colors_p_age_group) + 
    scale_fill_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Cumulative infections per 1000 population", x = "Day", color = "Age", fill = 'Age'); p3b
  
  ## save
  p1 + p1b + p2 + p2b + p3 + p3b + plot_layout(nrow = 3, guides = 'collect')
  ggsave(here::here('output','figures','epidem',sens_analysis,'time_series.png'), dpi=600, 
         device = "png", width = 12, height = 9)
  
  ## save
  p2facet + p2bfacet + plot_layout(nrow = 2, guides = 'collect')
  ggsave(here::here('output','figures','epidem',sens_analysis,'time_series_imd_facet.png'), dpi=600, 
         device = "png", width = 12, height = 8)
  
  ## save
  ggsave(plot = p3facet, here::here('output','figures','epidem',sens_analysis,'time_series_age_facet.png'), dpi=600, 
         device = "png", width = 12, height = 8)
  
  # ARRs
  X <- base_imd_arr
  data1000 <- copy(byall)
  for(a in 1:ng){data1000[, (paste0('Iw_g', a))] <- 1e3*data1000[, get(paste0('Iw_g', a))]/Sg0[a]} 
  data_1000ar <- data1000[, lapply(.SD, sum), by = c('sim')][, iW := NULL][, time := NULL]
  data_melt <- melt.data.table(data_1000ar, id.vars = c('sim'))
  data_melt[, age := rep(rep(pars$ages, each = n_distinct(data_melt$sim)), nimd)]
  data_melt[, imd := rep(1:nimd, each = na*n_distinct(data_melt$sim))]
  data_imdX <- data_melt[imd == X][, imd_X_value := value]
  data <- data_melt[data_imdX[, c('sim','age','imd_X_value')], on = c('sim','age')]
  data[, arr := value/imd_X_value]
  data_min <- data[, c('age','imd','arr')]
  data_agg <- rbind(
    data_min[, lapply(.SD, median), by = c('age','imd')][, meas := 'median'],
    data_min[, lapply(.SD, l95_func), by = c('age','imd')][, meas := 'l95'],
    data_min[, lapply(.SD, u95_func), by = c('age','imd')][, meas := 'u95']
  )
  data <- dcast.data.table(data_agg, age + imd ~ meas, value.var = 'arr')
  data$age <- factor(data$age, levels = pars$ages)
  
  arr_plot <- ggplot(data, aes(x=imd)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    geom_errorbar(aes(ymin = l95, ymax = u95, col = as.factor(imd), group = as.factor(imd)), 
                  width = 0.4, lwd = 0.8)  +
    geom_point(aes(y = median, col = as.factor(imd), group = as.factor(imd)),
               size = 3)  +
    theme_bw() +
    # facet_grid(. ~ age, switch = 'x') + 
    facet_wrap(age ~ .) + 
    scale_color_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=12),
          # axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y = "Relative attack rate", x = 'Age group', color = "IMD", fill = 'IMD'); arr_plot
  ggsave(here::here('output','figures','epidem',sens_analysis,'rel_attack_rates_by_imd.png'), dpi=600, 
         bg = 'white',
         device = "png", width = 12, height = 12)
  
  Xage <- base_age_arr
  data_ageX <- data_melt[age == Xage][, age_X_value := value]
  data <- data_melt[data_ageX[, c('sim','imd','age_X_value')], on = c('sim','imd')]
  data[, arr := value/age_X_value]
  data_min <- data[, c('age','imd','arr')]
  data_agg <- rbind(
    data_min[, lapply(.SD, median), by = c('age','imd')][, meas := 'median'],
    data_min[, lapply(.SD, l95_func), by = c('age','imd')][, meas := 'l95'],
    data_min[, lapply(.SD, u95_func), by = c('age','imd')][, meas := 'u95']
  )
  data <- dcast.data.table(data_agg, age + imd ~ meas, value.var = 'arr')
  data$age <- factor(data$age, levels = pars$ages)
  
  arr_plot_age <- ggplot(data, aes(x=age)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    geom_errorbar(aes(ymin = l95, ymax = u95, col = as.factor(age), group = as.factor(age)), 
                  width = 0.4, lwd = 0.8)  +
    geom_point(aes(y = median, col = as.factor(age), group = as.factor(age)),
               size = 3)  +
    theme_minimal() +
    facet_grid(imd~., switch = 'x') + 
    scale_color_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=12),
          # axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y = "Relative attack rate", x = 'IMD quintile', color = "Age", fill = 'Age'); arr_plot_age
  ggsave(here::here('output','figures','epidem',sens_analysis,'rel_attack_rates_by_age.png'), dpi=600, 
         bg = 'white',
         device = "png", width = 10, height = 10)
  
  ## across all groups
  data1000 <- copy(byall)
  for(a in 1:ng){data1000[, (paste0('Iw_g', a))] <- 1e3*data1000[, get(paste0('Iw_g', a))]/Sg0[a]} 
  data <- rbind(
    data1000[, lapply(.SD, median), by = 'time'][, meas := 'median'],
    data1000[, lapply(.SD, l95_func), by = 'time'][, meas := 'l95'],
    data1000[, lapply(.SD, u95_func), by = 'time'][, meas := 'u95']
  ); data[, sim := NULL][, iW := NULL]
  data <- melt.data.table(data, id.vars = c('time','meas'))
  data[, age := rep(rep(pars$ages, each = pars$nd*n_distinct(data$meas)), nimd)]
  data[, imd := rep(1:nimd, each = na*pars$nd*n_distinct(data$meas))]
  data <- dcast.data.table(data, time + age + imd ~ meas, value.var = 'value')
  data$age <- factor(data$age, levels = pars$ages)
  
  all_time_s <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = age), alpha=0.25)  +
    geom_line(aes(y = median, col = age), lwd=0.8)  +
    theme_bw() +
    facet_grid(imd ~ .) + 
    scale_color_manual(values = colors_p_age_group) + 
    scale_fill_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "Age", fill = 'Age'); all_time_s
  
  # across all groups, cumulative
  data1000 <- copy(byall)
  for(a in 1:ng){data1000[, (paste0('Iw_g', a))] <- 1e3*data1000[, get(paste0('Iw_g', a))]/Sg0[a]} 
  data_1000cum <- data1000[, lapply(.SD, cumsum), by = c('sim')][, time := data1000$time]
  data <- rbind(
    data_1000cum[, lapply(.SD, median), by = 'time'][, meas := 'median'],
    data_1000cum[, lapply(.SD, l95_func), by = 'time'][, meas := 'l95'],
    data_1000cum[, lapply(.SD, u95_func), by = 'time'][, meas := 'u95']
  ); data[, sim := NULL][, iW := NULL]
  data <- melt.data.table(data, id.vars = c('time','meas'))
  data[, age := rep(rep(pars$ages, each = pars$nd*n_distinct(data$meas)), nimd)]
  data[, imd := rep(1:nimd, each = na*pars$nd*n_distinct(data$meas))]
  data <- dcast.data.table(data, time + age + imd ~ meas, value.var = 'value')
  data$age <- factor(data$age, levels = pars$ages)
  
  all_time_s_cum <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = age), alpha=0.25)  +
    geom_line(aes(y = median, col = age), lwd=0.8)  +
    theme_bw() +
    facet_grid(imd ~ .) + 
    scale_color_manual(values = colors_p_age_group) + 
    scale_fill_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "Age", fill = 'Age'); all_time_s_cum
  
  all_time_s_cum_imd <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = l95, ymax = u95, fill = as.factor(imd)), alpha=0.25)  +
    geom_line(aes(y = median, col = as.factor(imd)), lwd=0.8)  +
    theme_bw() +
    facet_wrap(age ~ .) + 
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "Age", fill = 'Age'); all_time_s_cum_imd
  
  age_spec_ar <- ggplot(data[time == max(data$time)]) + 
    # geom_ribbon(aes(x = age, ymin = l95, ymax = u95, fill = as.factor(imd), group = imd), alpha = 0.25) +
    geom_bar(aes(x = age, y = median, fill = as.factor(imd)),
             stat = 'identity', position = 'dodge') +
    geom_errorbar(aes(x = age, ymin = l95, ymax = u95, 
                      group = as.factor(imd)), 
                  width = 0.4, position = position_dodge(width = 0.9), alpha= 0.75) +
    theme_bw() +
    scale_fill_manual(values = imd_quintile_colors) + 
    # scale_color_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Attack rate /1000", x = "Age group", color = "IMD quintile", fill = 'IMD quintile'); age_spec_ar
  
  all_time_s_cum_hm <- ggplot(data[time == max(data$time)]) + 
    geom_tile(aes(x = imd, y = age, fill = median)) +
    theme_bw() +
    scale_fill_viridis(option='A') + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Age group", x = "IMD quintile", color = "Attack rate /1000", fill = 'Attack rate /1000'); all_time_s_cum_hm
  
  layout <- '
AAAABBBB
AAAABBBB
AAAABBBB
AAAABBBB
AAAABBBB
CCCDDDDD
CCCDDDDD
CCCDDDDD
'
  
  all_time_s + all_time_s_cum + all_time_s_cum_hm + age_spec_ar + plot_layout(nrow = 2, guides = 'collect', design = layout)
  ggsave(here::here('output','figures','epidem',sens_analysis,'patchwork.png'), dpi=600, 
         device = "png", width = 14, height = 16)
  
  all_time_s + all_time_s_cum + plot_layout(nrow = 1, guides = 'collect')
  ggsave(here::here('output','figures','epidem',sens_analysis,'age_x_imd.png'), dpi=600, 
         device = "png", width = 10, height = 8)
  
  ggsave(plot = all_time_s_cum_imd, 
         here::here('output','figures','epidem',sens_analysis,'age_x_imd_cumulative.png'), dpi=600, 
         device = "png", width = 10, height = 10)
  
  ggsave(plot = age_spec_ar, 
         .args[3],
         dpi=600, 
         device = "png", width = 12, height = 6)
  
}else{
  
  # regional age structure
  
  imd_age_raw <-  data.table(read_csv(file.path("data", "census","pcd1age.csv"), show_col_types = F))
  
  demog_allreg <- imd_age_raw %>%
    mutate(
      age = age_grp,
      p_engreg = case_when(
        grepl('London',eng_reg) ~ 'Greater London',
        grepl('Yorkshire',eng_reg) ~ 'Yorkshire and the Humber',
        T ~ eng_reg),
      imd = as.character(imd_quintile)) %>% 
    select(p_engreg, imd, age, population) %>% 
    group_by(p_engreg, imd, age) %>% 
    summarise(Population = sum(population)) %>% ungroup() %>% 
    group_by(p_engreg, imd) %>% 
    mutate(tot_pop = sum(Population)) %>% ungroup() %>% 
    mutate(Proportion = Population/tot_pop)
  
  demog_allreg$age <- factor(demog_allreg$age,
                             levels = names(colors_age_grp))
  demog_allreg <- demog_allreg %>% arrange(p_engreg, imd, age)
  
  demog_allreg <- demog_allreg %>% 
    mutate(age = case_when(
      grepl('Aged 4 years', age) ~ '0-4',
      grepl('75|80|85', age) ~ '75+',
      T ~ gsub('Aged ', '', gsub(' to ', '-', gsub(' years', '', age)))
    )) 
  
  demog_allreg <- data.table(demog_allreg)
  
  ## fig 1 overall
  data1000 <- byw[, c('p_engreg','sim','time','Iw','Uw')][, Infe := (Iw + Uw)][, c('p_engreg','sim','time','Infe')]
  data <- rbind(
    data1000[, lapply(.SD, median), by = c('p_engreg','time')][, meas := 'median'],
    data1000[, lapply(.SD, l95_func), by = c('p_engreg','time')][, meas := 'l95'],
    data1000[, lapply(.SD, u95_func), by = c('p_engreg','time')][, meas := 'u95']
  ); data[, sim := NULL]
  data <- dcast.data.table(data, p_engreg + time ~ meas, value.var = 'Infe')
  data <- data[demog_allreg[, c('p_engreg','Population')][, lapply(.SD, sum), by = 'p_engreg'], on = 'p_engreg']
  
  p1 <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = 1000*l95/Population, ymax = 1000*u95/Population, fill = p_engreg), alpha=0.25)  +
    geom_line(aes(y = 1000*median/Population, col = p_engreg), lwd=1)  +
    theme_bw() +
    scale_color_manual(values = colors_p_engreg) + 
    scale_fill_manual(values = colors_p_engreg) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", col = 'Region', fill = 'Region'); p1
  
  ## fig 1b cumulative
  data1000 <- byw[, c('p_engreg','sim','time','Iw','Uw')][, Infe := (Iw + Uw)][, c('p_engreg','sim','time','Infe')]
  data1000cum <- data1000[, lapply(.SD, cumsum), by = c('p_engreg','sim')][, time := data1000$time]
  data <- rbind(
    data1000cum[, lapply(.SD, median), by = c('p_engreg','time')][, meas := 'median'],
    data1000cum[, lapply(.SD, l95_func), by = c('p_engreg','time')][, meas := 'l95'],
    data1000cum[, lapply(.SD, u95_func), by = c('p_engreg','time')][, meas := 'u95']
  ); data[, sim := NULL]
  data <- dcast.data.table(data, p_engreg + time ~ meas, value.var = 'Infe')
  data <- data[demog_allreg[, c('p_engreg','Population')][, lapply(.SD, sum), by = 'p_engreg'], on = 'p_engreg']
  
  p1b <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = 1000*l95/Population, ymax = 1000*u95/Population, fill = p_engreg), alpha=0.25)  +
    geom_line(aes(y = 1000*median/Population, col = p_engreg), lwd=1)  +
    theme_bw() +
    scale_color_manual(values = colors_p_engreg) + 
    scale_fill_manual(values = colors_p_engreg) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Cumulative infections per 1000 population", x = "Day" , col = 'Region', fill = 'Region'); p1b
  
  ## fig 2 by imd
  data_imd1000 <- byw[, c('p_engreg','sim','time','IUw_s1','IUw_s2','IUw_s3','IUw_s4','IUw_s5')]
  data <- rbind(
    data_imd1000[, lapply(.SD, median), by = c('p_engreg','time')][, meas := 'median'],
    data_imd1000[, lapply(.SD, l95_func), by = c('p_engreg','time')][, meas := 'l95'],
    data_imd1000[, lapply(.SD, u95_func), by = c('p_engreg','time')][, meas := 'u95']
  ); data[, sim := NULL]
  data <- melt.data.table(data, id.vars = c('p_engreg','time','meas'))
  data[, imd := substr(variable,6,6)]
  data <- dcast.data.table(data, p_engreg + time + imd ~ meas, value.var = 'value')
  data <- data[demog_allreg[, c('imd','p_engreg','Population')][, lapply(.SD, sum), by = c('imd','p_engreg')], on = c('imd','p_engreg')]
  
  p2 <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = 1000*l95/Population, ymax = 1000*u95/Population, fill = imd), alpha=0.25) +
    geom_line(aes(y = 1000*median/Population, col = imd), lwd=0.8)  +
    theme_bw() +
    facet_wrap(. ~ p_engreg) + 
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "IMD", fill = 'IMD'); p2
  
  ## fig 2b by imd
  data_imd1000 <- byw[, c('p_engreg','sim','time','IUw_s1','IUw_s2','IUw_s3','IUw_s4','IUw_s5')]
  data_imd1000cum <- data_imd1000[, lapply(.SD, cumsum), by = c('p_engreg','sim')][, time := data_imd1000$time]
  data <- rbind(
    data_imd1000cum[, lapply(.SD, median), by = c('p_engreg','time')][, meas := 'median'],
    data_imd1000cum[, lapply(.SD, l95_func), by = c('p_engreg','time')][, meas := 'l95'],
    data_imd1000cum[, lapply(.SD, u95_func), by = c('p_engreg','time')][, meas := 'u95']
  ); data[, sim := NULL]
  data <- melt.data.table(data, id.vars = c('p_engreg','time','meas'))
  data[, imd := substr(variable,6,6)]
  data <- dcast.data.table(data, p_engreg + time + imd ~ meas, value.var = 'value')
  data <- data[demog_allreg[, c('imd','p_engreg','Population')][, lapply(.SD, sum), by = c('imd','p_engreg')], on = c('imd','p_engreg')]
  
  p2b <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = 1000*l95/Population, ymax = 1000*u95/Population, fill = imd), alpha=0.25)  +
    geom_line(aes(y = 1000*median/Population, col = imd), lwd=0.8)  +
    theme_bw() +
    facet_wrap(. ~ p_engreg) + 
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Cumulative infections per 1000 population", x = "Day", color = "IMD", fill = 'IMD'); p2b
  
  imd_final_size <- data %>% 
    filter(time == max(time)) %>% 
    ggplot(aes(x=imd)) + 
    geom_errorbar(aes(ymin = 1000*l95/Population, ymax = 1000*u95/Population, col = imd), width = 0.2)  +
    geom_point(aes(y = 1000*median/Population, col = imd), size = 3)  +
    theme_bw() +
    facet_wrap(. ~ p_engreg) + 
    scale_color_manual(values = imd_quintile_colors) + 
    # scale_fill_manual(values = imd_quintile_colors) + 
    ylim(c(0,800)) + 
    theme(text=element_text(size=14),
          legend.position = 'none',
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Final size per 1000 population", x = "IMD Quintile", color = "IMD", fill = 'IMD'); imd_final_size
  
  data_imd1000 <- byw[, c('p_engreg','sim','time','IUw_s1','IUw_s2','IUw_s3','IUw_s4','IUw_s5')]
  data_imd1000sum <- data_imd1000[, lapply(.SD, sum), by = c('p_engreg','sim')][, time := NULL]
  X <- base_imd_arr
  data_melt <- melt.data.table(data_imd1000sum, id.vars = c('p_engreg','sim'))
  data_melt[, imd := substr(variable, 6, 6)][, variable := NULL]
  data_imdX <- data_melt[imd == X][, imd_X_value := value]
  data_imdX <- data_imdX[demog_allreg[imd == X][, c('p_engreg','Population')][, lapply(.SD, sum), by = c('p_engreg')], on = c('p_engreg')]
  data_imdX[, imd_X_arr := imd_X_value/Population]
  data <- data_melt[data_imdX[, c('p_engreg','sim','imd_X_arr')], on = c('p_engreg','sim')]
  data <- data[demog_allreg[, c('imd','p_engreg','Population')][, lapply(.SD, sum), by = c('imd','p_engreg')], on = c('imd','p_engreg')]
  data[, arr := (value/Population)/imd_X_arr]
  data_min <- data[, c('p_engreg','imd','arr')]
  data_agg <- rbind(
    data_min[, lapply(.SD, median), by = c('p_engreg','imd')][, meas := 'median'],
    data_min[, lapply(.SD, l95_func), by = c('p_engreg','imd')][, meas := 'l95'],
    data_min[, lapply(.SD, u95_func), by = c('p_engreg','imd')][, meas := 'u95']
  )
  data <- dcast.data.table(data_agg, p_engreg + imd ~ meas, value.var = 'arr')
  
  imd_final_size_arr <- data %>% 
    ggplot(aes(x=imd)) + 
    geom_hline(col = 1, lty = 2, alpha = 0.3, yintercept = 1) +
    geom_errorbar(aes(ymin = l95, ymax = u95, col = imd), width = 0.2)  +
    geom_point(aes(y = median, col = imd), size = 3)  +
    facet_wrap(. ~ p_engreg) + 
    theme_bw() +
    ylim(c(0.3, 2.5)) +
    scale_color_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=14),
          legend.position = 'none',
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Relative final size", x = "IMD Quintile", color = "IMD", fill = 'IMD'); imd_final_size_arr
  
  imd_final_size + imd_final_size_arr + plot_layout(nrow = 1) 
  
  ## save
  ggsave(here::here('output','figures','epidem',sens_analysis,'final_size_imd.png'), dpi=600, 
         device = "png", width = 16, height = 8)
  
  ## fig 3 by age
  data_age1000 <- byaw[, c('p_engreg','sim',paste0('IUw_a', 1:16))]
  data_age1000[, time := rep(min(byw$time):max(byw$time), n_distinct(byaw$p_engreg)*max(data_age1000$sim))]
  data <- rbind(
    data_age1000[, lapply(.SD, median), by = c('p_engreg','time')][, meas := 'median'],
    data_age1000[, lapply(.SD, l95_func), by = c('p_engreg','time')][, meas := 'l95'],
    data_age1000[, lapply(.SD, u95_func), by = c('p_engreg','time')][, meas := 'u95']
  ); data[, sim := NULL]
  data <- melt.data.table(data, id.vars = c('p_engreg','time','meas'))
  data[, age := rep(pars$ages, each = nrow(data)/(length(pars$ages)))]
  data <- dcast.data.table(data, p_engreg + time + age ~ meas, value.var = 'value')
  data$age <- factor(data$age, levels = pars$ages)
  data <- data[demog_allreg[, c('age','p_engreg','Population')][, lapply(.SD, sum), by = c('age','p_engreg')], on = c('age','p_engreg')]
  data$age <- factor(data$age, levels = pars$ages)
  
  p3 <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = 1000*l95/Population, ymax = 1000*u95/Population, fill = age), alpha=0.25)  +
    geom_line(aes(y = 1000*median/Population, col = age), lwd=0.8)  +
    theme_bw() +
    facet_wrap(. ~ p_engreg) +
    scale_color_manual(values = colors_p_age_group) + 
    scale_fill_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Infections per 1000 population", x = "Day", color = "Age", fill = 'Age');p3
  
  ## fig 3b by age
  data_age1000 <- byaw[, c('p_engreg','sim',paste0('IUw_a', 1:16))]
  data_age1000[, time := data_imd1000$time]
  data_age1000cum <- data_age1000[, lapply(.SD, cumsum), by = c('p_engreg','sim')][, time := data_age1000$time]
  data <- rbind(
    data_age1000cum[, lapply(.SD, median), by = c('p_engreg','time')][, meas := 'median'],
    data_age1000cum[, lapply(.SD, l95_func), by = c('p_engreg','time')][, meas := 'l95'],
    data_age1000cum[, lapply(.SD, u95_func), by = c('p_engreg','time')][, meas := 'u95']
  ); data[, sim := NULL]
  data <- melt.data.table(data, id.vars = c('p_engreg','time','meas'))
  data[, age := rep(pars$ages, each = nrow(data)/(length(pars$ages)))]
  data <- dcast.data.table(data, p_engreg + time + age ~ meas, value.var = 'value')
  data$age <- factor(data$age, levels = pars$ages)
  data <- data[demog_allreg[, c('age','p_engreg','Population')][, lapply(.SD, sum), by = c('age','p_engreg')], on = c('age','p_engreg')]
  
  p3b <- ggplot(data, aes(x=time)) + 
    geom_ribbon(aes(ymin = 1000*l95/Population, ymax = 1000*u95/Population, fill = age), alpha=0.25)  +
    geom_line(aes(y = 1000*median/Population, col = age), lwd=0.8)  +
    theme_bw() +
    facet_wrap(. ~ p_engreg) +
    scale_color_manual(values = colors_p_age_group) + 
    scale_fill_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Cumulative infections per 1000 population", x = "Day", color = "Age", fill = 'Age'); p3b
  
  ## save
  p1 + p1b + p2 + p2b + p3 + p3b + plot_layout(nrow = 3, guides = 'collect')
  ggsave(here::here('output','figures','epidem',sens_analysis,'time_series.png'), dpi=600, 
         device = "png", width = 16, height = 12)
  
  ## save
  p2b 
  ggsave(here::here('output','figures','epidem',sens_analysis,'time_series_imd_facet.png'), dpi=600, 
         device = "png", width = 12, height = 8)
  
  ## save
  p3b
  ggsave(here::here('output','figures','epidem',sens_analysis,'time_series_age_facet.png'), dpi=600, 
         device = "png", width = 12, height = 8)
  
  # ARRs
  X <- base_imd_arr
  data1000 <- copy(byall)
  data_1000ar <- data1000[, lapply(.SD, sum), by = c('p_engreg','sim')][, iW := NULL][, time := NULL]
  data_melt <- melt.data.table(data_1000ar, id.vars = c('p_engreg','sim'))
  data_melt[, age := rep(rep(pars$ages, each = n_distinct(data_melt$p_engreg)*n_distinct(data_melt$sim)), nimd)]
  data_melt[, imd := rep(1:nimd, each = na*n_distinct(data_melt$p_engreg)*n_distinct(data_melt$sim))]
  data_imdX <- data_melt[imd == X][, imd_X_value := value]
  data_imdX <- data_imdX[demog_allreg[imd == X][, c('p_engreg','age','Population')][, lapply(.SD, sum), by = c('p_engreg','age')], 
                         on = c('p_engreg','age')]
  data_imdX[, imd_X_arr := imd_X_value/Population]
  data <- data_melt[data_imdX[, c('p_engreg','sim','age','imd_X_arr')], on = c('p_engreg','sim','age')]
  data <- data[demog_allreg[, c('age','imd','p_engreg','Population')][, lapply(.SD, sum), by = c('age','imd','p_engreg')][, imd := as.numeric(imd)], on = c('age','imd','p_engreg')]
  data[, arr := (value/Population)/imd_X_arr]
  data_min <- data[, c('p_engreg','age','imd','arr')]
  data_agg <- rbind(
    data_min[, lapply(.SD, median), by = c('p_engreg','age','imd')][, meas := 'median'],
    data_min[, lapply(.SD, l95_func), by = c('p_engreg','age','imd')][, meas := 'l95'],
    data_min[, lapply(.SD, u95_func), by = c('p_engreg','age','imd')][, meas := 'u95']
  )
  data <- dcast.data.table(data_agg, p_engreg + age + imd ~ meas, value.var = 'arr')
  data$age <- factor(data$age, levels = pars$ages)
  
  arr_plot <- ggplot(data, aes(x=imd)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    geom_errorbar(aes(ymin = l95, ymax = u95, col = as.factor(imd), group = as.factor(imd)), 
                  width = 0.4, lwd = 0.8)  +
    geom_point(aes(y = median, col = as.factor(imd), group = as.factor(imd)),
               size = 3)  +
    geom_point(data = data %>% filter(l95 > 1),
               aes(y = median, col = as.factor(imd)), shape = 1, size = 5)  +
    theme_bw() +
    facet_grid(p_engreg ~ age, scales = 'free_y', switch = 'x') + 
    scale_color_manual(values = imd_quintile_colors) + 
    theme(text=element_text(size=12),
          # axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y = "Relative attack rate", x = 'Age group', color = "IMD", fill = 'IMD'); arr_plot
  ggsave(here::here('output','figures','epidem',sens_analysis,'rel_attack_rates_by_imd.png'), dpi=600, 
         bg = 'white',
         device = "png", width =20, height = 20)
  
  Xage <- base_age_arr
  data_ageX <- data_melt[age == Xage][, age_X_value := value]
  data_ageX <- data_ageX[demog_allreg[age == Xage][, c('p_engreg','imd','Population')][, lapply(.SD, sum), by = c('p_engreg','imd')][,imd := as.numeric(imd)], 
                         on = c('p_engreg','imd')]
  data_ageX[, age_X_arr := age_X_value/Population]
  data <- data_melt[data_ageX[, c('p_engreg','sim','imd','age_X_arr')], on = c('p_engreg','sim','imd')]
  data <- data[demog_allreg[, c('p_engreg','age','imd','Population')][, lapply(.SD, sum), by = c('p_engreg','age','imd')][,imd := as.numeric(imd)], 
                         on = c('p_engreg','age','imd')]
  data[, arr := (value/Population)/age_X_arr]
  data_min <- data[, c('p_engreg','age','imd','arr')]
  data_agg <- rbind(
    data_min[, lapply(.SD, median), by = c('p_engreg','age','imd')][, meas := 'median'],
    data_min[, lapply(.SD, l95_func), by = c('p_engreg','age','imd')][, meas := 'l95'],
    data_min[, lapply(.SD, u95_func), by = c('p_engreg','age','imd')][, meas := 'u95']
  )
  data <- dcast.data.table(data_agg, p_engreg + age + imd ~ meas, value.var = 'arr')
  data$age <- factor(data$age, levels = pars$ages)
  
  arr_plot_age <- ggplot(data, aes(x=age)) + 
    geom_hline(yintercept = 1, lty = 2, alpha = 0.5) + 
    geom_errorbar(aes(ymin = l95, ymax = u95, col = as.factor(age), group = as.factor(age)), 
                  width = 0.4, lwd = 0.8)  +
    geom_point(aes(y = median, col = as.factor(age), group = as.factor(age)),
               size = 3)  +
    geom_point(data = data %>% filter(l95 > 1),
               aes(y = median, col = as.factor(age)), shape = 1, size = 5)  +
    theme_bw() +
    facet_grid(p_engreg ~ imd, scales = 'free', switch = 'x') + 
    scale_color_manual(values = colors_p_age_group) + 
    theme(text=element_text(size=12),
          # axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    labs(y = "Relative attack rate", x = 'IMD quintile', color = "Age", fill = 'Age'); arr_plot_age
  ggsave(here::here('output','figures','epidem',sens_analysis,'rel_attack_rates_by_age.png'), dpi=600, 
         bg = 'white',
         device = "png", width = 20, height = 20)
  
  
  data <- copy(data_melt)
  data <- data[demog_allreg[, c('p_engreg','age','imd','Population')][, lapply(.SD, sum), by = c('p_engreg','age','imd')][,imd := as.numeric(imd)], 
               on = c('p_engreg','age','imd')]
  data[, arr := (value/Population)]
  data_min <- data[, c('p_engreg','age','imd','arr')]
  data_agg <- rbind(
    data_min[, lapply(.SD, median), by = c('p_engreg','age','imd')][, meas := 'median'],
    data_min[, lapply(.SD, l95_func), by = c('p_engreg','age','imd')][, meas := 'l95'],
    data_min[, lapply(.SD, u95_func), by = c('p_engreg','age','imd')][, meas := 'u95']
  )
  data <- dcast.data.table(data_agg, p_engreg + age + imd ~ meas, value.var = 'arr')
  data$age <- factor(data$age, levels = pars$ages)
  
  age_spec_ar <- ggplot(data) + 
    geom_bar(aes(x = age, y = median, fill = as.factor(imd)),
             stat = 'identity', position = 'dodge') +
    geom_errorbar(aes(x = age, ymin = l95, ymax = u95, 
                      group = as.factor(imd)), 
                  width = 0.4, position = position_dodge(width = 0.9), alpha= 0.75) +
    theme_bw() +
    scale_fill_manual(values = imd_quintile_colors) + 
    facet_wrap(p_engreg ~ ., scales = 'free') + 
    theme(text=element_text(size=10),
          legend.key.size = unit(2, 'mm'),
          plot.title = element_text(size = 12),
          axis.text.y = element_text(color=1),
          axis.text.x = element_text(color=1)) +
    labs(y = "Attack rate /1000", x = "Age group", color = "IMD quintile", fill = 'IMD quintile'); age_spec_ar
  
  ggsave(plot = age_spec_ar, 
         .args[3],
         dpi=600, 
         device = "png", width = 24, height = 12)
  
}
  









