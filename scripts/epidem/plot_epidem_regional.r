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

for(k in 1:4){
  
.args <- if (interactive()) c(
  k,
  file.path('output','figures','epidem','regional',k,'attack_rate_bars.png')
) else commandArgs(trailingOnly = TRUE)

if(!file.exists(gsub('/attack_rate_bars.png','',.args[2]))){dir.create(gsub('/attack_rate_bars.png','',.args[2]))}

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

regional_SA <- .args[1]
sens_analysis <- paste0('regional/', regional_SA)
                        
if(regional_SA == 1){} # do nothing
if(regional_SA == 2){pars$R0 <- 1.1} # lower R0
if(regional_SA == 3){pars$R0 <- 3} # higher R0
if(regional_SA == 4){ # fix beta, not R0 
  pset$R0fixed <- FALSE
  cm_base <- data.table(suppressWarnings(read_csv(file.path("output", "data", "cont_matrs","base","fitted_matrs_balanced.csv"), show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
  cm_base_mean <- cm_base[, lapply(.SD, mean), by = c('p_age_group','p_imd_q','c_age_group','c_imd_q')]
  source(paste0(source_dir,"/R0_.r")) 
  cm <- cm_base_mean[order(bootstrap_index, p_imd_q, p_age_group, c_imd_q, c_age_group)]
  cm <- cm %>% 
    mutate(p = paste0(p_imd_q, '_', p_age_group),
           c = paste0(c_imd_q, '_', c_age_group)) %>% 
    select(p,c,n) %>% pivot_wider(names_from = c, values_from = n) 
  pvec <- cm$p 
  cm <- cm %>% select(!p) %>% as.matrix()
  betanew <- R0(pars, cm_in = cm, R0assumed = as.numeric(pars$R0), printout = 0) #default 2.5
  pars$beta <- betanew
}

cat("\nDisease: ", pars$Disease,' --- ', sep = '')
cat("Vaccination: ", pars$Vaccination,' --- ', sep = '')
cat("Incidence: ", pars$Incidence,' --- ', sep = '')
if(!pset$R0fixed){cat("R0 not fixed, beta: ", round(pars$beta, 4),'\n', sep = '')}else{
  cat("R0: ", pars$R0,'\n', sep = '')
}

## If R0 low, make runtime longer
if(pset$R0fixed & (pars$R0 <= 1.1)){ 
  
  pars$times  <- 0:1000     #days sequence
  pars$nt     <- (max(pars$times)-min(pars$times))/pars$dt + 1       #no. time points, iterations
  pars$nw     <- ceiling((max(pars$times)-min(pars$times))/7)   #weeks length of model run
  pars$nd     <- ceiling((max(pars$times)-min(pars$times)))+1   #days length of model run
  
}else{
  if(pset$R0fixed & (pars$R0 < 1.65)){ 
    
    pars$times  <- 0:130     #days sequence
    pars$nt     <- (max(pars$times)-min(pars$times))/pars$dt + 1       #no. time points, iterations
    pars$nw     <- ceiling((max(pars$times)-min(pars$times))/7)   #weeks length of model run
    pars$nd     <- ceiling((max(pars$times)-min(pars$times)))+1   #days length of model run
    
  }
}


## Set base levels for IMD and age
base_imd_arr <- 5
base_age_arr <- '35-39'

# set seed
set.seed(120)

## Figures

ar=1 #aspect ratio

l95_func <- function(x){quantile(x, probs=0.025)}; u95_func <- function(x){quantile(x, probs=0.975)}

## Demography
# regional age structure

imd_age_raw <- data.table(read_csv(file.path("data","imd_25","imd_ages_1.csv"), show_col_types = F))

demog_allreg <- imd_age_raw %>% 
  mutate(p_engreg = case_when(
    grepl('London',p_engreg) ~ 'Greater London',
    grepl('Yorkshire',p_engreg) ~ 'Yorkshire and the Humber',
    T ~ p_engreg
  ),
  imd = imd_quintile,
  population = pop,
  age = age_grp) %>% 
  select(p_engreg, imd, age, population) %>% 
  group_by(p_engreg, imd, age) %>% 
  summarise(population = sum(population)) %>% ungroup() %>% 
  group_by(p_engreg, imd) %>% 
  mutate(tot_pop_imd = sum(population)) %>% 
  ungroup() %>% 
  group_by(p_engreg) %>% mutate(tot_pop = sum(population)) %>% ungroup() %>% 
  mutate(prop_imd = population/tot_pop_imd,
         prop = population/tot_pop)

demog_allreg$age <- factor(demog_allreg$age, levels = age_labels)
demog_allreg <- demog_allreg %>% arrange(p_engreg, imd, age)
demog_allreg <- data.table(demog_allreg)

## read files
byw <- data.table(); byaw <- data.table(); byall <- data.table()
for(reg in unique(demog_allreg$p_engreg)){
  
  filename <- file.path("output", "data", "epidem","regional","byall.rds")
  filename <- gsub('.rds', paste0('_', regional_SA, '_', gsub(' ', '_', reg), '.rds'), filename)
    
  byw <- rbind(byw,
               readRDS(gsub('all','w',filename)))
  byaw <- rbind(byaw,
                readRDS(gsub('all','aw',filename)))
  byall <- rbind(byall,
                 readRDS(filename))
  
}

  
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
  
  imd_final_size <- data_imd1000cum %>% 
    filter(time == max(time)) %>% select(!time) %>% 
    pivot_longer(!c(p_engreg, sim)) %>% 
    mutate(imd = substr(name, 6,6)) %>% 
    group_by(p_engreg, imd) %>% mutate(median = median(value)) %>% 
    left_join(demog_allreg[, c('imd','p_engreg','Population')][, lapply(.SD, sum), by = c('imd','p_engreg')], by = c('imd','p_engreg')) %>% 
    ggplot(aes(x=imd)) + 
    geom_violin(aes(y = 1000*value/Population, fill = imd, col = imd), alpha = 0.4)  +
    geom_point(aes(y = 1000*median/Population, col = imd), size = 4)  +
    theme_bw() +
    facet_wrap(. ~ p_engreg) + 
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) +
    ylim(c(0,NA)) +
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
  
  imd_final_size_arr <- data_min %>% 
    group_by(p_engreg, imd) %>% mutate(median = median(arr)) %>% 
    ggplot(aes(x=imd)) + 
    geom_hline(col = 1, lty = 2, alpha = 0.3, yintercept = 1) +
    geom_violin(aes(y = arr, fill = imd, col = imd), alpha = 0.4)  +
    geom_point(aes(y = median, col = imd), size = 4) +
    facet_wrap(. ~ p_engreg) + 
    theme_bw() +
    ylim(c(0, NA)) +
    scale_color_manual(values = imd_quintile_colors) + 
    scale_fill_manual(values = imd_quintile_colors) + 
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
         .args[2],
         dpi=600, 
         device = "png", width = 24, height = 12)
  

}









