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
  file.path("output", "data", "epidem","byall.rds"),
  file.path('output','figures','epidem','attack_rates_bars.png')
) else commandArgs(trailingOnly = TRUE)

# source colors etc.
source(here::here('scripts','assign_imd','assign_imd_fcns.R'))
source(here::here('scripts','setup','colors.R'))

### Basic setting
source_dir <- "scripts/epidem"
source(here::here(source_dir,'setup.r')) #repo

### Diseases cycle
pset$Disease <- "Influenza"

## Parameters
if(pset$Vaccination==0){
  if(pset$Disease=="COVID-19")    source(paste0(source_dir,"/pars/parsC_.r"))
  if(pset$Disease=="Influenza")   source(paste0(source_dir,"/pars/parsF_.r"))
  if(pset$Disease=="RSV-illness") source(paste0(source_dir,"/pars/parsR_.r"))
}else{
  if(pset$Disease=="COVID-19")    source(paste0(source_dir,"/pars/parsCv_.r"))
  if(pset$Disease=="Influenza")   source(paste0(source_dir,"/pars/parsFv_.r"))
  if(pset$Disease=="RSV-illness") source(paste0(source_dir,"/pars/parsRv_.r"))
}

# set seed
set.seed(120)

## Demography
demog <- read_csv("data/census/pcd1.csv", show_col_types = F) %>% 
  group_by(imd_quintile) %>% mutate(tot_pop = sum(population)) %>% 
  group_by(imd_quintile, age_grp, tot_pop) %>% summarise(Population = sum(population)) %>% 
  mutate(Proportion = Population/tot_pop) %>% rename(Age = age_grp, IMD = imd_quintile) 
demog$Age <- factor(demog$Age,
                    levels = names(colors_age_grp))
demog <- demog %>% arrange(IMD, Age)
# number of age groups
na   = pars$na
# number of SES
nimd = pars$nimd
# number of groups
ng   = na*nimd

## Demography
demog <- read_csv("data/census/pcd1.csv", show_col_types = F) %>% 
  group_by(imd_quintile) %>% mutate(tot_pop = sum(population)) %>% 
  group_by(imd_quintile, age_grp, tot_pop) %>% summarise(Population = sum(population)) %>% 
  mutate(Proportion = Population/tot_pop) %>% rename(Age = age_grp, IMD = imd_quintile) 
demog$Age <- factor(demog$Age,
                    levels = names(colors_age_grp))
demog <- demog %>% arrange(IMD, Age)
# number of age groups
na   = pars$na
# number of SES
nimd = pars$nimd
# number of groups
ng   = na*nimd

# proportion by age
pa<-vector(); for (i in 1:na){pa[i]=sum(demog$Population[which(demog$Age==pars$ages[i])])/sum(demog$Population)}
#  check:
#  round(pa,4)          [1] 0.0573 0.0873 0.0693 0.1500 0.1337 0.1258 0.1351 0.1058 0.1358
#  round(pars$ageons,4) [1] 0.0471 0.0882 0.0700 0.1516 0.1351 0.1272 0.1366 0.1069 0.1373

if(grepl('14', demog$Age[2])){warning('Demog names wrong')}

## Initial state: S, E1:2, I1:2, U1:2, R, D
oNg  <- 1/demog$Population;   # 1/Population
Sg0  <- demog$Population;   # Susceptible - Initial population, unless there's acquired immunity
E1g0 <- rep(0,ng);  # Exposed     - seed of epidemic
E2g0 <- rep(0,ng);  # Exposed
U1g0 <- rep(0,ng);  # Pre-clinical cases
U2g0 <- rep(0,ng);  # Pre-clinical cases
I1g0 <- rep(0,ng);  # Sub-clinical cases
I2g0 <- rep(0,ng);  # clinical cases
Rg0  <- rep(0,ng);  # Recovered 
Dg0  <- rep(0,ng);  # Dead 

## Population by age group (over SES), by SES (over age), overall
Na<-rep(0,na)
Ns<-rep(0,nimd)
for (ia in 1:na) { for (is in 1:nimd) {
  Na[ia] = Na[ia] + 1/oNg[(is-1)*na + ia] 
  Ns[is] = Ns[is] + 1/oNg[(is-1)*na + ia] }}
Npop = sum(1/oNg);

# pars: imd=1, age 30 to 39", 1/100,000 latent infections
E1g0 = (1/oNg)*pars$pE1g0
Sg0  = Sg0 - E1g0

## read files
byw <- readRDS(gsub('all','w',.args[1]))
byaw <- readRDS(gsub('all','aw',.args[1]))
byall <- readRDS(.args[1])

## Figures
  
ar=1 #aspect ratio

l95_func <- function(x){quantile(x, probs=0.025)}; u95_func <- function(x){quantile(x, probs=0.975)}

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

## fig 3 by age
data_age1000 <- byaw[, c('sim',paste0('IUw_a', 1:16))]
for(a in 1:16){data_age1000[, (paste0('IUw_a', a))] <- 1e3*data_age1000[, get(paste0('IUw_a', a))]/Na[a]} 
data_age1000[, time := rep(min(data$time):max(data$time), max(data_age1000$sim))]
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
ggsave(here::here('output','figures','epidem','time_series.png'), dpi=600, 
       device = "png", width = 12, height = 9)

## save
p2facet + p2bfacet + plot_layout(nrow = 2, guides = 'collect')
ggsave(here::here('output','figures','epidem','time_series_imd_facet.png'), dpi=600, 
       device = "png", width = 12, height = 8)

## save
ggsave(plot = p3facet, here::here('output','figures','epidem','time_series_age_facet.png'), dpi=600, 
       device = "png", width = 12, height = 8)

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

all_time_s <- ggplot(data[time %in% 20:75], aes(x=time)) + 
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

all_time_s_cum <- ggplot(data[time %in% 20:75], aes(x=time)) + 
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

all_time_s_cum_imd <- ggplot(data[time %in% 20:75], aes(x=time)) + 
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
  scale_fill_viridis() + 
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
ggsave(here::here('output','figures','epidem','patchwork.png'), dpi=600, 
       device = "png", width = 14, height = 16)

all_time_s + all_time_s_cum + plot_layout(nrow = 1, guides = 'collect')
ggsave(here::here('output','figures','epidem','age_x_imd.png'), dpi=600, 
       device = "png", width = 10, height = 8)

ggsave(plot = all_time_s_cum_imd, 
       here::here('output','figures','epidem','age_x_imd_cumulative.png'), dpi=600, 
       device = "png", width = 10, height = 10)

ggsave(plot = age_spec_ar, 
       .args[2],
       dpi=600, 
       device = "png", width = 10, height = 7)


