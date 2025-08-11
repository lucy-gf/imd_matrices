#pipeline SEIRD Rcpp

require(bench)
require(magrittr)
require(ggplot2)
require(ggtext)
require(gridExtra)
require(Rcpp)
require(tidyverse)
require(data.table)
require(patchwork)
require(viridis)
options(dplyr.summarise.inform = FALSE)

source(here::here('scripts','assign_imd','assign_imd_fcns.R'))
source(here::here('scripts','setup','colors.R'))

# set seed
set.seed(120)

## Contact matrices
if(!exists('cm1000')){
cm1000 <- data.table(suppressWarnings(read_csv(here::here('output','data','cont_matrs','fitted_matrs.csv'), show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
cm1000 <- cm1000[, c('bootstrap_index','p_age_group','p_imd_q','c_age_group','c_imd_q','n')][, lapply(.SD, sum), by = c('bootstrap_index','p_age_group','p_imd_q','c_age_group','c_imd_q')]
}
cm1000$p_age_group <- factor(cm1000$p_age_group, levels = pars$ages); cm1000$c_age_group <- factor(cm1000$c_age_group, levels = pars$ages)
cm1000 <- cm1000[order(bootstrap_index, p_imd_q, p_age_group, c_imd_q, c_age_group)]

## Parameters
if(pset$Vaccination==0){
   if(pset$Disease=="COVID-19")    source(paste0(source_dir,"/parsC_.r"))
   if(pset$Disease=="Influenza")   source(paste0(source_dir,"/parsF_.r"))
   if(pset$Disease=="RSV-illness") source(paste0(source_dir,"/parsR_.r"))
}else{
   if(pset$Disease=="COVID-19")    source(paste0(source_dir,"/parsCv_.r"))
   if(pset$Disease=="Influenza")   source(paste0(source_dir,"/parsFv_.r"))
   if(pset$Disease=="RSV-illness") source(paste0(source_dir,"/parsRv_.r"))
}
cat(paste0("Disease: ", pars$Disease),'\n')
cat(paste0("Vaccination: ", pars$Vaccination),'\n')
cat(paste0("Incidence: ", pars$Incidence),'\n')

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

## run model x1000
n_bs <- max(cm1000$bootstrap_index)
byw <- data.table(); byaw <- data.table(); byall <- data.table()
betatrack <- rep(0, n_bs)

for(sim_num in 1:n_bs){
  
  cm <- cm1000[bootstrap_index == sim_num,] 
  cm$p_age_group <- factor(cm$p_age_group, levels = pars$ages); cm$c_age_group <- factor(cm$c_age_group, levels = pars$ages)
  cm <- cm[order(bootstrap_index, p_imd_q, p_age_group, c_imd_q, c_age_group)]
  cm <- cm %>% 
    mutate(p = paste0(p_imd_q, '_', p_age_group),
           c = paste0(c_imd_q, '_', c_age_group)) %>% 
    select(p,c,n) %>% pivot_wider(names_from = c, values_from = n) 
  pvec <- cm$p 
  cm <- cm %>% select(!p) %>% as.matrix()
  if(sum(colnames(cm) == pvec) != length(pvec)){warning('CM names wrong')}
  if(grepl('14', pvec[2])){warning('CM names wrong')}
  cmdim1 = dim(cm)[1]
  
  ## R0, set new beta
  if(!pset$R0fixed){betanew <- pars$beta}else{
    source(paste0(source_dir,"/R0_.r"))      #outputs cav
    betanew = R0(pars, R0assumed = as.numeric(pars$R0), printout = 0) #default 2.5
    betatrack[sim_num] <- betanew
    # cat(paste0("Assuming R0 = ", pars$R0 ,", beta = ", round(betanew,4), "/day",'\n'))
  }
  
  parscpp = within(parscpp <- pars, {
    cm=as.vector(cm); cmdim1=cmdim1; mI=pars$m; beta=betanew;
    Sg0=Sg0; E1g0=E1g0; I1g0=I1g0; I2g0=I2g0; U1g0=U1g0; U2g0=U2g0; 
    Rg0=Rg0; Dg0=Dg0; oNg=oNg })
  #  for output
  parsum = parscpp;
  #  remove what's not needed for Rcpp
  parscpp <- parscpp %>% magrittr::inset(c('age', 'ages', 'ageons', 'm'), NULL)  #parscpp45[['age']] <- NULL; etc
  
  ## Model output (for the proposed parameters)
  if (pset$COMPILE==1 & pset$Vaccination==0) {
    if(pset$DailyIncidence==0) sourceCpp(file = paste0(source_dir,"/","SEIRDas_.cpp"))
    if(pset$DailyIncidence==1) sourceCpp(file = paste0(source_dir,"/","SEIRDasday_.cpp")) }
  if (pset$COMPILE==1 & pset$Vaccination==1) {
    if(pset$DailyIncidence==0) sourceCpp(file = paste0(source_dir,"/","SEIRDasvacc_.cpp"))
    if(pset$DailyIncidence==1) sourceCpp(file = paste0(source_dir,"/","SEIRDasvaccday_.cpp")) }
  
  mas <- model(parscpp)
  
  byw <- rbind(byw, 
               cbind(mas$byw, sim = sim_num))
 
  byaw <- rbind(byaw, 
                cbind(mas$byaw, sim = sim_num))
  
  byall <- rbind(byall, 
                 cbind(mas$byall, sim = sim_num))
  
  if(sim_num == 1){cat('Simulations: ')}
  if(sim_num %% 50 == 0){cat(paste0(sim_num, ', '))}
  
}

## save files
write_rds(byw, here::here(output_dir,'data','epidem','byw.rds'))
write_rds(byaw, here::here(output_dir,'data','epidem','byaw.rds'))
write_rds(byall, here::here(output_dir,'data','epidem','byall.rds'))

Iwpeakvalvec = byw[, c('sim','Iw')][, lapply(.SD, max), by = 'sim']
cat('\n',paste0("Peak:  ", signif(mean(Iwpeakvalvec$Iw),digits=3)*10^(-6)," million (95% CI: ", 
                signif(quantile(Iwpeakvalvec$Iw, 0.025),3)*10^(-6),
                ' - ', signif(quantile(Iwpeakvalvec$Iw, 0.975),3)*10^(-6), ' million)'), sep = '')
cat('\n',paste0("Beta:  ", signif(mean(betatrack),digits=3)," (95% CI: ", 
                signif(quantile(betatrack, 0.025),3),
                ' - ', signif(quantile(betatrack, 0.975),3), ')'), sep = '')

## imd check
imd1 <- colSums(byw[, paste0('Iw_s', 1:5)])
imd2raw <- colSums(byall[, paste0('Iw_g', 1:80)]); imd2 <- rep(0, nimd)
for(imd_i in 1:nimd){imd2[imd_i] <- sum(imd2raw[16*(imd_i - 1) + (1:16)])}
if(sum(abs(imd1 - imd2) < 1) != nimd){stop('IMD sums not aligning')}

## age check
age1 <- colSums(byaw[, paste0('Iw_a', 1:16)])
age2raw <- colSums(byall[, paste0('Iw_g', 1:80)]); age2 <- rep(0, na)
for(age_i in 1:na){age2[age_i] <- sum(age2raw[(age_i - 16) + 16*1:5])}
if(sum(abs(age1 - age2) < 1) != na){stop('Age sums not aligning')}

## Figures
if (pset$FIGURES==1){
  
ar=1 #aspect ratio

if(pars$Incidence=="Daily"){daily="daily_"} else {daily=""}
filename=paste0(output_dir, '/data/', parsum$Disease,"_SEIRD_epidemic_",daily)

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

all_time_s + all_time_s_cum + all_time_s_cum_hm + all_time_s_cum_imd + plot_layout(nrow = 2, guides = 'collect')
ggsave(here::here('output','figures','epidem','patchwork.png'), dpi=600, 
       device = "png", width = 14, height = 16)

}

## Performance

if (pset$DIAGNOSTIC==1){
   if(pset$ncomparisons==1){  #1 #2
     niter= 100 #3000 #1000
     test <- bench::mark(model(parscpp), min_iterations = niter)
     #test <- bench::mark(mas, min_iterations = niter)
     sink(file = paste0(filename,".txt"),append=FALSE,split=FALSE)
        cat(paste0("Iterations = ", niter),'\n')
        print(test[1:11]) #cut last 2 columns
     sink()
     pdf(file=here::here('output','figures','epidem','performance.pdf'))
        print(plot(test))
        ## plot time vs memory allocation
        #  https://bench.r-lib.org/
        #  library(tidyr)
        print(test %>% unnest(c(time, gc)) %>%
                 filter(gc == "none") %>%
                 mutate(expression = as.character(expression)) %>%
                 ggplot(aes(x = mem_alloc, y = time, color = expression)) + geom_point())
     dev.off()
     
     } else { #For comparisons
     #sourceCpp(file = paste0(source_dir,"/","SEIURDasv_.cpp"))
     niter= 3000 #1000
     test <- bench::mark(model(parscpp), model2(parscpp), min_iterations = 3000) #1000)
     sink(file = paste0(output_dir,"/",filename,".txt"),append=FALSE,split=FALSE)
        cat(paste0("Iterations = ", niter),'\n')
        summary(test, relative = TRUE)
        summary(test, relative = FALSE)
     sink()
     pdf(file=paste0(output_dir,"/",filename,".pdf"))
        plot(test)
        ## plot time vs memory allocation
        test %>% unnest(c(time, gc)) %>%
                 filter(gc == "none") %>%
                 mutate(expression = as.character(expression)) %>%
                ggplot(aes(x = mem_alloc, y = time, color = expression)) + geom_point()
     dev.off()
     
  }#comparisons
  
  ##Profiling
  #sourceCpp(file = paste0(source_dir,"/","SEIRDa.cpp"))
  #profvis::profvis(SEIRDa(parscpp))
  #=> "Error in parse_rprof_lines(lines, expr_source) : 
  #    No parsing data available. Maybe your function was too fast?"
  
}#Diagnostic



## Text summary

if (pset$SUMMARY==1){

sink(file = here::here('output','figures','epidem','out.txt'),append=FALSE,split=FALSE)

cat("\n")

cat("\n Iw peak \n")
print(paste0("Peak:  ", signif(mean(Iwpeakvalvec$Iw),digits=3)*10^(-6)," million (95% CI: ", 
                signif(quantile(Iwpeakvalvec$Iw, 0.025),3)*10^(-6),
                ' - ', signif(quantile(Iwpeakvalvec$Iw, 0.975),3)*10^(-6), ' million)'))

cat("\n Study \n")
print(paste0("Disease:    ", parsum$Disease))
print(paste0("Population: ", Npop))
print(paste0("Age groups: ", parsum$na))
print(paste0("SE  groups: ", parsum$nimd))
print(paste0("All groups: ", parsum$na*parsum$nimd))
print(paste0("Age distribution: ")); print(pa*Npop)
print(paste0("Age proportions:  ")); print(round(pa,4))
print(paste0("Ages:             ")); print(parsum$ages)
print(paste0("Age (median):     ")); print(parsum$age)
print(paste0("Reporting rate:   ")); print(parsum$rrep)

cat("\n Natural history \n")
print(paste0("Assuming R0 = ", parsum$R0))
print(paste0("  then beta = (1/day) ", round(betanew,4) ))
print(paste0("latent period (tEI, tEU),   days:    ", 1/parsum$rEI))
print(paste0("infectious period (rIR),    days:    ", 1/parsum$rIR))
print(paste0("infectious period (rUR),    days:    ", 1/parsum$rUR))
print(paste0("infectious preclin (rI1I2), days:    ", 1/parsum$rI1I2))
print(paste0("infectious clinical (rI2R), days:    ", 1/parsum$rI2R))
print(paste0("relative subclinical infectiousness: ", parsum$f))
print(paste0("susceptibility by age     : ")); print(parsum$u)
print(paste0("clinical  fraction by age : ")); print(parsum$y)
print(paste0("mortality fraction by age : ")); print(parsum$mI)

cat("\n Initial condition \n");
print(paste0("Initial latent proportion pE1g0: ")); print(as.numeric(parsum$pE1g0))
print(paste0("Initial latent infections  E1g0: ")); print(as.numeric(parsum$pE1g0*(1/oNg)))
print(paste0("Initial susceptible         Sg0: ")); print(Sg0)

cat("\n Temporal \n")
print(paste0("Time range:       ", range(pars$times)))
print(paste0("Number of weeks:  ", pars$nw))
print(paste0("Number of points: ", pars$nt))
print(paste0("dt:               ", pars$dt))

cat("\n Contacts \n")
print(paste0("Contact data: Polymod 2005"))
print(paste0("Average contact rate of cm45: ", round(cav,3)))
print(paste0("Contact matrix: ", parsum$cmdim1, " x ", parsum$cmdim1))
print(paste0("Contact matrix: ")); #cm

if (pset$Vaccination==1){
cat("\n Vaccination \n")
print(paste0("Coverage:         ")); print(parsum$vcov)
print(paste0("Efficacy:         ")); print(parsum$veff)
print(paste0("Vaccination rate: ", round(parsum$rV,5)))
print(paste0("Reduce clin frac: ", round(parsum$vcln,5))) }


cat("\n")
sink()

cat("\n")

}##SUMMARY


