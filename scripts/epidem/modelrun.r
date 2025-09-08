#pipeline SEIRD Rcpp

suppressPackageStartupMessages(require(bench))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(gridExtra))
suppressPackageStartupMessages(require(Rcpp))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(data.table))
options(dplyr.summarise.inform = FALSE)

.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","fitted_matrs_balanced.csv"),
  file.path("output", "data", "epidem","byall.rds")
) else commandArgs(trailingOnly = TRUE)

# source colors etc.
source(here::here('scripts','assign_imd','assign_imd_fcns.R'))
source(here::here('scripts','setup','colors.R'))

### Basic setting
source_dir <- "scripts/epidem"
source(here::here(source_dir,'setup.r')) #repo

### Diseases cycle
pset$Disease <- "Influenza"

# set seed
set.seed(120)

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
cat("Disease: ", pars$Disease,' --- ', sep = '')
cat("Vaccination: ", pars$Vaccination,' --- ', sep = '')
cat("Incidence: ", pars$Incidence,' --- ', sep = '')
if(!pset$R0fixed){cat("R0 not fixed, beta: ", pars$beta,'\n', sep = '')}else{
  cat("R0: ", pars$R0,'\n', sep = '')
}

## If R0 low, make runtime longer
if(pset$R0fixed & (pars$R0 < 1.1)){ 
  
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

## Contact matrices
if(!exists('cm1000')){
  cm1000 <- data.table(suppressWarnings(read_csv(.args[1], show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
  cm1000 <- cm1000[, c('bootstrap_index','p_age_group','p_imd_q','c_age_group','c_imd_q','n')][, lapply(.SD, sum), by = c('bootstrap_index','p_age_group','p_imd_q','c_age_group','c_imd_q')]
}
cm1000$p_age_group <- factor(cm1000$p_age_group, levels = pars$ages); cm1000$c_age_group <- factor(cm1000$c_age_group, levels = pars$ages)
cm1000 <- cm1000[order(bootstrap_index, p_imd_q, p_age_group, c_imd_q, c_age_group)]

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
    if(pset$DailyIncidence==0) sourceCpp(file = paste0(source_dir,"/cpp/","SEIRDas_.cpp"))
    if(pset$DailyIncidence==1) sourceCpp(file = paste0(source_dir,"/cpp/","SEIRDasday_.cpp")) }
  if (pset$COMPILE==1 & pset$Vaccination==1) {
    if(pset$DailyIncidence==0) sourceCpp(file = paste0(source_dir,"/cpp/","SEIRDasvacc_.cpp"))
    if(pset$DailyIncidence==1) sourceCpp(file = paste0(source_dir,"/cpp/","SEIRDasvaccday_.cpp")) }
  
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

Iwpeakvalvec = byw[, c('sim','Iw')][, lapply(.SD, max), by = 'sim']
cat('\n',paste0("Peak:  ", signif(mean(Iwpeakvalvec$Iw),digits=3)*10^(-6)," million (95% CI: ", 
                signif(quantile(Iwpeakvalvec$Iw, 0.025),3)*10^(-6),
                ' - ', signif(quantile(Iwpeakvalvec$Iw, 0.975),3)*10^(-6), ' million)'), sep = '')
if(pset$R0fixed){
  cat('\n',paste0("Beta:  ", signif(mean(betatrack),digits=3)," (95% CI: ", 
                  signif(quantile(betatrack, 0.025),3),
                  ' - ', signif(quantile(betatrack, 0.975),3), ')\n'), sep = '')
}

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

## save files
write_rds(byw, gsub('all','w',.args[2]))
write_rds(byaw, gsub('all','aw',.args[2]))
write_rds(byall, .args[2])



