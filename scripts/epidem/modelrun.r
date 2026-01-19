# run epidemics

suppressPackageStartupMessages(require(bench))
suppressPackageStartupMessages(require(magrittr))
suppressPackageStartupMessages(require(gridExtra))
suppressPackageStartupMessages(require(Rcpp))
suppressPackageStartupMessages(require(tidyverse))
suppressPackageStartupMessages(require(data.table))
options(dplyr.summarise.inform = FALSE)
library(purrr)

#### SET UP ####

.args <- if (interactive()) c(
  file.path("output", "data", "cont_matrs","base","fitted_matrs_balanced.csv"),
  'base',
  file.path("output", "data", "epidem","base","byall.rds")
) else commandArgs(trailingOnly = TRUE)

sens_analysis <- .args[2]

if(!file.exists(gsub('/byall.rds','',.args[3]))){dir.create(gsub('/byall.rds','',.args[3]))}

# source colors etc.
source(here::here('scripts','assign_imd','assign_imd_fcns.R'))
source(here::here('scripts','setup','colors.R'))

### Basic setting
source_dir <- "scripts/epidem"
source(here::here(source_dir,'setup.r')) #repo
if(exists('cm1000')){rm(cm1000)}

### Diseases cycle
pset$Disease <- "Influenza"

# set seed
set.seed(120)

#### FUNCTION ####

run_epidemic <- function(
    sens_analysis_input,
    cm_1000,
    init_infected = 1e3,
    regional_SA = NULL,
    region = NULL,
    save = T,
    output_file
){
  
  ## Demography
  age_structure_num <- ifelse(sens_analysis_input != 'nhs_ages', 1, 2)
  
  if(sens_analysis_input == 'regional'){
    
    demog_allreg <- read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F) %>% 
      mutate(p_engreg = case_when(
        grepl('London',p_engreg) ~ 'Greater London',
        grepl('Yorkshire',p_engreg) ~ 'Yorkshire and the Humber',
        T ~ p_engreg
      ),
      IMD = imd_quintile,
      population = pop,
      Age = age_grp) %>% 
      select(p_engreg, IMD, Age, population) %>% 
      group_by(p_engreg, IMD, Age) %>% 
      summarise(Population = sum(population)) %>% ungroup() %>% 
      group_by(p_engreg, IMD) %>% 
      mutate(tot_pop = sum(Population)) %>% ungroup() %>% 
      mutate(Proportion = Population/tot_pop)
    
    demog_allreg$Age <- factor(demog_allreg$Age,
                               levels = names(colors_p_age_group))
    demog_allreg <- demog_allreg %>% arrange(p_engreg, IMD, Age)
    
    tot_pop <- sum(demog_allreg$Population)
    
    # filter demography for specific region
    demog <- demog_allreg %>% filter(p_engreg == region)
    
    # filter contact matrix for specific region
    cm_1000 <- cm_1000[p_engreg == region]
    cm_1000[, p_engreg := NULL]
    
  }else{
   
    demog <- read_csv(file.path("data","imd_25",paste0("imd_ages_", age_structure_num,".csv")), show_col_types = F) %>% 
      group_by(imd_quintile, age_grp) %>% summarise(population = sum(pop)) %>% 
      group_by(imd_quintile) %>% mutate(tot_pop = sum(population)) %>% 
      group_by(imd_quintile, age_grp, tot_pop) %>% summarise(Population = sum(population)) %>% 
      mutate(Proportion = Population/tot_pop) %>% rename(Age = age_grp, IMD = imd_quintile) 
    demog$Age <- factor(demog$Age,
                        levels = age_labels)
    demog <- demog %>% arrange(IMD, Age)
    
    tot_pop <- sum(demog$Population)

  }
  
  # input for parsF_.R
  demog_population <<- demog$Population
  
  ## Parameters
  source(paste0(source_dir,"/parsF_.r"))
  
  if(sens_analysis_input == 'regional'){
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
  }
  
  cat("\nDisease: ", pars$Disease,' --- ', sep = '')
  cat("Vaccination: ", pars$Vaccination,' --- ', sep = '')
  cat("Incidence: ", pars$Incidence,' --- ', sep = '')
  if(!pset$R0fixed){cat("R0 not fixed, beta: ", pars$beta,'\n', sep = '')}else{
    cat("R0: ", pars$R0,'\n', sep = '')
  }
  
  ## If R0 low, make runtime longer
  if(pset$R0fixed & (pars$R0 <= 1.1)){ 
    
    pars$times  <- 0:600     #days sequence
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
  
  # reorder/level contact matrix data table
  cm_1000$p_age_group <- factor(cm_1000$p_age_group, levels = pars$ages)
  cm_1000$c_age_group <- factor(cm_1000$c_age_group, levels = pars$ages)
  
  cm_1000 <- cm_1000[order(bootstrap_index, p_imd_q, p_age_group, c_imd_q, c_age_group)]
  
  # number of age groups
  na   = pars$na
  # number of SES
  nimd = pars$nimd
  # number of groups
  ng   = na*nimd
  
  # proportion by age
  pa<-vector(); for (i in 1:na){pa[i]=sum(demog$Population[which(demog$Age==pars$ages[i])])/sum(demog$Population)}
  
  if(sens_analysis_input != 'nhs_ages' & !grepl('9', demog$Age[2])){warning('Demog names wrong')}
  if(sens_analysis_input == 'nhs_ages' & !grepl('11', demog$Age[2])){warning('Demog names wrong')}
  
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
  
  # seed the initial 1,000 latent infections proportionately across groups
  prop <- init_infected/tot_pop
  pars$pE1g0 <- rep(prop, length(E1g0))
  E1g0 = (1/oNg)*pars$pE1g0
  Sg0  = Sg0 - E1g0
  
  ## Population by age group (over SES), by SES (over age), overall
  Na<-rep(0,na)
  Ns<-rep(0,nimd)
  for (ia in 1:na) { for (is in 1:nimd) {
    Na[ia] = Na[ia] + 1/oNg[(is-1)*na + ia] 
    Ns[is] = Ns[is] + 1/oNg[(is-1)*na + ia] }}
  Npop = sum(1/oNg);
  
  # set dataframes etc. to 0
  n_bs <- max(cm_1000$bootstrap_index)
  byw <- data.table(); byaw <- data.table(); byall <- data.table()
  BETATRACK <- rep(0, n_bs)
  
  ## run model x1000
  for(sim_num in 1:n_bs){
    
    cm <- cm_1000[bootstrap_index == sim_num,] 
    cm$p_age_group <- factor(cm$p_age_group, levels = pars$ages)
    cm$c_age_group <- factor(cm$c_age_group, levels = pars$ages)
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
      betanew = R0(pars, cm_in = cm, R0assumed = as.numeric(pars$R0), printout = 0) #default 2.5
      BETATRACK[sim_num] <- betanew
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
    if (pset$COMPILE==1 & pset$Vaccination==0 & pset$DailyIncidence==1) {
      sourceCpp(file = paste0(source_dir,"/cpp/","SEIRDasday_",ng,".cpp")) 
    }
    
    mas <- model(parscpp)
    
    byw <- rbind(byw, 
                 cbind(mas$byw, sim = sim_num, beta = betanew))
    
    byaw <- rbind(byaw, 
                  cbind(mas$byaw, sim = sim_num, beta = betanew))
    
    byall <- rbind(byall, 
                   cbind(mas$byall, sim = sim_num, beta = betanew))
    
    if(sim_num == 1){cat('Simulations: ')}
    if(sim_num %% 50 == 0){cat(paste0(sim_num, ', '))}
    
  }
  
  Iwpeakvalvec = byw[, c('sim','Iw')][, lapply(.SD, max), by = 'sim']
  cat('\n',paste0("Peak:  ", signif(mean(Iwpeakvalvec$Iw),digits=3)*10^(-6)," million (95% CI: ", 
                  signif(quantile(Iwpeakvalvec$Iw, 0.025),3)*10^(-6),
                  ' - ', signif(quantile(Iwpeakvalvec$Iw, 0.975),3)*10^(-6), ' million)'), sep = '')
  if(pset$R0fixed){
    cat('\n',paste0("Beta:  ", signif(mean(BETATRACK),digits=3)," (95% CI: ", 
                    signif(quantile(BETATRACK, 0.025),3),
                    ' - ', signif(quantile(BETATRACK, 0.975),3), ')\n'), sep = '')
  }
  
  ## check sums
  imd_vec <- paste0('Iw_s', 1:nimd)
  age_vec <- paste0('Iw_a', 1:na)
  group_vec <- paste0('Iw_g', 1:ng)
  
  ## imd check
  imd1 <- colSums(byw[, ..imd_vec])
  imd2raw <- colSums(byall[, ..group_vec]); imd2 <- rep(0, nimd)
  for(imd_i in 1:nimd){imd2[imd_i] <- sum(imd2raw[na*(imd_i - 1) + (1:na)])}
  if(sum(abs(imd1 - imd2) < 1) != nimd){stop('IMD sums not aligning')}
  
  ## age check
  age1 <- colSums(byaw[, ..age_vec])
  age2raw <- colSums(byall[, ..group_vec]); age2 <- rep(0, na)
  for(age_i in 1:na){age2[age_i] <- sum(age2raw[(age_i - na) + na*1:5])}
  if(sum(abs(age1 - age2) < 1) != na){stop('Age sums not aligning')}
  
  ## attach region if in regional sensitivity analysis
  if(sens_analysis == 'regional'){
    
    byw <- byw %>% mutate(p_engreg = reg)
    byaw <- byaw %>% mutate(p_engreg = reg)
    byall <- byall %>% mutate(p_engreg = reg)
    
    output_file <- gsub('.rds', paste0('_', regional_SA, '_', gsub(' ', '_', reg), '.rds'), output_file)
    
  }
  
  ## save files
  if(save){
    
    write_rds(byw, gsub('all','w',output_file))
    write_rds(byaw, gsub('all','aw',output_file))
    write_rds(byall, output_file)
    
    return()
  
    }else{
      
    return(list(byw,
                byaw,
                byall))
      
  }
  
}

#### INPUTS ####
# set ages
if(sens_analysis == 'nhs_ages'){
  age_limits <- c(5,12,18,26,35,50,70,80)
  age_labels <- paste0(c(0,age_limits), c(rep('-', length(age_limits)),''), c(age_limits - 1, '+'))
}

## contact matrices
cm_col_vec <- c('bootstrap_index','p_imd_q','p_age_group','c_imd_q','c_age_group','n')
if(sens_analysis == 'regional'){ cm_col_vec <- c(cm_col_vec, 'p_engreg') }
sum_over <- cm_col_vec[!cm_col_vec=='n']

if(!exists('cm1000')){
  cm1000 <- data.table(suppressWarnings(read_csv(.args[1], show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
  if(length(setdiff(colnames(cm1000), cm_col_vec)) != 0){print(colnames(cm1000))}
  cm1000 <- cm1000[, ..cm_col_vec][, lapply(.SD, sum), by = sum_over]
}

#### RUN ####

if(sens_analysis == 'regional'){
  
  for(regional_SA_i in 1:4){
    
    for(reg in unique(cm1000$p_engreg)){
      
      cat('\nRegion: ', reg, '\n', sep = '')
      
      epidemic_outputs <- run_epidemic(
        sens_analysis_input = sens_analysis,
        cm_1000 = cm1000,
        region = reg,
        regional_SA = regional_SA_i,
        save = T,
        output_file = .args[3]
      )
      
    }
    
  }
  
  write_rds(data.frame(x = 0), .args[3])
  
}else{
  
  epidemic_outputs <- run_epidemic(
    sens_analysis_input = sens_analysis,
    cm_1000 = cm1000,
    save = T,
    output_file = .args[3]
  )
  
}  

   



