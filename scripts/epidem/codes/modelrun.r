#pipeline SEIRD Rcpp

require(bench)
require(magrittr)
require(ggplot2)
require(ggtext)
require(gridExtra)
require(Rcpp)
require(tidyverse)

## Contact matrices
# cm45<-(as.matrix(read.csv(paste0(input_dir,"/Mas45_urban.csv"),header=F))) # removes name of columns
# cm45dim1 = dim(cm45)[1]

cm1000 <- data.table(suppressWarnings(read_csv(here::here('output','data','cont_matrs','fitted_matrs.csv'), show_col_types = F)))[bootstrap_index != 'bootstrap_index',]
cm1000 <- cm1000[, c('bootstrap_index','p_age_group','p_imd_q','c_age_group','c_imd_q','n')][, lapply(.SD, sum), by = c('bootstrap_index','p_age_group','p_imd_q','c_age_group','c_imd_q')]
cm1000$p_age_group <- factor(cm1000$p_age_group, levels = pars$ages); cm1000$c_age_group <- factor(cm1000$c_age_group, levels = pars$ages)
cm <- cm1000[bootstrap_index==1,] %>% 
  arrange(p_imd_q, p_age_group, c_imd_q, c_age_group) %>% 
  mutate(p = paste0(p_imd_q, '_', p_age_group),
         c = paste0(c_imd_q, '_', c_age_group)) %>% 
  select(p,c,n) %>% pivot_wider(names_from = c, values_from = n) %>% select(!p) %>% as.matrix()
cmdim1 = dim(cm)[1]

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

## R0 and average contacts
source(paste0(source_dir,"/R0_.r"))      #outputs cav
betanew = R0(pars,as.numeric(pars$R0),0) #default 2.5
cat(paste0("Assuming R0 = ", pars$R0 ,", beta = ", round(betanew,4), "/day",'\n'))

## Parameters
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
Iwpeakval = max(mas$byw$Iw)*10^(-6)
Iwpeakloc = mas$byw$time[which(mas$byw$Iw==max(mas$byw$Iw))]
cat(paste0("Peak:  ", round(Iwpeakval,3) ," million at ", Iwpeakloc, " days"))
cat("\n")

## Figures
if (pset$FIGURES==1){
  
ar=1 #aspect ratio

if(pars$Incidence=="Daily"){daily="daily_"} else {daily=""}
filename=paste0(parsum$Disease,"_",area,"_SEIRD_epidemic_",daily,pset$Namevacc,TODAY)
pdf(file=paste0(output_dir,"/",filename,".pdf"))


## fig 1 overall
data <- data.frame(time=rep(mas$byw$time,2), IUw=10^5*c(mas$byw$Iw + mas$byw$Uw)/Npop)

p1 <- ggplot(data, aes(x=time)) + 
  geom_line(aes(y = IUw), lwd=0.8, col = 'darkgreen')  +
  theme_bw() +
  theme(text=element_text(size=10),
        legend.key.size = unit(2, 'mm'),
        plot.title = element_text(size = 12),
        axis.text.y = element_text(color=1),
        axis.text.x = element_text(color=1)) +
  labs(y = "Infections /100k/week", x = "Day") 

print(p1)
if (pset$platform=="repo" & pars$Disease=="RSV-illness") p1R<-p1
if (pset$platform=="repo" & pars$Disease=="Influenza")   p1F<-p1
if (pset$platform=="repo" & pars$Disease=="COVID-19")    p1C<-p1


## fig 2 by ses
data <- data.frame(time=rep(mas$byw$time,5), 
      IUw=10^5*c(mas$byw$IUw_s1/Ns[1], mas$byw$IUw_s2/Ns[2], mas$byw$IUw_s3/Ns[3], 
                 mas$byw$IUw_s4/Ns[4], mas$byw$IUw_s5/Ns[5]),
      Iw =10^5*c(mas$byw$Iw_s1/Ns[1],  mas$byw$Iw_s2/Ns[2],  mas$byw$Iw_s3/Ns[3],  
                 mas$byw$Iw_s4/Ns[4],  mas$byw$Iw_s5/Ns[5]),
      IMD=rep(1:5,each=length(mas$byw$time)))
p2 <- ggplot(data, aes(x=time)) + 
  geom_line(aes(y = Iw, group=IMD, color=IMD), lwd=0.8)  +
  theme_bw() + 
  theme(text=element_text(size=10),
        legend.key.size = unit(2, 'mm'),
        plot.title = element_text(size = 12),
        axis.text.y = element_text(color=1),
        axis.text.x = element_text(color=1)) +
  labs(y = "Infections /100k/week", x = "Day", color = "IMD") #+ theme(aspect.ratio=ar)

print(p2)
if (pset$platform=="repo" & pars$Disease=="RSV-illness") p2R<-p2
if (pset$platform=="repo" & pars$Disease=="Influenza")   p2F<-p2
if (pset$platform=="repo" & pars$Disease=="COVID-19")    p2C<-p2


## fig 3 by age
data <- data.frame(time=rep(mas$byw$time,na), 
       IUw=10^5*c(mas$byaw$IUw_a1/Na[1], mas$byaw$IUw_a2/Na[2], mas$byaw$IUw_a3/Na[3], mas$byaw$IUw_a4/Na[4], 
                  mas$byaw$IUw_a5/Na[5], mas$byaw$IUw_a6/Na[6], mas$byaw$IUw_a7/Na[7], mas$byaw$IUw_a8/Na[8], 
                  mas$byaw$IUw_a1/Na[9], mas$byaw$IUw_a2/Na[10], mas$byaw$IUw_a3/Na[11], mas$byaw$IUw_a4/Na[12], 
                  mas$byaw$IUw_a5/Na[13], mas$byaw$IUw_a6/Na[14], mas$byaw$IUw_a7/Na[15], mas$byaw$IUw_a8/Na[16]),
        Iw=10^5*c(mas$byaw$Iw_a1/Na[1],  mas$byaw$Iw_a2/Na[2],  mas$byaw$Iw_a3/Na[3],  mas$byaw$Iw_a4/Na[4],  
                  mas$byaw$Iw_a5/Na[5],  mas$byaw$Iw_a6/Na[6],  mas$byaw$Iw_a7/Na[7],  mas$byaw$Iw_a8/Na[8],
                  mas$byaw$Iw_a1/Na[9],  mas$byaw$Iw_a2/Na[10],  mas$byaw$Iw_a3/Na[11],  mas$byaw$Iw_a4/Na[12],  
                  mas$byaw$Iw_a5/Na[13],  mas$byaw$Iw_a6/Na[14],  mas$byaw$Iw_a7/Na[15],  mas$byaw$Iw_a8/Na[16]),
        AGE=rep(pars$ages,each=length(mas$byw$time)))
data$AGE <- factor(data$AGE, levels = pars$ages)
p3 <- ggplot(data, aes(x=time)) + 
  geom_line(aes(y = Iw, group=AGE, color=AGE), lwd=0.8)  +
  theme_bw() + 
  theme(text=element_text(size=10),
        legend.key.size = unit(2, 'mm'),
        plot.title = element_text(size = 12),
        axis.text.y = element_text(color=1),
        axis.text.x = element_text(color=1)) +
  labs(y = "Infections /100k/week", x = "Day", color = "Age")  #+ theme(aspect.ratio=ar)

print(p3)
dev.off()
if (pset$platform=="repo" & pars$Disease=="RSV-illness") p3R<-p3
if (pset$platform=="repo" & pars$Disease=="Influenza")   p3F<-p3
if (pset$platform=="repo" & pars$Disease=="COVID-19")    p3C<-p3

## save

ggsave(here::here('output','figures','epidem','time_series.png'), dpi=600, 
       gridExtra::grid.arrange(p1F,p2F,p3F, nrow=3, ncol=1), device = "png")

}

## Performance

if (pset$DIAGNOSTIC==1){
   if(pset$ncomparisons==1){  #1 #2
     niter= 100 #3000 #1000
     test <- bench::mark(model(parscpp), min_iterations = niter)
     #test <- bench::mark(mas, min_iterations = niter)
     sink(file = paste0(output_dir,"/",filename,".txt"),append=FALSE,split=FALSE)
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
print(paste0("Peak:  ", round(Iwpeakval,3) ," million at ", Iwpeakloc, " days"))

cat("\n Study \n")
print(paste0("Disease:    ", parsum$Disease))
print(paste0("Area:       ", area))
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


