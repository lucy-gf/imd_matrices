# R0 and beta
# COVID-19, Influenza
#
# Requires cm45, pars


## Demography
na   <- pars$na
nimd <- pars$nimd

## Average contact rate of cm45
#   contact rate of participant across contacts
cp <- vector(); for (i in 1:(na*nimd)){ cp[i]=sum(cm[i,])}
#   population proportion across age x imd strata
pa0 <- demog$Population
pa0 <- pa0/sum(pa0)

cav = sum(pa0*cp)
cat(paste0("Average daily contacts: ", round(cav,5)),'\n') #[1] 10.80047

## R0 and beta #################################################################

## NGM
#Erlang has no effect on R0 without vital dynamics (mu=0)
#https://www.biorxiv.org/content/10.1101/319574v1.full.pdf

R0 <- function(pars,R0assumed=2.5,printout=1){
  
ng    = na*nimd
u45   = rep(pars$u,nimd)
y45   = pars$y45
orIR  = 1/pars$rIR          
orUR  = 1/pars$rUR          
beta0 = pars$beta            
fu    = pars$f              
ngm   = cm

for (k in 1:ng){ 
  y_k=y45[k]
  for (j in 1:ng) {
    ngm[j,k] = beta0*u45[j]*cm[j,k]*( y_k*orIR + fu*(1-y_k)*orUR) }}

# max EV
EVs = eigen(ngm)$values
R00 = max(Re(EVs[which(Im(EVs)==0)]))

# beta given R0assumed
beta = R0assumed/(R00/beta0)
if(printout==1) print(paste0("Assuming R0 = ", R0assumed, ", then beta = ", round(beta,4), " /day"))

return(beta)
}


#beta = R0(pars)

