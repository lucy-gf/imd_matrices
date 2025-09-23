pars <- list()
pars <- within(pars, {
  
    Disease     <- "Influenza"
    Vaccination <- "No"
    Incidence   <- pset$Incidence
    
    #temporal
    dt     <- 0.1             #0.01 #time step (days) #smaller than Baguelin 2013 (0.25)
    times  <- 0:90      #days sequence
    nt     <- (max(times)-min(times))/dt + 1       #no. time points, iterations
    nw     <- ceiling((max(times)-min(times))/7)   #weeks length of model run
    nd     <- ceiling((max(times)-min(times)))+1   #days length of model run
    
    #demography
    ages   <- c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75+')
    na     <- length(ages)               #number of age groups
    nimd   <- 5               #number of SE groups
    ageons <- c(4883006, 4673063, 4990547, 5317066, 5596283, 5392341, 4329016, 5147128, 5241642, 4739455, 5744011, 5648873, 4852694, 4126324, 4166646, 7178232); ageons=ageons/sum(ageons) 
    
    age    <- c(2.5 + 5*(0:14), mean(c(75,100)))
    
    #natural history            
    rEI    <- 1/0.8           #latency,  Baguelin 2013
    rI1I2  <- 2/1.8           #recovery, Baguelin 2013
    rI2R   <- 2/1.8           #recovery, Baguelin 2013
    rIR    <- 1/(1/rI1I2 + 1/rI2R)
    rUR    <- 1/1.8 #rIR      #recovery, Baguelin 2013
    #rIH    <-                #hospitalisation
    #rHR    <-                #recovery rate in hospital 
    #rHD    <-                #death rate in hospital
    #rC     <-                #rate of loss of positivity

    R0     <- 1.5 #1.95            #variant adjusted from Baguelin 2013 Fig 22, 36, S52-54 2007-08
    f      <- 0 #0.5          #relative transmission of U group
    beta   <- 0.23            #
    
    #Clinical responses
    #Susceptibility - variant & age-adjusted Baguelin 2013, Fig 22, 36, S52-54 2007-08, H3N2 dominant over H1N1, B
    u   <- rep(0.5, na)
    #Critically infected fraction
    y   <- rep(1, na)    # 1 for now
    #Clinical fraction - by age and IMD group
    y45 <- rep(y,nimd)
    
    #Mortality fraction if clinically infected 
    # 0 for now
    IFR <- rep(0, na)
    m <- IFR/y
    
    #Initial condition
    #imd=1, age 30 to 34, 1/100,000 latent infections
    pE1g0   <- rep(0,na*nimd)  #initialise proportion latently infected across age x SES groups
    pE1g0[7] = (1/10^5)        #1/100,000 latent infections in age group 5 in SES 1
    
    #rate of reporting - variant & age-adjusted Baguelin 2013, Fig 22, 36, S52-54 2007-08, H3N2 dominant over H1N1, B
    rrep <- c(0.004000, 0.004000, 0.014500, 0.025000, 0.025000, 0.025000, 0.025000, 0.018125, 0.011250)
    
    #NB parameters
    #k      <- 1               #dispersion/shape par of NB likelihood - #derived sh fit

})


