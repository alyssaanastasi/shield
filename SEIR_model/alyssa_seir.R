library(deSolve)
library(tidyverse)

#### SEIR model ####

seir <- function(t, y, pars){
  # For COVID, Flu, & RSV
  # S -> E -> I -> R -> S2
  #        -> H (Hospitalized/Severe) -> R OR D
  # Parameters
  mu <- pars$mu[1]  # probability of transition given contact
  epsilon <- pars$epsilon[1]  # how long it takes to move from e to i
  probCH1 <- pars$probCH1[1] #probability of severe infection/hospitalization with no prior immunity
  probCH2 <- pars$probCH2[1] #probability of severe infection/hospitalization with prior immunity
  probAH1 <- pars$probAH1[1] 
  probAH2 <- pars$probAH2[1] 
  probSH1 <- pars$probSH1[1] 
  probSH2 <- pars$probSH2[1] 
  gamma <- pars$gamma[1] # recovery rate
  omega <- pars$omega[1] # waning of immunity 
  b <- pars$b[1] # immunity scalar (people with previous infection/vaccination have longer waning of immunity)
  alphaC <- pars$alphaC[1] # infection-induced death rate for children
  alphaA <- pars$alphaA[1] #infection induced death rate for adults (both parents and CA)
  alphaS <- pars$alphaS[1]
  cCC <- pars$cCC[1] # contact between children 
  cCCA <- pars$cCCA[1] # contact between children and childless adults
  cCP <- pars$cCP[1] # contact between children and adults with children
  cCS <- pars$cCS[1] # contact between seniors and children
  cAA <- pars$cAA[1] # contact between adults
  cSA <- pars$cSA[1] # contact between seniors and adults
  cSS <- pars$cSS[1] # contact between seniors
  ss <- pars$ss[1] # scale susceptibility -> people who have already had covid are less susceptible
  si <- pars$si[1] # scale infectiousness -> people with immunity are less infectious
  vaccC <- pars$vaccC #child vaccination
  vaccCA <- pars$vaccCA #childless adult vaccination
  vaccP <- pars$vaccP #parent vaccination
  vaccS <- pars$vaccS #senior vaccination
  
  # Set up Population Parameters
  # Source: Illinois 2020 Census Data
  percentAdult <- 5398257/12812508
  percentParent <- (883257+109092+70752+265126)/4998395
  percentAdultParent <- percentParent / percentAdult
  
  # State variables:
  # 4 groups: Children, Adults with Kids, Adults without Kids, Seniors 
  # Children 
  S_C1 <- y[1] # no immunity
  E_C1 <- y[2]
  I_C1 <- y[3]
  H_C1 <- y[4]
  R_C1 <- y[5]
  D_C1 <- y[6]
  
  S_C2 <- y[7] # some immunity (prior infection, vaccination, etc)
  E_C2 <- y[8]
  I_C2 <- y[9]
  H_C2 <- y[10]
  R_C2 <- y[11]
  D_C2 <- y[12]
  
  # Childless Adults
  S_CA1 <- y[13] # no immunity
  E_CA1 <- y[14]
  I_CA1 <- y[15]
  H_CA1 <- y[16]
  R_CA1 <- y[17]
  D_CA1 <- y[18]
  
  S_CA2 <- y[19] # some immunity (prior infection, vaccination, etc)
  E_CA2 <- y[20]
  I_CA2 <- y[21]
  H_CA2 <- y[22]
  R_CA2 <- y[23]
  D_CA2 <- y[24]
  
  # Adults with Children (Parents)
  S_P1 <- y[25] # no immunity
  E_P1 <- y[26]
  I_P1 <- y[27]
  H_P1 <- y[28]
  R_P1 <- y[29]
  D_P1 <- y[30]
  
  S_P2 <- y[31] # some immunity (prior infection, vaccination, etc)
  E_P2 <- y[32]
  I_P2 <- y[33]
  H_P2 <- y[34]
  R_P2 <- y[35]
  D_P2 <- y[36]
  
  # Seniors 
  S_S1 <- y[37] # no immunity
  E_S1 <- y[38]
  I_S1 <- y[39]
  H_S1 <- y[40]
  R_S1 <- y[41]
  D_S1 <- y[42]
  
  S_S2 <- y[43] # some immunity (prior infection, vaccination, etc)
  E_S2 <- y[44]
  I_S2 <- y[45]
  H_S2 <- y[46]
  R_S2 <- y[47]
  D_S2 <- y[48]
  
  # Group totals
  sumIC <- I_C1 + si*I_C2 + H_C1 + si*H_C2 #As of right now, this assumes hospitalized people are just as infectious as normal infected people ?? 
  popC <- S_C1 + E_C1 + I_C1 + H_C1 + R_C1 + S_C2 + E_C2 + I_C2 + H_C2 + R_C2
  sumICA <- I_CA1 + si*I_CA2 + H_CA1 + si*H_CA2
  popCA <- S_CA1 + E_CA1+ I_CA1 + H_CA1 + R_CA1 + S_CA2 + E_CA2 + I_CA2 + H_CA2 + R_CA2
  sumIP <- I_P1 + si*I_P2 + H_P1 + si*H_P2
  popP <- S_P1 + E_P1+ I_P1 + H_P1 + R_P1 + S_P2 + E_P2 + I_P2 + H_P2 + R_P2
  sumIS <- I_S1 + si*I_S2 + H_S1 + si*H_S2
  popS <- S_S1 + E_S1+ I_S1 + H_S1 + R_S1 + S_S2 + E_S2+ I_S2 + H_S2 + R_S2
  sumI_Adults <- sumICA + sumIP
  popAdults <- popCA + popP
  
  # force of infection: 
  # Lambda = Child to Adult contact * mu * prop of adults infected
  lambdaC <- mu*(cCC*sumIC/popC + cCCA*sumICA/popCA + cCP*sumIP/popP + cCS*sumIS/popS)
  lambdaCA <- mu*(cAA*sumI_Adults/popAdults + cCCA*sumIC/popC + cSA*sumIS/popS)
  lambdaP <- mu*(cAA*sumI_Adults/popAdults + cCP*sumIC/popC + cSA*sumIS/popS)
  lambdaS <- mu*(cSS*sumIS/popS + cCS*sumIC/popC + cSA*sumI_Adults/popAdults)
  
  #Aging Rates
  deltaCA <- 1/(18*365) # aging rate from child to adult
  deltaAS <- 1/(32*365) # aging rate from adult to senior
  deltaSC <- 1/(30*365) # aging rate from senior to child (birth rate?)
    
  ## Updates
  #child updates
  dS_C1 <- -(lambdaC + vaccC(t))*S_C1 - deltaCA*S_C1 + deltaSC*popS
  dE_C1 <- lambdaC*S_C1 - (epsilon + vaccC(t))*E_C1 - deltaCA*E_C1 
  dI_C1 <- (1-probCH1)*epsilon*E_C1 - gamma*I_C1 - (deltaCA)*I_C1 
  dH_C1 <- probCH1*epsilon*E_C1 - gamma*H_C1 - (deltaCA)*H_C1 
  dR_C1 <- gamma*I_C1 + (1-alphaC1)*gamma*H_C1 - (omega + vaccC(t))*R_C1 - (deltaCA)*R_C1 
  dD_C1 <- alphaC1*gamma*H_C1
  
  dS_C2 <- omega*R_C1 + b*omega*R_C2 - (ss*lambdaC + vaccC(t))*S_C2 - (deltaCA)*S_C2 
  dE_C2 <- ss*lambdaC*S_C2 - (epsilon + vaccC(t))*E_C2 - (deltaCA)*E_C2
  dI_C2 <- (1-probCH2)*epsilon*E_C2 - gamma*I_C2 - (deltaCA)*I_C2
  dH_C2 <- probCH2*epsilon*E_C2 - gamma*H_C2 - (deltaCA)*H_C2
  dR_C2 <- vaccC(t)*(S_C1 + E_C1 + R_C1 + S_C2 + E_C2) + gamma*I_C2 - b*omega*R_C2 - (deltaCA)*R_C2
  dD_C2 <- alphaC2*gamma*H_C2
  
  
  # childless adults updates
  dS_CA1 <- -(lambdaCA + vaccCA(t)) * S_CA1 + (deltaCA * (1-percentAdultParent))*S_C1 - (deltaAS)*S_CA1
  dE_CA1 <- lambdaCA* S_CA1 - (epsilon + vaccCA(t)) * E_CA1 + (deltaCA * (1-percentAdultParent))*E_C1 - (deltaAS)*E_CA1
  dI_CA1 <- epsilon * E_CA1 - gamma * I_CA1 + (deltaCA * (1-percentAdultParent))*I_C1 - (deltaAS)*I_CA1
  dR_CA1 <- gamma * I_CA1 - (omega + vaccCA(t)) * R_CA1 + (deltaCA * (1-percentAdultParent))*R_C1 - (deltaAS)*R_CA1
  dD_CA1 <- alphaA1 * gamma * I_CA1
  dS_CA2 <- omega * R_CA1 + b * omega * R_CA2 - (ss * lambdaCA + vaccCA(t)) * S_CA2 + (deltaCA * (1-percentAdultParent))*S_C2 - (deltaAS)*S_CA2
  dE_CA2 <- ss * lambdaCA * S_CA2 - (epsilon + vaccCA(t)) * E_CA2 + (deltaCA * (1-percentAdultParent))*E_C2 - (deltaAS)*E_CA2
  dI_CA2 <- epsilon * E_CA2 - gamma * I_CA2 + (deltaCA * (1-percentAdultParent))*I_C2 - (deltaAS)*I_CA2
  dR_CA2 <- vaccCA(t) * (S_CA1 + E_CA1 + R_CA1 + S_CA2 + E_CA2) + gamma * I_CA2 - b * omega * R_CA2 + (deltaCA * (1-percentAdultParent))*R_C2 - (deltaAS)*R_CA2
  dD_CA2 <- alphaA2 * gamma * I_CA2
  
  #parent updates
  dS_P1 <- -(lambdaP + vaccP(t)) * S_P1 + (deltaCA * percentAdultParent)*S_C1 - (deltaAS)*S_P1
  dE_P1 <- lambdaP * S_P1 - (epsilon + vaccP(t)) * E_P1 + (deltaCA * percentAdultParent)*E_C1 - (deltaAS)*E_P1
  dI_P1 <- epsilon * E_P1 - gamma * I_P1 + (deltaCA * percentAdultParent)*I_C1 - (deltaAS)*I_P1
  dR_P1 <- gamma * I_P1 - (omega + vaccP(t)) * R_P1 + (deltaCA * percentAdultParent)*R_C1 - (deltaAS)*R_P1
  dD_P1 <- alphaA1 * gamma * I_P1
  dS_P2 <- omega * R_P1 + b * omega * R_P2 - (ss * lambdaP + vaccP(t)) * S_P2 + (deltaCA * percentAdultParent)*S_C2 - (deltaAS)*S_P2
  dE_P2 <- ss * lambdaP * S_P2 - (epsilon + vaccP(t)) * E_P2 + (deltaCA * percentAdultParent)*E_C2 - (deltaAS)*E_P2
  dI_P2 <- epsilon * E_P2 - gamma * I_P2 + (deltaCA * percentAdultParent)*I_C2 - (deltaAS)*I_P2
  dR_P2 <- vaccP(t) * (S_P1 + E_P1 + R_P1 + S_P2 + E_P2) + gamma * I_P2 - b * omega * R_P2 + (deltaCA * percentAdultParent)*R_C2 - (deltaAS)*R_P2
  dD_P2 <- alphaA2 * gamma * I_P2
  
  
  # senior updates
  dS_S1 <- -(lambdaS + vaccS(t)) * S_S1 + (deltaAS)*(S_CA1 + S_P1) - (deltaSC)*S_S1
  dE_S1 <- lambdaS * S_S1 - (epsilon + vaccS(t)) * E_S1 + (deltaAS)*(E_CA1 + E_P1) - (deltaSC)*E_S1
  dI_S1 <- epsilon * E_S1 - gamma * I_S1 + (deltaAS)*(I_CA1 + S_P1) - (deltaSC)*I_S1
  dR_S1 <- gamma * I_S1 - (omega + vaccS(t)) * R_S1 + (deltaAS)*(R_CA1 + R_P1) - (deltaSC)*R_S1
  dD_S1 <- alphaS1 * gamma * I_S1
  dS_S2 <- omega * R_S1 + b * omega * R_S2 - (ss * lambdaS + vaccS(t)) * S_S2 + (deltaAS)*(S_CA2 + S_P2) - (deltaSC)*S_S2
  dE_S2 <- ss * lambdaS * S_S2 - (epsilon + vaccS(t)) * E_S2 + (deltaAS)*(E_CA2 + E_P2) - (deltaSC)*E_S2
  dI_S2 <- epsilon * E_S2 - gamma * I_S2 + (deltaAS)*(I_CA2 + I_P2) - (deltaSC)*I_S2
  dR_S2 <- vaccS(t) * (S_S1 + E_S1 + R_S1 + S_S2 + E_S2) + gamma * I_S2 - b * omega * R_S2 + (deltaAS)*(R_CA2 + R_P2) - (deltaSC)*R_S2
  dD_S2 <- alphaS2 * gamma * I_S2
  
  # Return list of gradients
  list(c(dS_C1, dE_C1, dI_C1, dR_C1, dD_C1,
        dS_C2, dE_C2, dI_C2, dR_C2, dD_C2,
        dS_CA1, dE_CA1, dI_CA1, dR_CA1, dD_CA1,
        dS_CA2, dE_CA2, dI_CA2, dR_CA2, dD_CA2,
        dS_P1, dE_P1, dI_P1, dR_P1, dD_P1,
        dS_P2, dE_P2, dI_P2, dR_P2, dD_P2,
        dS_S1, dE_S1, dI_S1, dR_S1, dD_S1,
        dS_S2, dE_S2, dI_S2, dR_S2, dD_S2))
  
}

#### Seasonality ###

#### Seasonal function ####
seas_matrix <- function(mu_janapr, mu_mayaug, mu_sepdec){
  n <- 365
  x <- 0:(n-1)/(n-1); #normalizing sequence
  k<- 0:6/6 # number of parameters wanted & normalized 
  # below is a cubic spline 
  matrix <- cSplineDes(x, k, ord = 4, derivs=0) %>%
    as.data.frame() %>%
    mutate(day = 1:365) %>%
    rename(JanApr = V1, MayAug = V2, SepDec = V3) %>%
    pivot_longer(JanApr:SepDec, values_to = "Value", names_to = "Season") %>%
    mutate(spline_scalar = case_when(
      Season == "JanApr" ~ mu_janapr,
      Season == "MayAug" ~ mu_mayaug,
      Season == "SepDec" ~ mu_sepdec,
    )) %>%
    mutate(Value = spline_scalar*Value) %>%
    group_by(day) %>% 
    summarise(Value = mean(Value)) %>%
    ungroup() %>%
    mutate(day = if_else(day == 365, 0, day))
  return(matrix)
}

seas_function <- function(t, matrix){
  matrix <- matrix %>%
    filter(day == t %% 365)
  mu <- matrix$Value[1]
  return(mu)
}

#### loglik ####
loglik <- function(
    mu_janapr, mu_mayaug, mu_sepdec, 
    alphaC1, alphaC2, alphaA1, alphaA2, alphaS1, alphaS2,
    sig_dist
){
  
  
  # First, feed in standard parms
  paras <- run_parms
  # update death rate, hospitalization risks
  paras["alphaC1"] <- alpha
  paras["risk_old_h"] <- risk_old_h
  paras["risk_child_h"] <- risk_child_h
  # set seasonality
  set_mat <- seas_matrix(mu_janapr, mu_mayaug, mu_sepdec)
  ## make sure mu is defined globally so that it actually passes into the seir function
  mu <<- function(t){
    seas_function(t, matrix = set_mat)
  }
  
  # set initial conditions
  init <- run_init
  
  # Set times
  times <- 92:481
  
  # vec_print <- c(mu_janfeb, mu_marapr, mu_mayjun, 
  #                mu_julaug, mu_septoct, mu_novdec,
  #                alpha,
  #                sig_dist)
  # 
  # print("------- PARAS --------")
  # print(vec_print)
  
  # Run the model 
  out_calib1 <- ode(y=init, func = seir, times=times, parms = paras) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(time, 
           Hosp_1_child, Hosp_2_child,Hosp_3_child,
           D_1_child, D_2_child, D_2E_child, D_3_child, D_3E_child,
           Hosp_1_adult, Hosp_2_adult,Hosp_3_adult,
           D_1_adult, D_2_adult, D_2E_adult, D_3_adult, D_3E_adult,
           Hosp_1_old, Hosp_2_old,Hosp_3_old,
           D_1_old, D_2_old, D_2E_old, D_3_old, D_3E_old,
           Hosp_1_hr, Hosp_2_hr, Hosp_3_hr,
           D_1_hr, D_2_hr, D_2E_hr, D_3_hr, D_3E_hr
    ) %>%
    group_by(time) %>%
    reframe(
      `inc hosp_0-64` = Hosp_1_adult + Hosp_2_adult + Hosp_3_adult + Hosp_1_child + Hosp_2_child + Hosp_3_child + Hosp_1_hr + Hosp_2_hr + Hosp_3_hr,
      `inc nhsn_0-17` = Hosp_1_child + Hosp_2_child + Hosp_3_child,
      `inc nhsn_18-64` = Hosp_1_adult + Hosp_2_adult + Hosp_3_adult + Hosp_1_hr + Hosp_2_hr + Hosp_3_hr,
      `inc hosp_65-130` = Hosp_1_old + Hosp_2_old + Hosp_3_old,
      `inc nhsn_65-130` = Hosp_1_old + Hosp_2_old + Hosp_3_old,
      `inc hosp_0-130` = `inc hosp_0-64` + `inc hosp_65-130`,
      `inc death_0-64` = D_1_adult + D_2_adult + D_2E_adult + D_3_adult + D_3E_adult + D_1_child + D_2_child + D_2E_child +  D_3_child + D_3E_child + D_1_hr + D_2_hr + D_2E_hr + D_3_hr + D_3E_hr,
      `inc death_65-130` = D_1_old + D_2_old + D_2E_old +  D_3_old + D_3E_old,
      `inc death_0-130` = `inc death_0-64` + `inc death_65-130`
    ) %>%
    ungroup() %>%
    pivot_longer(-time) %>%
    separate(name, into = c("target", "age_group"), sep = "_") %>%
    group_by(target, age_group) %>%
    arrange(time) %>%
    mutate(value = value - lag(value, 7)) %>% ## weekly
    ungroup() %>%
    mutate(date = as.Date("01-01-2024", "%m-%d-%Y") + time) %>%
    select(-time) %>%
    right_join(data, by = c("target", "age_group", "date")) %>%
    # Normalize the data. Here, I'm using the maximum in the observed data for each target and 
    # age combination. This step is very important. Without it, the MLE weights targets and age differently,
    # and doesn't converge. 
    group_by(target, age_group) %>%
    arrange(date) %>%
    mutate(max_normal = max(observation, na.rm = T),
           observation = observation/max_normal,
           value = value/max_normal) %>%
    filter(!is.na(observation), !is.na(value)) %>%
    ungroup()
  
  # Now get the negative log likelihood. Note we are concurrently fitting the sd
  ll <- -sum(dnorm(x=out_calib1$value,mean=out_calib1$observation,sd=sig_dist,log=TRUE))
  
  # This code ensures that if the output of the log likelihood is NA, the MLE won't stop. Instead it will return
  # an extremely large, positive value of the negative log likelihood and keep iterating.
  ll_final = if_else(is.na(ll), 10^6, ll)
  
  return(ll_final)
}