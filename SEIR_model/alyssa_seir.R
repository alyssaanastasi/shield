library(deSolve)
library(tidyverse)

#### SEIR model ####

seir <- function(t, y, pars){
  # For COVID, Flu, & RSV
  # S -> E -> I -> R -> S2
  #        -> H (Hospitalized/Severe) -> R OR D
  # Parameters
  # mu <- pars$mu[1]  # probability of transition given contact

  epsilon <- pars$epsilon[1]  # how long it takes to move from e to i
  probHC1 <- pars$probHC1[1] #probability of severe infection/hospitalization with no prior immunity
  probHC2 <- pars$probHC2[1] #probability of severe infection/hospitalization with prior immunity
  probHA1 <- pars$probHA1[1] 
  probHA2 <- pars$probHA2[1] 
  probHS1 <- pars$probHS1[1] 
  probHS2 <- pars$probHS2[1] 
  gamma <- pars$gamma[1] # recovery rate
  omega <- pars$omega[1] # waning of immunity 
  b <- pars$b[1] # immunity scalar (people with previous infection/vaccination have longer waning of immunity)
  alphaC <- pars$alphaC[1] # infection-induced death rate for children
  alphaA <- pars$alphaA[1] #infection induced death rate for adults (both parents and CA)
  alphaS <- pars$alphaS[1] # infection-induced death rate for seniors
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
  lambdaC <- mu(t)*(cCC*sumIC/popC + cCCA*sumICA/popCA + cCP*sumIP/popP + cCS*sumIS/popS)
  lambdaC_terms <- c(mu(t), cCC*sumIC/popC, cCCA*sumICA/popCA, cCP*sumIP/popP, cCS*sumIS/popS)
  lambdaCA <- mu(t)*(cAA*sumI_Adults/popAdults + cCCA*sumIC/popC + cSA*sumIS/popS)
  lambdaP <- mu(t)*(cAA*sumI_Adults/popAdults + cCP*sumIC/popC + cSA*sumIS/popS)
  lambdaS <- mu(t)*(cSS*sumIS/popS + cCS*sumIC/popC + cSA*sumI_Adults/popAdults)
  
  #Aging Rates
  deltaCA <- 1/(18*365) # aging rate from child to adult
  deltaAS <- 1/(32*365) # aging rate from adult to senior
  deltaSC <- 1/(30*365) # aging rate from senior to child (birth rate?)
    
  ## Updates
  # child updates
  dS_C1 <- -(lambdaC + vaccC(t))*S_C1 - deltaCA*S_C1 + deltaSC*popS
  dE_C1 <- lambdaC*S_C1 - (epsilon + vaccC(t))*E_C1 - deltaCA*E_C1 
  dI_C1 <- (1-probHC1)*epsilon*E_C1 - gamma*I_C1 - deltaCA*I_C1 
  dH_C1 <- probHC1*epsilon*E_C1 - gamma*H_C1 - deltaCA*H_C1 
  dR_C1 <- gamma*I_C1 + (1-alphaC)*gamma*H_C1 - (omega + vaccC(t))*R_C1 - deltaCA*R_C1 
  dD_C1 <- alphaC*gamma*H_C1
  
  dS_C2 <- omega*R_C1 + b*omega*R_C2 - (ss*lambdaC + vaccC(t))*S_C2 - deltaCA*S_C2 
  dE_C2 <- ss*lambdaC*S_C2 - (epsilon + vaccC(t))*E_C2 - deltaCA*E_C2
  dI_C2 <- (1-probHC2)*epsilon*E_C2 - gamma*I_C2 - deltaCA*I_C2
  dH_C2 <- probHC2*epsilon*E_C2 - gamma*H_C2 - deltaCA*H_C2
  dR_C2 <- vaccC(t)*(S_C1 + E_C1 + R_C1 + S_C2 + E_C2) + gamma*I_C2 + (1-alphaC)*gamma*H_C2 - b*omega*R_C2 - deltaCA*R_C2
  dD_C2 <- alphaC*gamma*H_C2
  
  # childless adults
  dS_CA1 <- -(lambdaCA + vaccCA(t))*S_CA1 + deltaCA*(1 - percentAdultParent)*S_C1 - deltaAS*S_CA1
  dE_CA1 <- lambdaCA*S_CA1 - (epsilon + vaccCA(t))*E_CA1 + deltaCA*(1 - percentAdultParent)*E_C1 - deltaAS*E_CA1
  dI_CA1 <- (1-probHA1)*epsilon*E_CA1 - gamma*I_CA1 + deltaCA*(1 - percentAdultParent)*I_C1 - deltaAS*I_CA1
  dH_CA1 <- probHA1*epsilon*E_CA1 - gamma*H_CA1 + deltaCA*(1 - percentAdultParent)*H_C1 - deltaAS*H_CA1
  dR_CA1 <- gamma*I_CA1 + (1 - alphaA)*gamma*H_CA1 - (omega + vaccCA(t))*R_CA1 + deltaCA*(1 - percentAdultParent)*R_C1 - deltaAS*R_CA1
  dD_CA1 <- alphaA*gamma*H_CA1
  
  dS_CA2 <- omega*R_CA1 + b*omega*R_CA2 - (ss*lambdaCA + vaccCA(t))*S_CA2 + deltaCA*(1 - percentAdultParent)*S_C2 - deltaAS*S_CA2
  dE_CA2 <- ss*lambdaCA*S_CA2 - (epsilon + vaccCA(t))*E_CA2 + deltaCA*(1 - percentAdultParent)*E_C2 - deltaAS*E_CA2
  dI_CA2 <- (1-probHA2)*epsilon*E_CA2 - gamma*I_CA2 + deltaCA*(1 - percentAdultParent)*I_C2 - deltaAS*I_CA2
  dH_CA2 <- probHA2*epsilon*E_CA2 - gamma*H_CA2 + deltaCA*(1 - percentAdultParent)*H_C2 - deltaAS*H_CA2
  dR_CA2 <- vaccCA(t)*(S_CA1 + E_CA1 + R_CA1 + S_CA2 + E_CA2) + gamma*I_CA2 + (1 - alphaA)*gamma*H_CA2 - b*omega*R_CA2 + deltaCA*(1 - percentAdultParent)*R_C2 - deltaAS*R_CA2
  dD_CA2 <- alphaA*gamma*H_CA2
  
  # parents
  dS_P1 <- -(lambdaP + vaccP(t))*S_P1 + deltaCA*percentAdultParent*S_C1 - deltaAS*S_P1
  dE_P1 <- lambdaP*S_P1 - (epsilon + vaccP(t))*E_P1 + deltaCA*percentAdultParent*E_C1 - deltaAS*E_P1
  dI_P1 <- (1-probHA1)*epsilon*E_P1 - gamma*I_P1 + deltaCA*percentAdultParent*I_C1 - deltaAS*I_P1
  dH_P1 <- probHA1*epsilon*E_P1 - gamma*H_P1 + deltaCA*percentAdultParent*H_C1 - deltaAS*H_P1
  dR_P1 <- gamma*I_P1 + (1 - alphaA)*gamma*H_P1 - (omega + vaccP(t))*R_P1 + deltaCA*percentAdultParent*R_C1 - deltaAS*R_P1
  dD_P1 <- alphaA*gamma*H_P1
  
  dS_P2 <- omega*R_P1 + b*omega*R_P2 - (ss*lambdaP + vaccP(t))*S_P2 + deltaCA*percentAdultParent*S_C2 - deltaAS*S_P2
  dE_P2 <- ss*lambdaP*S_P2 - (epsilon + vaccP(t))*E_P2 + deltaCA*percentAdultParent*E_C2 - deltaAS*E_P2
  dI_P2 <- (1-probHA2)*epsilon*E_P2 - gamma*I_P2 + deltaCA*percentAdultParent*I_C2 - deltaAS*I_P2
  dH_P2 <- probHA2*epsilon*E_P2 - gamma*H_P2 + deltaCA*percentAdultParent*H_C2 - deltaAS*H_P2
  dR_P2 <- vaccP(t)*(S_P1 + E_P1 + R_P1 + S_P2 + E_P2) + gamma*I_P2 + (1 - alphaA)*gamma*H_P2 - b*omega*R_P2 + deltaCA*percentAdultParent*R_C2 - deltaAS*R_P2
  dD_P2 <- alphaA*gamma*H_P2
  
  # seniors
  dS_S1 <- -(lambdaS + vaccS(t))*S_S1 + deltaAS*(S_CA1 + S_P1) - deltaSC*S_S1
  dE_S1 <- lambdaS*S_S1 - (epsilon + vaccS(t))*E_S1 + deltaAS*(E_CA1 + E_P1) - deltaSC*E_S1
  dI_S1 <- (1-probHS1)*epsilon*E_S1 - gamma*I_S1 + deltaAS*(I_CA1 + I_P1) - deltaSC*I_S1
  dH_S1 <- probHS1*epsilon*E_S1 - gamma*H_S1 + deltaAS*(H_CA1 + H_P1) - deltaSC*H_S1
  dR_S1 <- gamma*I_S1 + (1 - alphaS)*gamma*H_S1 - (omega + vaccS(t))*R_S1 + deltaAS*(R_CA1 + R_P1) - deltaSC*R_S1
  dD_S1 <- alphaS*gamma*H_S1
  
  dS_S2 <- omega*R_S1 + b*omega*R_S2 - (ss*lambdaS + vaccS(t))*S_S2 + deltaAS*(S_CA2 + S_P2) - deltaSC*S_S2
  dE_S2 <- ss*lambdaS*S_S2 - (epsilon + vaccS(t))*E_S2 + deltaAS*(E_CA2 + E_P2) - deltaSC*E_S2
  dI_S2 <- (1-probHS2)*epsilon*E_S2 - gamma*I_S2 + deltaAS*(I_CA2 + I_P2) - deltaSC*I_S2
  dH_S2 <- probHS2*epsilon*E_S2 - gamma*H_S2 + deltaAS*(H_CA2 + H_P2) - deltaSC*H_S2
  dR_S2 <- vaccS(t)*(S_S1 + E_S1 + R_S1 + S_S2 + E_S2) + gamma*I_S2 + (1 - alphaS)*gamma*H_S2 - b*omega*R_S2 + deltaAS*(R_CA2 + R_P2) - deltaSC*R_S2
  dD_S2 <- alphaS*gamma*H_S2
  
  # Return list of gradients
  list(c(
    dS_C1, dE_C1, dI_C1, dH_C1, dR_C1, dD_C1,
    dS_C2, dE_C2, dI_C2, dH_C2, dR_C2, dD_C2,
    dS_CA1, dE_CA1, dI_CA1, dH_CA1, dR_CA1, dD_CA1,
    dS_CA2, dE_CA2, dI_CA2, dH_CA2, dR_CA2, dD_CA2,
    dS_P1, dE_P1, dI_P1, dH_P1, dR_P1, dD_P1,
    dS_P2, dE_P2, dI_P2, dH_P2, dR_P2, dD_P2,
    dS_S1, dE_S1, dI_S1, dH_S1, dR_S1, dD_S1,
    dS_S2, dE_S2, dI_S2, dH_S2, dR_S2, dD_S2
  ))
  
  
}
