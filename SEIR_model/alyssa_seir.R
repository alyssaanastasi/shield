library(deSolve)
library(tidyverse)

#### SEIR model ####

seir <- function(t, y, pars){
  # Parameters
  epsilon <- pars["epsilon"]  # rate of infection (how long it takes to move from e to i)
  omega <- pars["omega"] # waning of immunity 
  b <- pars["b"]
  mu <- pars["mu"]  # probability of transition given contact
  gamma <- pars["gamma"] # recovery rate for asymptomatic
  alphaC1 <- pars["alphaC1"] # infection-induced death rate
  alphaC2 <- pars["alphaC2"]
  alphaCA1 <- pars["alphaCA1"]
  alphaCA2 <- pars["alphaCA2"]
  alphaP1 <- pars["alphaP1"]
  alphaP2 <- pars["alphaP2"]
  alphaS1 <- pars["alphaS1"]
  alphaS2 <- pars["alphaS2"]
  cCC <- pars["cCC"] # contact between children 
  cCCA <- pars["cCCA"] # contact between children and childless adults
  cCP <- pars["cCP"] # contact between children and adults with children
  cCS <- pars["cCS"] # contact between seniors and children
  cAA <- pars["cAA"] # contact between adults
  cSA <- pars["cSA"] # contact between seniors and adults
  cSS <- pars["cSS"] # contact between seniors
  ss <- pars["ss"] # scale susceptibility -> people who have already had covid are less susceptible
  si <- pars["si"] # scale infectiousness -> people with immunity are less infectious
  
  # State variables:
  # 4 groups: Children, Adults with Kids, Adults without Kids, Seniors 
  # Children 
  S_C1 <- y[1]  # no immunity
  E_C1 <- y[2]
  I_C1 <- y[3]
  R_C1 <- y[4]
  D_C1 <- y[5]
  S_C2 <- y[6]  # some immunity (prior infection, vaccination, etc)
  E_C2 <- y[7]
  I_C2 <- y[8]
  R_C2 <- y[9]
  D_C2 <- y[10]
  
  # Childless Adults
  S_CA1 <- y[11] # no immunity
  E_CA1 <- y[12]
  I_CA1 <- y[13]
  R_CA1 <- y[14]
  D_CA1 <- y[15]
  S_CA2 <- y[16] # some immunity (prior infection, vaccination, etc)
  E_CA2 <- y[17]
  I_CA2 <- y[18]
  R_CA2 <- y[19]
  D_CA2 <- y[20]
  
  # Adults with Children (Parents)
  S_P1 <- y[21] # no immunity
  E_P1 <- y[22]
  I_P1 <- y[23]
  R_P1 <- y[24]
  D_P1 <- y[25] 
  S_P2 <- y[26] # some immunity (prior infection, vaccination, etc)
  E_P2 <- y[27]
  I_P2 <- y[28]
  R_P2 <- y[29]
  D_P2 <- y[30]
  
  # Seniors 
  S_S1 <- y[31] # no immunity
  E_S1 <- y[32]
  I_S1 <- y[33]
  R_S1 <- y[34]
  D_S1 <- y[35]
  S_S2 <- y[36] # some immunity (prior infection, vaccination, etc)
  E_S2 <- y[37]
  I_S2 <- y[38]
  R_S2 <- y[39]
  D_S2 <- y[40]
  
  
  # Group totals
  sumIC <- I_C1 + si*I_C2
  popC <- S_C1 + E_C1 + I_C1 + R_C1 + S_C2 + E_C2 + I_C2 + R_C2
  sumICA <- I_CA1 + si*I_CA2
  popCA <- S_CA1 + E_CA1+ I_CA1 + R_CA1 + S_CA2 + E_CA2 + I_CA2 + R_CA2
  sumIP <- I_P1 + si*I_P2
  popP <- S_P1 + E_P1+ I_P1 + R_P1 + S_P2 + E_P2+ I_P2 + R_P2
  sumIS <- I_S1 + si*I_S2
  popS <- S_S1 + E_S1+ I_S1 + R_S1 + S_S2 + E_S2+ I_S2 + R_S2
  sumI_Adults <- sumICA + sumIP
  popAdults <- popCA + popP
  
  # force of infection: 
  # Lambda = Child to Adult contact * mu * prop of adults infected
  lambdaC <- mu*(cCC*sumIC/popC + cCCA*sumICA/popCA + cCP*sumIP/popP + cCS*sumIS/popS)
  lambdaCA <- mu*(cAA*sumI_Adults/popAdults + cCCA*sumIC/popC + cSA*sumS/popS)
  lambdaP <- mu*(cAA*sumI_Adults/popAdults + cCP*sumIC/popC + cSA*sumS/popS)
  lambdaS <- mu*(cSS*sumIS/popS + cCS*sumIC/popC + cSA*sumI_Adults/popAdults)
  
  ## Updates
  #child updates
  dS_C1 <- -(lambdaC + vaccC(t))*S_C1
  dE_C1 <- lambdaC*S_C1 - (epsilon + vaccC(t))*E_C1
  dI_C1 <- epsilon*E_C1 - gamma*I_C1
  dR_C1 <- gamma*I_C1 - (omega + vaccC(t))*R_C1
  dD_C1 <- alphaC1*gamma*I_C1
  dS_C2 <- omega*R_C1 + b*omega*R_C2 - (ss*lambdaC + vaccC(t))*S_C2
  dE_C2 <- ss*lambdaC*S_C2 - (epsilon + vaccC(t))*E_C2
  dI_C2 <- epsilon*E_C2 - gamma*I_C2
  dR_C2 <- vaccC(t)*(S_C1 + E_C1 + R_C1 + S_C2 + E_C2) +
    gamma*I_C2 - b*omega*R_C2
  dD_C2 <- alphaC2*gamma*I_C2
  
  
  # childless adults updates
  dS_CA1 <- -(lambdaCA + vaccCA(t)) * S_CA1
  dE_CA1 <- lambdaCA* S_CA1 - (epsilon + vaccCA(t)) * E_CA1
  dI_CA1 <- epsilon * E_CA1 - gamma * I_CA1
  dR_CA1 <- gamma * I_CA1 - (omega + vaccCA(t)) * R_CA1
  dD_CA1 <- alphaCA1 * gamma * I_CA1
  dS_CA2 <- omega * R_CA1 + b * omega * R_CA2 - (ss * lambdaCA + vaccCA(t)) * S_CA2
  dE_CA2 <- ss * lambdaCA * S_CA2 - (epsilon + vaccCA(t)) * E_CA2
  dI_CA2 <- epsilon * E_CA2 - gamma * I_CA2
  dR_CA2 <- vaccCA(t) * (S_CA1 + E_CA1 + R_CA1 + S_CA2 + E_CA2) +
    gamma * I_CA2 - b * omega * R_CA2
  dD_CA2 <- alphaCA2 * gamma * I_CA2
  
  # senior updates
  dS_S1 <- -(lambdaS + vaccS(t)) * S_S1
  dE_S1 <- lambdaS * S_S1 - (epsilon + vaccS(t)) * E_S1
  dI_S1 <- epsilon * E_S1 - gamma * I_S1
  dR_S1 <- gamma * I_S1 - (omega + vaccS(t)) * R_S1
  dD_S1 <- alphaS1 * gamma * I_S1
  dS_S2 <- omega * R_S1 + b * omega * R_S2 - (ss * lambdaS + vaccS(t)) * S_S2
  dE_S2 <- ss * lambdaS * S_S2 - (epsilon + vaccS(t)) * E_S2
  dI_S2 <- epsilon * E_S2 - gamma * I_S2
  dR_S2 <- vaccS(t) * (S_S1 + E_S1 + R_S1 + S_S2 + E_S2) +
    gamma * I_S2 - b * omega * R_S2
  dD_S2 <- alphaS2 * gamma * I_S2
  
  # Return list of gradients
  list(c(dS_C1, dE_C1, dI_C1, dR_C1, dD_C1,
         dS_C2, dE_C2, dI_C2, dR_C2, dD_C2,
         dS_CA1, dE_CA1, dI_CA1, dR_CA1, dD_CA1,
         dS_CA2, dE_CA2, dI_CA2, dR_CA2, dD_CA2,
         dS_S1, dE_S1, dI_S1, dR_S1, dD_S1,
         dS_S2, dE_S2, dI_S2, dR_S2, dD_S2))
}

#### Type III functional response ####

type_III_resp <- function (t, Vmax, WVH, k) {
  
  return(Vmax*(t^k)/((WVH^k) + (t^k)))
}

#### Vaccination ####

vacc_weekly <- function (Vmax, WVH, k){
  datOut <- tibble(Week = 1:60, Vac = type_III_resp(1:60, Vmax,WVH, k)) %>%
    mutate(dailyVac =  c(Vac[1],diff(Vac)),
           VacCumSum = cumsum(dailyVac)) %>% 
    arrange(Week) %>%
    mutate(Day = Week*7-7+1, dailyVac=dailyVac/7)
  
  vaccF <- splinefun(x = datOut$Day, y = datOut$dailyVac)
  return(vaccF)
}
