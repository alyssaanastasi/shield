library(deSolve)
library(tidyverse)

#### Tripledemic SEIR model ####

seir <- function(t, y, pars){
  # For COVID, Flu, & RSV
  # S -> E -> I -> H -> S or D
  #             -> D
  
  # We have 4 distinct age groups abbreviated as follows:
    # C - Children 0-5
    # OC - Older Children 6-18
    # A - Adults 18-65
    # S - Seniors 65+
  # Parameters
  ## General
  # cCC <- pars['cCC'] # contact between children
  # cCOC <- pars['cCOC'] # contact between children and older children
  # cCA <- pars['cCA'] # contact between children and adults
  # cCS <- pars['cCS'] # contact between children and seniors
  # cOCOC <- pars['cOCOC'] #contact between older children
  # cOCA <- pars['cOCA'] #contact between older children and adults
  # cOCS <- pars['cOCS'] #contact between older children and seniors
  # cAA <- pars['cAA'] # contact between adults
  # cAS <- pars['cAS']# contact between seniors and adults
  # cSS <- pars['cSS'] # contact between seniors
  # H_capacity <- pars['H_capacity'] # hospital constraint
  
  ## RSV
  beta_RSV <- pars["beta_RSV"]
  epsilon_RSV <- pars["epsilon_RSV"] # how long it takes to move from e to i
  gammaI_RSV <- pars["gammaI_RSV"] # recovery rate for infected compartment
  gammaH_RSV <- pars["gammaH_RSV"] # recovery rate for hospitalized compartment
  omega_RSV <- pars["omega_RSV"] # waning of immunity 
  ss_RSV <- pars['ss_RSV'] # scale susceptibility -> people who have already had covid are less susceptible
  si_RSV <- pars['si_RSV'] # scale infectiousness -> people with immunity are less infectious
  vaccC_RSV <- pars['vaccC_RSV'] #child vaccination
  vaccOC_RSV <- pars['vaccOC_RSV'] #older child adult vaccination
  vaccA_RSV <- pars['vaccA_RSV'] #adult vaccination
  vaccS_RSV <- pars['vaccS_RSV'] #senior vaccination
  ve_RSV <- pars['ve_RSV'] #vaccine efficacy
  
  ### C parms
  probHC_RSV <- pars["probHC_RSV"] #probability of hospitalization for RSV without vax
  probHC_RSV_vax <- pars["probHC_RSV_vax"] #probability of hospitalization for RSV with vax
  alphaC_RSV <- pars["alphaC_RSV"]
  
  ### OC parms
  probHOC_RSV <- pars["probHOC_RSV"] #probability of hospitalization for RSV without vax
  probHOC_RSV_vax <- pars["probHOC_RSV_vax"] #probability of hospitalization for RSV with vax
  alphaOC_RSV <- pars["alphaOC_RSV"]
  
  ### A parms
  probHA_RSV <- pars["probHA_RSV"] #probability of hospitalization for RSV without vax
  probHA_RSV_vax <- pars["probHA_RSV_vax"] #probability of hospitalization for RSV with vax
  alphaA_RSV <- pars["alphaA_RSV"]
  
  ### S parms
  probHS_RSV <- pars["probHS_RSV"] #probability of hospitalization for RSV without vax
  probHS_RSV_vax <- pars["probHS_RSV_vax"] #probability of hospitalization for RSV with vax
  alphaS_RSV <- pars["alphaS_RSV"]
  
  ## COVID
  beta_COV <- pars["beta_COV"]
  epsilon_COV <- pars["epsilon_COV"] # how long it takes to move from e to i
  gammaI_COV <- pars["gammaI_COV"] # recovery rate for infected compartment
  gammaH_COV <- pars["gammaH_COV"] # recovery rate for hospitalized compartment
  omega_COV <- pars["omega_COV"] # waning of immunity 
  ss_COV <- pars['ss_COV'] # scale susceptibility -> people who have already had covid are less susceptible
  si_COV <- pars['si_COV'] # scale infectiousness -> people with immunity are less infectious
  vaccC_COV <- pars['vaccC_COV'] #child vaccination
  vaccOC_COV <- pars['vaccOC_COV'] #older child adult vaccination
  vaccA_COV <- pars['vaccA_COV'] #adult vaccination
  vaccS_COV <- pars['vaccS_COV'] #senior vaccination
  ve_COV <- pars['ve_COV'] #vaccine efficacy
  
  ### C parms
  probHC_COV <- pars["probHC_COV"] #probability of hospitalization for COV without vax
  probHC_COV_vax <- pars["probHC_COV_vax"] #probability of hospitalization for COV with vax
  alphaC_COV <- pars["alphaC_COV"]
  
  ### OC parms
  probHOC_COV <- pars["probHOC_COV"] #probability of hospitalization for COV without vax
  probHOC_COV_vax <- pars["probHOC_COV_vax"] #probability of hospitalization for COV with vax
  alphaOC_COV <- pars["alphaOC_COV"]
  
  ### A parms
  probHA_COV <- pars["probHA_COV"] #probability of hospitalization for COV without vax
  probHA_COV_vax <- pars["probHA_COV_vax"] #probability of hospitalization for COV with vax
  alphaA_COV <- pars["alphaA_COV"]
  
  ### S parms
  probHS_COV <- pars["probHS_COV"] #probability of hospitalization for COV without vax
  probHS_COV_vax <- pars["probHS_COV_vax"] #probability of hospitalization for COV with vax
  alphaS_COV <- pars["alphaS_COV"]
  
  ## FLU
  beta_FLU <- pars["beta_FLU"]
  epsilon_FLU <- pars["epsilon_FLU"] # how long it takes to move from e to i
  gammaI_FLU <- pars["gammaI_FLU"] # recovery rate for infected compartment
  gammaH_FLU <- pars["gammaH_FLU"] # recovery rate for hospitalized compartment
  omega_FLU <- pars["omega_FLU"] # waning of immunity 
  ss_FLU <- pars['ss_FLU'] # scale susceptibility -> people who have already had FLU are less susceptible
  si_FLU <- pars['si_FLU'] # scale infectiousness -> people with immunity are less infectious
  vaccC_FLU <- pars['vaccC_FLU'] #child vaccination
  vaccOC_FLU <- pars['vaccOC_FLU'] #older child adult vaccination
  vaccA_FLU <- pars['vaccA_FLU'] #adult vaccination
  vaccS_FLU <- pars['vaccS_FLU'] #senior vaccination
  ve_FLU <- pars['ve_FLU'] #vaccine efficacy
  
  ### C parms
  probHC_FLU <- pars["probHC_FLU"] #probability of hospitalization for FLU without vax
  probHC_FLU_vax <- pars["probHC_FLU_vax"] #probability of hospitalization for FLU with vax
  alphaC_FLU <- pars["alphaC_FLU"]
  
  ### OC parms
  probHOC_FLU <- pars["probHOC_FLU"] #probability of hospitalization for FLU without vax
  probHOC_FLU_vax <- pars["probHOC_FLU_vax"] #probability of hospitalization for FLU with vax
  alphaOC_FLU <- pars["alphaOC_FLU"]
  
  ### A parms
  probHA_FLU <- pars["probHA_FLU"] #probability of hospitalization for FLU without vax
  probHA_FLU_vax <- pars["probHA_FLU_vax"] #probability of hospitalization for FLU with vax
  alphaA_FLU <- pars["alphaA_FLU"]
  
  ### S parms
  probHS_FLU <- pars["probHS_FLU"] #probability of hospitalization for FLU without vax
  probHS_FLU_vax <- pars["probHS_FLU_vax"] #probability of hospitalization for FLU with vax
  alphaS_FLU <- pars["alphaS_FLU"]
  
  # State variables:
  # 4 groups: Children, Older Children, Adults, Seniors
  # 3 diseases: RSV, COVID, Flu
  # For each disease: Vax & Unvax
  
  ## Stratified single S 
  S_C <- y["S_C"]
  S_OC <- y["S_OC"]
  S_A <- y["S_A"]
  S_S <- y["S_S"]
  
  ## RSV
  
  ### Children 
  E_C_RSV_vax <- y["E_C_RSV_vax"]
  I_C_RSV_vax <- y["I_C_RSV_vax"]
  H_C_RSV_vax <- y["H_C_RSV_vax"]
  D_C_RSV_vax <- y["D_C_RSV_vax"]
  
  E_C_RSV <- y["E_C_RSV"]
  I_C_RSV <- y["I_C_RSV"]
  H_C_RSV <- y["H_C_RSV"]
  D_C_RSV <- y["D_C_RSV"]
  
  RSV_C = c(E_C_RSV_vax, I_C_RSV_vax, H_C_RSV_vax,
           E_C_RSV, I_C_RSV, H_C_RSV)
  
  ### Older Children
  E_OC_RSV_vax <- y["E_OC_RSV_vax"]
  I_OC_RSV_vax <- y["I_OC_RSV_vax"]
  H_OC_RSV_vax <- y["H_OC_RSV_vax"]
  D_OC_RSV_vax <- y["D_OC_RSV_vax"]
  
  E_OC_RSV <- y["E_OC_RSV"]
  I_OC_RSV <- y["I_OC_RSV"]
  H_OC_RSV <- y["H_OC_RSV"]
  D_OC_RSV <- y["D_OC_RSV"]
  
  RSV_OC = c(E_OC_RSV_vax, I_OC_RSV_vax, H_OC_RSV_vax,
              E_OC_RSV, I_OC_RSV, H_OC_RSV)
  
  
  ### Adults
  E_A_RSV_vax <- y["E_A_RSV_vax"]
  I_A_RSV_vax <- y["I_A_RSV_vax"]
  H_A_RSV_vax <- y["H_A_RSV_vax"]
  D_A_RSV_vax <- y["D_A_RSV_vax"]
  
  E_A_RSV <- y["E_A_RSV"]
  I_A_RSV <- y["I_A_RSV"]
  H_A_RSV <- y["H_A_RSV"]
  D_A_RSV <- y["D_A_RSV"]
  
  RSV_A = c(E_A_RSV_vax, I_A_RSV_vax, H_A_RSV_vax,
            E_A_RSV, I_A_RSV, H_A_RSV)
  
  ### Seniors 
  E_S_RSV_vax <- y["E_S_RSV_vax"]
  I_S_RSV_vax <- y["I_S_RSV_vax"]
  H_S_RSV_vax <- y["H_S_RSV_vax"]
  D_S_RSV_vax <- y["D_S_RSV_vax"]
  
  E_S_RSV <- y["E_S_RSV"]
  I_S_RSV <- y["I_S_RSV"]
  H_S_RSV <- y["H_S_RSV"]
  D_S_RSV <- y["D_S_RSV"]
  
  RSV_S = c(E_S_RSV_vax, I_S_RSV_vax, H_S_RSV_vax,
            E_S_RSV, I_S_RSV, H_S_RSV)
  
  ## COVID
  
  ### Children 
  E_C_COV_vax <- y["E_C_COV_vax"]
  I_C_COV_vax <- y["I_C_COV_vax"]
  H_C_COV_vax <- y["H_C_COV_vax"]
  D_C_COV_vax <- y["D_C_COV_vax"]
  
  E_C_COV <- y["E_C_COV"]
  I_C_COV <- y["I_C_COV"]
  H_C_COV <- y["H_C_COV"]
  D_C_COV <- y["D_C_COV"]
  
  COV_C = c(E_C_COV_vax, I_C_COV_vax, H_C_COV_vax,
            E_C_COV, I_C_COV, H_C_COV)
  
  ### Older Children
  E_OC_COV_vax <- y["E_OC_COV_vax"]
  I_OC_COV_vax <- y["I_OC_COV_vax"]
  H_OC_COV_vax <- y["H_OC_COV_vax"]
  D_OC_COV_vax <- y["D_OC_COV_vax"]
  
  E_OC_COV <- y["E_OC_COV"]
  I_OC_COV <- y["I_OC_COV"]
  H_OC_COV <- y["H_OC_COV"]
  D_OC_COV <- y["D_OC_COV"]
  
  COV_OC = c(E_OC_COV_vax, I_OC_COV_vax, H_OC_COV_vax,
             E_OC_COV, I_OC_COV, H_OC_COV)
  
  
  ### Adults
  E_A_COV_vax <- y["E_A_COV_vax"]
  I_A_COV_vax <- y["I_A_COV_vax"]
  H_A_COV_vax <- y["H_A_COV_vax"]
  D_A_COV_vax <- y["D_A_COV_vax"]
  
  E_A_COV <- y["E_A_COV"]
  I_A_COV <- y["I_A_COV"]
  H_A_COV <- y["H_A_COV"]
  D_A_COV <- y["D_A_COV"]
  
  COV_A = c(E_A_COV_vax, I_A_COV_vax, H_A_COV_vax,
            E_A_COV, I_A_COV, H_A_COV)
  
  ### Seniors 
  E_S_COV_vax <- y["E_S_COV_vax"]
  I_S_COV_vax <- y["I_S_COV_vax"]
  H_S_COV_vax <- y["H_S_COV_vax"]
  D_S_COV_vax <- y["D_S_COV_vax"]
  
  E_S_COV <- y["E_S_COV"]
  I_S_COV <- y["I_S_COV"]
  H_S_COV <- y["H_S_COV"]
  D_S_COV <- y["D_S_COV"]
  
  COV_S = c(E_S_COV_vax, I_S_COV_vax, H_S_COV_vax,
            E_S_COV, I_S_COV, H_S_COV)
  
  ## FLU
  
  ### Children 
  E_C_FLU_vax <- y["E_C_FLU_vax"]
  I_C_FLU_vax <- y["I_C_FLU_vax"]
  H_C_FLU_vax <- y["H_C_FLU_vax"]
  D_C_FLU_vax <- y["D_C_FLU_vax"]
  
  E_C_FLU <- y["E_C_FLU"]
  I_C_FLU <- y["I_C_FLU"]
  H_C_FLU <- y["H_C_FLU"]
  D_C_FLU <- y["D_C_FLU"]
  
  FLU_C = c(E_C_FLU_vax, I_C_FLU_vax, H_C_FLU_vax,
            E_C_FLU, I_C_FLU, H_C_FLU)
  
  ### Older Children
  E_OC_FLU_vax <- y["E_OC_FLU_vax"]
  I_OC_FLU_vax <- y["I_OC_FLU_vax"]
  H_OC_FLU_vax <- y["H_OC_FLU_vax"]
  D_OC_FLU_vax <- y["D_OC_FLU_vax"]
  
  E_OC_FLU <- y["E_OC_FLU"]
  I_OC_FLU <- y["I_OC_FLU"]
  H_OC_FLU <- y["H_OC_FLU"]
  D_OC_FLU <- y["D_OC_FLU"]
  
  FLU_OC = c(E_OC_FLU_vax, I_OC_FLU_vax, H_OC_FLU_vax,
             E_OC_FLU, I_OC_FLU, H_OC_FLU)
  
  
  ### Adults
  E_A_FLU_vax <- y["E_A_FLU_vax"]
  I_A_FLU_vax <- y["I_A_FLU_vax"]
  H_A_FLU_vax <- y["H_A_FLU_vax"]
  D_A_FLU_vax <- y["D_A_FLU_vax"]
  
  E_A_FLU <- y["E_A_FLU"]
  I_A_FLU <- y["I_A_FLU"]
  H_A_FLU <- y["H_A_FLU"]
  D_A_FLU <- y["D_A_FLU"]
  
  FLU_A = c(E_A_FLU_vax, I_A_FLU_vax, H_A_FLU_vax,
            E_A_FLU, I_A_FLU, H_A_FLU)
  
  ### Seniors 
  E_S_FLU_vax <- y["E_S_FLU_vax"]
  I_S_FLU_vax <- y["I_S_FLU_vax"]
  H_S_FLU_vax <- y["H_S_FLU_vax"]
  D_S_FLU_vax <- y["D_S_FLU_vax"]
  
  E_S_FLU <- y["E_S_FLU"]
  I_S_FLU <- y["I_S_FLU"]
  H_S_FLU <- y["H_S_FLU"]
  D_S_FLU <- y["D_S_FLU"]
  
  FLU_S = c(E_S_FLU_vax, I_S_FLU_vax, H_S_FLU_vax,
            E_S_FLU, I_S_FLU, H_S_FLU)
  
  # Group totals
  # sum_RSV = sum(RSV_C) + sum(RSV_OC) + sum(RSV_A) + sum(RSV_S)
  # sum_COV
  # sum_FLU
  
  # print("time:")
  # print(t)
  sumIC_RSV <- I_C_RSV + si_RSV*I_C_RSV_vax + H_C_RSV + si_RSV*H_C_RSV_vax
  sumIC_COV <- I_C_COV + si_COV*I_C_COV_vax + H_C_COV + si_COV*H_C_COV_vax
  sumIC_FLU <- I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax
  popC <- S_C + sum(RSV_C) + sum(COV_C) + sum(FLU_C)
  sumIOC_RSV <- I_OC_RSV + si_RSV*I_OC_RSV_vax + H_OC_RSV + si_RSV*H_OC_RSV_vax
  sumIOC_COV <- I_OC_COV + si_COV*I_OC_COV_vax + H_OC_COV + si_COV*H_OC_COV_vax
  sumIOC_FLU <- I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax
  popOC <- S_OC + sum(RSV_OC) + sum(COV_OC) + sum(FLU_OC)
  sumIA_RSV <- I_A_RSV + si_RSV*I_A_RSV_vax + H_A_RSV + si_RSV*H_A_RSV_vax
  sumIA_COV <- I_A_COV + si_COV*I_A_COV_vax + H_A_COV + si_COV*H_A_COV_vax
  sumIA_FLU <- I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax
  popA <- S_A + sum(RSV_A) + sum(COV_A) + sum(FLU_A)
  sumIS_RSV <- I_S_RSV + si_RSV*I_S_RSV_vax + H_S_RSV + si_RSV*H_S_RSV_vax
  sumIS_COV <- I_S_COV + si_COV*I_S_COV_vax + H_S_COV + si_COV*H_S_COV_vax
  sumIS_FLU <- I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax
  popS <- S_S + sum(RSV_S) + sum(COV_S) + sum(FLU_S)

  # force of infection: 
  # Lambda = x to y population contact * mu * prop of y infected
  lambdaC_RSV <- beta_RSV*(cCC*sumIC_RSV/popC + cCOC*sumIOC_RSV/popOC + cCA*sumIA_RSV/popA + cCS*sumIS_RSV/popS)
  lambdaOC_RSV <- beta_RSV*(cOCC*sumIC_RSV/popC + cOCOC*sumIOC_RSV/popOC + cOCA*sumIA_RSV/popA + cOCS*sumIS_RSV/popS)
  # print(lambdaOC_RSV)
  lambdaA_RSV <- beta_RSV*(cAC*sumIC_RSV/popC + cAOC*sumIOC_RSV/popOC + cAA*sumIA_RSV/popA + cAS*sumIS_RSV/popS)
  lambdaS_RSV <- beta_RSV*(cSC*sumIC_RSV/popC + cSOC*sumIOC_RSV/popOC + cSA*sumIA_RSV/popA + cSS*sumIS_RSV/popS)
  
  lambdaC_COV <- beta_COV*(cCC*sumIC_COV/popC + cCOC*sumIOC_COV/popOC + cCA*sumIA_COV/popA + cCS*sumIS_COV/popS)
  lambdaOC_COV <- beta_COV*(cOCC*sumIC_COV/popC + cOCOC*sumIOC_COV/popOC + cOCA*sumIA_COV/popA + cOCS*sumIS_COV/popS)
  lambdaA_COV <- beta_COV*(cAC*sumIC_COV/popC + cAOC*sumIOC_COV/popOC + cAA*sumIA_COV/popA + cAS*sumIS_COV/popS)
  lambdaS_COV <- beta_COV*(cSC*sumIC_COV/popC + cSOC*sumIOC_COV/popOC + cSA*sumIA_COV/popA + cSS*sumIS_COV/popS)
  
  lambdaC_FLU <- beta_FLU*(cCC*sumIC_FLU/popC + cCOC*sumIOC_FLU/popOC + cCA*sumIA_FLU/popA + cCS*sumIS_FLU/popS)
  lambdaOC_FLU <- beta_FLU*(cOCC*sumIC_FLU/popC + cOCOC*sumIOC_FLU/popOC + cOCA*sumIA_FLU/popA + cOCS*sumIS_FLU/popS)
  lambdaA_FLU <- beta_FLU*(cAC*sumIC_FLU/popC + cAOC*sumIOC_FLU/popOC + cAA*sumIA_FLU/popA + cAS*sumIS_FLU/popS)
  lambdaS_FLU <- beta_FLU*(cSC*sumIC_FLU/popC + cSOC*sumIOC_FLU/popOC + cSA*sumIA_FLU/popA + cSS*sumIS_FLU/popS)
  
  # Updates
  dS_C <- 
    -(lambdaC_RSV)*S_C + (1-probHC_RSV_vax)*gammaI_RSV*I_C_RSV_vax + (1-alphaC_RSV)*gammaH_RSV*H_C_RSV_vax +
    (1-probHC_RSV)*gammaI_RSV*I_C_RSV + (1-alphaC_RSV)*gammaH_RSV*H_C_RSV -
    (lambdaC_COV)*S_C + (1-probHC_COV_vax)*gammaI_COV*I_C_COV_vax + (1-alphaC_COV)*gammaH_COV*H_C_COV_vax +
    (1-probHC_COV)*gammaI_COV*I_C_COV + (1-alphaC_COV)*gammaH_COV*H_C_COV -
    (lambdaC_FLU)*S_C + (1-probHC_FLU_vax)*gammaI_FLU*I_C_FLU_vax + (1-alphaC_FLU)*gammaH_FLU*H_C_FLU_vax +
    (1-probHC_FLU)*gammaI_FLU*I_C_FLU + (1-alphaC_FLU)*gammaH_FLU*H_C_FLU
  
  
  dS_OC <- 
    -(lambdaOC_RSV)*S_OC + (1-probHOC_RSV_vax)*gammaI_RSV*I_OC_RSV_vax + (1-alphaOC_RSV)*gammaH_RSV*H_OC_RSV_vax +
    (1-probHOC_RSV)*gammaI_RSV*I_OC_RSV + (1-alphaOC_RSV)*gammaH_RSV*H_OC_RSV -
    (lambdaOC_COV)*S_OC + (1-probHOC_COV_vax)*gammaI_COV*I_OC_COV_vax + (1-alphaOC_COV)*gammaH_COV*H_OC_COV_vax +
    (1-probHOC_COV)*gammaI_COV*I_OC_COV + (1-alphaOC_COV)*gammaH_COV*H_OC_COV -
    (lambdaOC_FLU)*S_OC + (1-probHOC_FLU_vax)*gammaI_FLU*I_OC_FLU_vax + (1-alphaOC_FLU)*gammaH_FLU*H_OC_FLU_vax + 
    (1-probHOC_FLU)*gammaI_FLU*I_OC_FLU + (1-alphaOC_FLU)*gammaH_FLU*H_OC_FLU
  
  
  dS_A <- 
    -(lambdaA_RSV)*S_A + (1-probHA_RSV_vax)*gammaI_RSV*I_A_RSV_vax + (1-alphaA_RSV)*gammaH_RSV*H_A_RSV_vax +
    (1-probHA_RSV)*gammaI_RSV*I_A_RSV + (1-alphaA_RSV)*gammaH_RSV*H_A_RSV - 
    (lambdaA_COV)*S_A + (1-probHA_COV_vax)*gammaI_COV*I_A_COV_vax + (1-alphaA_COV)*gammaH_COV*H_A_COV_vax +
    (1-probHA_COV)*gammaI_COV*I_A_COV + (1-alphaA_COV)*gammaH_COV*H_A_COV -
    (lambdaA_FLU)*S_A + (1-probHA_FLU_vax)*gammaI_FLU*I_A_FLU_vax + (1-alphaA_FLU)*gammaH_FLU*H_A_FLU_vax +
    (1-probHA_FLU)*gammaI_FLU*I_A_FLU + (1-alphaA_FLU)*gammaH_FLU*H_A_FLU
  
  
  dS_S <- 
    -(lambdaS_RSV)*S_S + (1-probHS_RSV_vax)*gammaI_RSV*I_S_RSV_vax + (1-alphaS_RSV)*gammaH_RSV*H_S_RSV_vax +
    (1-probHS_RSV)*gammaI_RSV*I_S_RSV + (1-alphaS_RSV)*gammaH_RSV*H_S_RSV -
    (lambdaS_COV)*S_S + (1-probHS_COV_vax)*gammaI_COV*I_S_COV_vax + (1-alphaS_COV)*gammaH_COV*H_S_COV_vax +
    (1-probHS_COV)*gammaI_COV*I_S_COV + (1-alphaS_COV)*gammaH_COV*H_S_COV -
    (lambdaS_FLU)*S_S + (1-probHS_FLU_vax)*gammaI_FLU*I_S_FLU_vax + (1-alphaS_FLU)*gammaH_FLU*H_S_FLU_vax +
    (1-probHS_FLU)*gammaI_FLU*I_S_FLU + (1-alphaS_FLU)*gammaH_FLU*H_S_FLU
  
  
  ## RSV 
  ### Children Updates
  dE_C_RSV_vax <- (lambdaC_RSV * vaccC_RSV * ve_RSV)*S_C - epsilon_RSV*E_C_RSV_vax
  dI_C_RSV_vax <- epsilon_RSV*E_C_RSV_vax - gammaI_RSV*I_C_RSV_vax
  dH_C_RSV_vax <- probHC_RSV_vax*gammaI_RSV*I_C_RSV_vax - gammaH_RSV*H_C_RSV_vax
  dD_C_RSV_vax <- alphaC_RSV*gammaH_RSV*H_C_RSV_vax
  
  dE_C_RSV <- (lambdaC_RSV * (1-vaccC_RSV))*S_C - epsilon_RSV*E_C_RSV
  dI_C_RSV <- epsilon_RSV*E_C_RSV - gammaI_RSV*I_C_RSV
  dH_C_RSV <- probHC_RSV*gammaI_RSV*I_C_RSV - gammaH_RSV*H_C_RSV
  dD_C_RSV <- alphaC_RSV*gammaH_RSV*H_C_RSV
  
  ### Older Children
  dE_OC_RSV_vax <- (lambdaOC_RSV * vaccOC_RSV * ve_RSV)*S_OC - epsilon_RSV*E_OC_RSV_vax
  dI_OC_RSV_vax <- epsilon_RSV*E_OC_RSV_vax - gammaI_RSV*I_OC_RSV_vax
  dH_OC_RSV_vax <- probHOC_RSV_vax*gammaI_RSV*I_OC_RSV_vax - gammaH_RSV*H_OC_RSV_vax
  dD_OC_RSV_vax <- alphaOC_RSV*gammaH_RSV*H_OC_RSV_vax
  
  dE_OC_RSV <- (lambdaOC_RSV * (1-vaccOC_RSV))*S_OC - epsilon_RSV*E_OC_RSV
  dI_OC_RSV <- epsilon_RSV*E_OC_RSV - gammaI_RSV*I_OC_RSV
  dH_OC_RSV <- probHOC_RSV*gammaI_RSV*I_OC_RSV - gammaH_RSV*H_OC_RSV
  dD_OC_RSV <- alphaOC_RSV*gammaH_RSV*H_OC_RSV
  
  ### Adults
  dE_A_RSV_vax <- (lambdaA_RSV * vaccA_RSV * ve_RSV)*S_A - epsilon_RSV*E_A_RSV_vax
  dI_A_RSV_vax <- epsilon_RSV*E_A_RSV_vax - gammaI_RSV*I_A_RSV_vax
  dH_A_RSV_vax <- probHA_RSV_vax*gammaI_RSV*I_A_RSV_vax - gammaH_RSV*H_A_RSV_vax
  dD_A_RSV_vax <- alphaA_RSV*gammaH_RSV*H_A_RSV_vax
  
  dE_A_RSV <- (lambdaA_RSV * (1-vaccA_RSV))*S_A - epsilon_RSV*E_A_RSV
  dI_A_RSV <- epsilon_RSV*E_A_RSV - gammaI_RSV*I_A_RSV
  dH_A_RSV <- probHA_RSV*gammaI_RSV*I_A_RSV - gammaH_RSV*H_A_RSV
  dD_A_RSV <- alphaA_RSV*gammaH_RSV*H_A_RSV
  
  ### Seniors
  dE_S_RSV_vax <- (lambdaS_RSV * vaccS_RSV * ve_RSV)*S_S - epsilon_RSV*E_S_RSV_vax
  dI_S_RSV_vax <- epsilon_RSV*E_S_RSV_vax - gammaI_RSV*I_S_RSV_vax
  dH_S_RSV_vax <- probHS_RSV_vax*gammaI_RSV*I_S_RSV_vax - gammaH_RSV*H_S_RSV_vax
  dD_S_RSV_vax <- alphaS_RSV*gammaH_RSV*H_S_RSV_vax
  
  dE_S_RSV <- (lambdaS_RSV * (1-vaccS_RSV))*S_S - epsilon_RSV*E_S_RSV
  dI_S_RSV <- epsilon_RSV*E_S_RSV - gammaI_RSV*I_S_RSV
  dH_S_RSV <- probHS_RSV*gammaI_RSV*I_S_RSV - gammaH_RSV*H_S_RSV
  dD_S_RSV <- alphaS_RSV*gammaH_RSV*H_S_RSV
  
  ## COVID
  ### Children Updates
  dE_C_COV_vax <- (lambdaC_COV * vaccC_COV * ve_COV)*S_C - epsilon_COV*E_C_COV_vax
  dI_C_COV_vax <- epsilon_COV*E_C_COV_vax - gammaI_COV*I_C_COV_vax
  dH_C_COV_vax <- probHC_COV_vax*gammaI_COV*I_C_COV_vax - gammaH_COV*H_C_COV_vax
  dD_C_COV_vax <- alphaC_COV*gammaH_COV*H_C_COV_vax
  
  dE_C_COV <- (lambdaC_COV * (1-vaccC_COV))*S_C - epsilon_COV*E_C_COV
  dI_C_COV <- epsilon_COV*E_C_COV - gammaI_COV*I_C_COV
  dH_C_COV <- probHC_COV*gammaI_COV*I_C_COV - gammaH_COV*H_C_COV
  dD_C_COV <- alphaC_COV*gammaH_COV*H_C_COV
  
  ### Older Children
  dE_OC_COV_vax <- (lambdaOC_COV * vaccOC_COV * ve_COV)*S_OC - epsilon_COV*E_OC_COV_vax
  dI_OC_COV_vax <- epsilon_COV*E_OC_COV_vax - gammaI_COV*I_OC_COV_vax
  dH_OC_COV_vax <- probHOC_COV_vax*gammaI_COV*I_OC_COV_vax - gammaH_COV*H_OC_COV_vax
  dD_OC_COV_vax <- alphaOC_COV*gammaH_COV*H_OC_COV_vax
  
  dE_OC_COV <- (lambdaOC_COV * (1-vaccOC_COV))*S_OC - epsilon_COV*E_OC_COV
  dI_OC_COV <- epsilon_COV*E_OC_COV - gammaI_COV*I_OC_COV
  dH_OC_COV <- probHOC_COV*gammaI_COV*I_OC_COV - gammaH_COV*H_OC_COV
  dD_OC_COV <- alphaOC_COV*gammaH_COV*H_OC_COV
  
  ### Adults
  dE_A_COV_vax <- (lambdaA_COV * vaccA_COV * ve_COV)*S_A - epsilon_COV*E_A_COV_vax
  dI_A_COV_vax <- epsilon_COV*E_A_COV_vax - gammaI_COV*I_A_COV_vax
  dH_A_COV_vax <- probHA_COV_vax*gammaI_COV*I_A_COV_vax - gammaH_COV*H_A_COV_vax
  dD_A_COV_vax <- alphaA_COV*gammaH_COV*H_A_COV_vax
  
  dE_A_COV <- (lambdaA_COV * (1-vaccA_COV))*S_A - epsilon_COV*E_A_COV
  dI_A_COV <- epsilon_COV*E_A_COV - gammaI_COV*I_A_COV
  dH_A_COV <- probHA_COV*gammaI_COV*I_A_COV - gammaH_COV*H_A_COV
  dD_A_COV <- alphaA_COV*gammaH_COV*H_A_COV
  
  ### Seniors
  dE_S_COV_vax <- (lambdaS_COV * vaccS_COV * ve_COV)*S_S - epsilon_COV*E_S_COV_vax
  dI_S_COV_vax <- epsilon_COV*E_S_COV_vax - gammaI_COV*I_S_COV_vax
  dH_S_COV_vax <- probHS_COV_vax*gammaI_COV*I_S_COV_vax - gammaH_COV*H_S_COV_vax
  dD_S_COV_vax <- alphaS_COV*gammaH_COV*H_S_COV_vax
  
  dE_S_COV <- (lambdaS_COV * (1-vaccS_COV))*S_S - epsilon_COV*E_S_COV
  dI_S_COV <- epsilon_COV*E_S_COV - gammaI_COV*I_S_COV
  dH_S_COV <- probHS_COV*gammaI_COV*I_S_COV - gammaH_COV*H_S_COV
  dD_S_COV <- alphaS_COV*gammaH_COV*H_S_COV
  
  ## FLU
  ### Children Updates
  dE_C_FLU_vax <- (lambdaC_FLU * vaccC_FLU * ve_FLU)*S_C - epsilon_FLU*E_C_FLU_vax
  dI_C_FLU_vax <- epsilon_FLU*E_C_FLU_vax - gammaI_FLU*I_C_FLU_vax
  dH_C_FLU_vax <- probHC_FLU_vax*gammaI_FLU*I_C_FLU_vax - gammaH_FLU*H_C_FLU_vax
  dD_C_FLU_vax <- alphaC_FLU*gammaH_FLU*H_C_FLU_vax
  
  dE_C_FLU <- (lambdaC_FLU * (1-vaccC_FLU))*S_C - epsilon_FLU*E_C_FLU
  dI_C_FLU <- epsilon_FLU*E_C_FLU - gammaI_FLU*I_C_FLU
  dH_C_FLU <- probHC_FLU*gammaI_FLU*I_C_FLU - gammaH_FLU*H_C_FLU
  dD_C_FLU <- alphaC_FLU*gammaH_FLU*H_C_FLU
  
  ### Older Children
  dE_OC_FLU_vax <- (lambdaOC_FLU * vaccOC_FLU * ve_FLU)*S_OC - epsilon_FLU*E_OC_FLU_vax
  dI_OC_FLU_vax <- epsilon_FLU*E_OC_FLU_vax - gammaI_FLU*I_OC_FLU_vax
  dH_OC_FLU_vax <- probHOC_FLU_vax*gammaI_FLU*I_OC_FLU_vax - gammaH_FLU*H_OC_FLU_vax
  dD_OC_FLU_vax <- alphaOC_FLU*gammaH_FLU*H_OC_FLU_vax
  
  dE_OC_FLU <- (lambdaOC_FLU * (1-vaccOC_FLU))*S_OC - epsilon_FLU*E_OC_FLU
  dI_OC_FLU <- epsilon_FLU*E_OC_FLU - gammaI_FLU*I_OC_FLU
  dH_OC_FLU <- probHOC_FLU*gammaI_FLU*I_OC_FLU - gammaH_FLU*H_OC_FLU
  dD_OC_FLU <- alphaOC_FLU*gammaH_FLU*H_OC_FLU
  
  ### Adults
  dE_A_FLU_vax <- (lambdaA_FLU * vaccA_FLU * ve_FLU)*S_A - epsilon_FLU*E_A_FLU_vax
  dI_A_FLU_vax <- epsilon_FLU*E_A_FLU_vax - gammaI_FLU*I_A_FLU_vax
  dH_A_FLU_vax <- probHA_FLU_vax*gammaI_FLU*I_A_FLU_vax - gammaH_FLU*H_A_FLU_vax
  dD_A_FLU_vax <- alphaA_FLU*gammaH_FLU*H_A_FLU_vax
  
  dE_A_FLU <- (lambdaA_FLU * (1-vaccA_FLU))*S_A - epsilon_FLU*E_A_FLU
  dI_A_FLU <- epsilon_FLU*E_A_FLU - gammaI_FLU*I_A_FLU
  dH_A_FLU <- probHA_FLU*gammaI_FLU*I_A_FLU - gammaH_FLU*H_A_FLU
  dD_A_FLU <- alphaA_FLU*gammaH_FLU*H_A_FLU
  
  ### Seniors
  dE_S_FLU_vax <- (lambdaS_FLU * vaccS_FLU * ve_FLU)*S_S - epsilon_FLU*E_S_FLU_vax
  dI_S_FLU_vax <- epsilon_FLU*E_S_FLU_vax - gammaI_FLU*I_S_FLU_vax
  dH_S_FLU_vax <- probHS_FLU_vax*gammaI_FLU*I_S_FLU_vax - gammaH_FLU*H_S_FLU_vax
  dD_S_FLU_vax <- alphaS_FLU*gammaH_FLU*H_S_FLU_vax
  
  dE_S_FLU <- (lambdaS_FLU * (1-vaccS_FLU))*S_S - epsilon_FLU*E_S_FLU
  dI_S_FLU <- epsilon_FLU*E_S_FLU - gammaI_FLU*I_S_FLU
  dH_S_FLU <- probHS_FLU*gammaI_FLU*I_S_FLU - gammaH_FLU*H_S_FLU
  dD_S_FLU <- alphaS_FLU*gammaH_FLU*H_S_FLU
  
  
  # Return list of gradients
  list(c(
    dS_C, dS_OC, dS_A, dS_S,
    
    dE_C_RSV_vax, dI_C_RSV_vax, dH_C_RSV_vax, dD_C_RSV_vax,
    dE_C_RSV, dI_C_RSV, dH_C_RSV, dD_C_RSV,
    
    dE_OC_RSV_vax, dI_OC_RSV_vax, dH_OC_RSV_vax, dD_OC_RSV_vax,
    dE_OC_RSV, dI_OC_RSV, dH_OC_RSV, dD_OC_RSV,
    
    dE_A_RSV_vax, dI_A_RSV_vax, dH_A_RSV_vax, dD_A_RSV_vax,
    dE_A_RSV, dI_A_RSV, dH_A_RSV, dD_A_RSV,
    
    dE_S_RSV_vax, dI_S_RSV_vax, dH_S_RSV_vax, dD_S_RSV_vax,
    dE_S_RSV, dI_S_RSV, dH_S_RSV, dD_S_RSV,
    
    dE_C_COV_vax, dI_C_COV_vax, dH_C_COV_vax, dD_C_COV_vax,
    dE_C_COV, dI_C_COV, dH_C_COV, dD_C_COV,
    
    dE_OC_COV_vax, dI_OC_COV_vax, dH_OC_COV_vax, dD_OC_COV_vax,
    dE_OC_COV, dI_OC_COV, dH_OC_COV, dD_OC_COV,
    
    dE_A_COV_vax, dI_A_COV_vax, dH_A_COV_vax, dD_A_COV_vax,
    dE_A_COV, dI_A_COV, dH_A_COV, dD_A_COV,
    
    dE_S_COV_vax, dI_S_COV_vax, dH_S_COV_vax, dD_S_COV_vax,
    dE_S_COV, dI_S_COV, dH_S_COV, dD_S_COV,
    
    dE_C_FLU_vax, dI_C_FLU_vax, dH_C_FLU_vax, dD_C_FLU_vax,
    dE_C_FLU, dI_C_FLU, dH_C_FLU, dD_C_FLU,
    
    dE_OC_FLU_vax, dI_OC_FLU_vax, dH_OC_FLU_vax, dD_OC_FLU_vax,
    dE_OC_FLU, dI_OC_FLU, dH_OC_FLU, dD_OC_FLU,
    
    dE_A_FLU_vax, dI_A_FLU_vax, dH_A_FLU_vax, dD_A_FLU_vax,
    dE_A_FLU, dI_A_FLU, dH_A_FLU, dD_A_FLU,
    
    dE_S_FLU_vax, dI_S_FLU_vax, dH_S_FLU_vax, dD_S_FLU_vax,
    dE_S_FLU, dI_S_FLU, dH_S_FLU, dD_S_FLU
  ))
  
  
  
}