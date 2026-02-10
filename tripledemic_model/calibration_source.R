# Functions for use for model calibration

# SEIR Functions per pathogen 
seir_COV <- function(t, y, pars) {
  
  # Bring all params into current function environment 
  list2env(as.list(y), envir=environment())
  list2env(as.list(pars), envir=environment())
 
  
  COV_C = c(E_C_COV_vax, I_C_COV_vax, H_C_COV_vax,
            E_C_COV, I_C_COV, H_C_COV)
  
  COV_OC = c(E_OC_COV_vax, I_OC_COV_vax, H_OC_COV_vax,
             E_OC_COV, I_OC_COV, H_OC_COV)
  
  COV_A = c(E_A_COV_vax, I_A_COV_vax, H_A_COV_vax,
            E_A_COV, I_A_COV, H_A_COV)
  
  COV_S = c(E_S_COV_vax, I_S_COV_vax, H_S_COV_vax,
            E_S_COV, I_S_COV, H_S_COV)
  
  
  sumIC_COV <- I_C_COV + si_COV*I_C_COV_vax + H_C_COV + si_COV*H_C_COV_vax
  popC <- S_C + sum(COV_C)
  sumIOC_COV <- I_OC_COV + si_COV*I_OC_COV_vax + H_OC_COV + si_COV*H_OC_COV_vax
  popOC <- S_OC + sum(COV_OC)
  sumIA_COV <- I_A_COV + si_COV*I_A_COV_vax + H_A_COV + si_COV*H_A_COV_vax
  popA <- S_A + sum(COV_A)
  sumIS_COV <- I_S_COV + si_COV*I_S_COV_vax + H_S_COV + si_COV*H_S_COV_vax
  popS <- S_S + sum(COV_S)
  
  lambdaC_COV <- beta_COV(t)*(cCC*sumIC_COV/popC + cCOC*sumIOC_COV/popOC + cCA*sumIA_COV/popA + cCS*sumIS_COV/popS)
  lambdaOC_COV <- beta_COV(t)*(cCOC*sumIC_COV/popC + cOCOC*sumIOC_COV/popOC + cOCA*sumIA_COV/popA + cOCS*sumIS_COV/popS)
  lambdaA_COV <- beta_COV(t)*(cCA*sumIC_COV/popC + cOCA*sumIOC_COV/popOC + cAA*sumIA_COV/popA + cAS*sumIS_COV/popS)
  lambdaS_COV <- beta_COV(t)*(cCS*sumIC_COV/popC + cOCS*sumIOC_COV/popOC + cAS*sumIA_COV/popA + cSS*sumIS_COV/popS)
  
  prop_can_be_hospitalized <- 1
  
  # Updates
  dS_C <- -((lambdaC_COV * ((vaccC_COV * ve_COV) +  (1-vaccC_COV))))*S_C + omega_COV*R_C_COV
  dS_OC <- -((lambdaOC_COV * ((vaccOC_COV * ve_COV) +  (1-vaccOC_COV))))*S_OC + omega_COV*R_OC_COV
  dS_A <- -((lambdaA_COV * ((vaccA_COV * ve_COV) +  (1-vaccA_COV))))*S_A + omega_COV*R_A_COV
  dS_S <- -((lambdaS_COV * ((vaccS_COV * ve_COV) +  (1-vaccS_COV))))*S_S + omega_COV*R_S_COV
  
  ## COVID
  ### Children Updates
  dE_C_COV_vax <- (lambdaC_COV * vaccC_COV * ve_COV)*S_C - epsilon_COV*E_C_COV_vax 
  dI_C_COV_vax <- epsilon_COV*E_C_COV_vax - gammaI_COV*I_C_COV_vax
  dH_C_COV_vax <-  -gammaH_COV*H_C_COV_vax + prop_can_be_hospitalized*probHC_COV_vax*gammaI_COV*I_C_COV_vax
  dD_C_COV_vax <- alphaC_COV*gammaH_COV*H_C_COV_vax + (1-prop_can_be_hospitalized)*probHC_COV_vax*gammaI_COV*I_C_COV_vax
  
  dE_C_COV <- (lambdaC_COV * (1-vaccC_COV))*S_C - epsilon_COV*E_C_COV  
  dI_C_COV <- epsilon_COV*E_C_COV - gammaI_COV*I_C_COV
  dH_C_COV <- -gammaH_COV*H_C_COV + prop_can_be_hospitalized*probHC_COV*gammaI_COV*I_C_COV
  dD_C_COV <- alphaC_COV*gammaH_COV*H_C_COV + (1-prop_can_be_hospitalized)*probHC_COV*gammaI_COV*I_C_COV
  
  dR_C_COV <- (1-probHC_COV)*gammaI_COV*I_C_COV + (1-alphaC_COV)*gammaH_COV*H_C_COV +
    (1-probHC_COV_vax)*gammaI_COV*I_C_COV_vax + (1-alphaC_COV)*gammaH_COV*H_C_COV_vax - omega_COV*R_C_COV 
  
  ### Older Children
  dE_OC_COV_vax <- (lambdaOC_COV * vaccOC_COV * ve_COV)*S_OC - epsilon_COV*E_OC_COV_vax 
  dI_OC_COV_vax <- epsilon_COV*E_OC_COV_vax - gammaI_COV*I_OC_COV_vax
  dH_OC_COV_vax <- - gammaH_COV*H_OC_COV_vax + prop_can_be_hospitalized*probHOC_COV_vax*gammaI_COV*I_OC_COV_vax
  dD_OC_COV_vax <- alphaOC_COV*gammaH_COV*H_OC_COV_vax + (1-prop_can_be_hospitalized)*probHOC_COV_vax*gammaI_COV*I_OC_COV_vax
  
  
  dE_OC_COV <- (lambdaOC_COV * (1-vaccOC_COV))*S_OC - epsilon_COV*E_OC_COV 
  dI_OC_COV <- epsilon_COV*E_OC_COV - gammaI_COV*I_OC_COV
  dH_OC_COV <-  -gammaH_COV*H_OC_COV + prop_can_be_hospitalized*probHOC_COV*gammaI_COV*I_OC_COV
  dD_OC_COV <- alphaOC_COV*gammaH_COV*H_OC_COV + (1-prop_can_be_hospitalized)*probHOC_COV*gammaI_COV*I_OC_COV
  
  dR_OC_COV <- (1-probHOC_COV)*gammaI_COV*I_OC_COV + (1-alphaOC_COV)*gammaH_COV*H_OC_COV +
    (1-probHOC_COV_vax)*gammaI_COV*I_OC_COV_vax + (1-alphaOC_COV)*gammaH_COV*H_OC_COV_vax - omega_COV*R_OC_COV 
  
  ### Adults
  dE_A_COV_vax <- (lambdaA_COV * vaccA_COV * ve_COV)*S_A - epsilon_COV*E_A_COV_vax 
  dI_A_COV_vax <- epsilon_COV*E_A_COV_vax - gammaI_COV*I_A_COV_vax
  dH_A_COV_vax <- -gammaH_COV*H_A_COV_vax + prop_can_be_hospitalized*probHA_COV_vax*gammaI_COV*I_A_COV_vax
  dD_A_COV_vax <- alphaA_COV*gammaH_COV*H_A_COV_vax + (1-prop_can_be_hospitalized)*probHA_COV_vax*gammaI_COV*I_A_COV_vax
  
  
  dE_A_COV <- (lambdaA_COV * (1-vaccA_COV))*S_A - epsilon_COV*E_A_COV 
  dI_A_COV <- epsilon_COV*E_A_COV - gammaI_COV*I_A_COV
  dH_A_COV <- -gammaH_COV*H_A_COV + prop_can_be_hospitalized*probHA_COV*gammaI_COV*I_A_COV
  dD_A_COV <- alphaA_COV*gammaH_COV*H_A_COV + (1-prop_can_be_hospitalized)*probHA_COV*gammaI_COV*I_A_COV
  
  dR_A_COV <- (1-probHA_COV)*gammaI_COV*I_A_COV + (1-alphaA_COV)*gammaH_COV*H_A_COV +
    (1-probHA_COV_vax)*gammaI_COV*I_A_COV_vax + (1-alphaA_COV)*gammaH_COV*H_A_COV_vax - omega_COV*R_A_COV 
  
  ### Seniors
  dE_S_COV_vax <- (lambdaS_COV * vaccS_COV * ve_COV)*S_S - epsilon_COV*E_S_COV_vax 
  dI_S_COV_vax <- epsilon_COV*E_S_COV_vax - gammaI_COV*I_S_COV_vax
  dH_S_COV_vax <- -gammaH_COV*H_S_COV_vax + prop_can_be_hospitalized*probHS_COV_vax*gammaI_COV*I_S_COV_vax
  dD_S_COV_vax <- alphaS_COV*gammaH_COV*H_S_COV_vax + (1-prop_can_be_hospitalized)*probHS_COV_vax*gammaI_COV*I_S_COV_vax
  
  
  dE_S_COV <- (lambdaS_COV * (1-vaccS_COV))*S_S - epsilon_COV*E_S_COV 
  dI_S_COV <- epsilon_COV*E_S_COV - gammaI_COV*I_S_COV
  dH_S_COV <- -gammaH_COV*H_S_COV + prop_can_be_hospitalized*probHS_COV*gammaI_COV*I_S_COV
  dD_S_COV <- alphaS_COV*gammaH_COV*H_S_COV + (1-prop_can_be_hospitalized)*probHS_COV*gammaI_COV*I_S_COV
  
  
  dR_S_COV <- (1-probHS_COV)*gammaI_COV*I_S_COV + (1-alphaS_COV)*gammaH_COV*H_S_COV +
    (1-probHS_COV_vax)*gammaI_COV*I_S_COV_vax + (1-alphaS_COV)*gammaH_COV*H_S_COV_vax - omega_COV*R_S_COV 
  
  list(c(
    dS_C, dS_OC, dS_A, dS_S,
    
    dE_C_COV_vax, dI_C_COV_vax, dH_C_COV_vax, dD_C_COV_vax,
    dE_C_COV, dI_C_COV, dH_C_COV, dD_C_COV, dR_C_COV,
    
    dE_OC_COV_vax, dI_OC_COV_vax, dH_OC_COV_vax, dD_OC_COV_vax,
    dE_OC_COV, dI_OC_COV, dH_OC_COV, dD_OC_COV, dR_OC_COV,
    
    dE_A_COV_vax, dI_A_COV_vax, dH_A_COV_vax, dD_A_COV_vax,
    dE_A_COV, dI_A_COV, dH_A_COV, dD_A_COV, dR_A_COV,
    
    dE_S_COV_vax, dI_S_COV_vax, dH_S_COV_vax, dD_S_COV_vax,
    dE_S_COV, dI_S_COV, dH_S_COV, dD_S_COV, dR_S_COV))
  
}

seir_RSV <- function(t, y, pars) {
  
  # Bring all params into current function environment 
  list2env(as.list(y), envir=environment())
  list2env(as.list(pars), envir=environment())
  
  
  RSV_C = c(E_C_RSV_vax, I_C_RSV_vax, H_C_RSV_vax,
            E_C_RSV, I_C_RSV, H_C_RSV)
  
  RSV_OC = c(E_OC_RSV_vax, I_OC_RSV_vax, H_OC_RSV_vax,
             E_OC_RSV, I_OC_RSV, H_OC_RSV)
  
  RSV_A = c(E_A_RSV_vax, I_A_RSV_vax, H_A_RSV_vax,
            E_A_RSV, I_A_RSV, H_A_RSV)
  
  RSV_S = c(E_S_RSV_vax, I_S_RSV_vax, H_S_RSV_vax,
            E_S_RSV, I_S_RSV, H_S_RSV)
  
  
  sumIC_RSV <- I_C_RSV + si_RSV*I_C_RSV_vax + H_C_RSV + si_RSV*H_C_RSV_vax
  popC <- S_C + sum(RSV_C)
  sumIOC_RSV <- I_OC_RSV + si_RSV*I_OC_RSV_vax + H_OC_RSV + si_RSV*H_OC_RSV_vax
  popOC <- S_OC + sum(RSV_OC)
  sumIA_RSV <- I_A_RSV + si_RSV*I_A_RSV_vax + H_A_RSV + si_RSV*H_A_RSV_vax
  popA <- S_A + sum(RSV_A)
  sumIS_RSV <- I_S_RSV + si_RSV*I_S_RSV_vax + H_S_RSV + si_RSV*H_S_RSV_vax
  popS <- S_S + sum(RSV_S)
  
  lambdaC_RSV <- beta_RSV(t)*(cCC*sumIC_RSV/popC + cCOC*sumIOC_RSV/popOC + cCA*sumIA_RSV/popA + cCS*sumIS_RSV/popS)
  lambdaOC_RSV <- beta_RSV(t)*(cCOC*sumIC_RSV/popC + cOCOC*sumIOC_RSV/popOC + cOCA*sumIA_RSV/popA + cOCS*sumIS_RSV/popS)
  lambdaA_RSV <- beta_RSV(t)*(cCA*sumIC_RSV/popC + cOCA*sumIOC_RSV/popOC + cAA*sumIA_RSV/popA + cAS*sumIS_RSV/popS)
  lambdaS_RSV <- beta_RSV(t)*(cCS*sumIC_RSV/popC + cOCS*sumIOC_RSV/popOC + cAS*sumIA_RSV/popA + cSS*sumIS_RSV/popS)
  
  prop_can_be_hospitalized <- 1
  
  # Updates
  dS_C <- -((lambdaC_RSV * ((vaccC_RSV * ve_RSV) +  (1-vaccC_RSV))))*S_C + omega_RSV*R_C_RSV
  dS_OC <- -((lambdaOC_RSV * ((vaccOC_RSV * ve_RSV) +  (1-vaccOC_RSV))))*S_OC + omega_RSV*R_OC_RSV
  dS_A <- -((lambdaA_RSV * ((vaccA_RSV * ve_RSV) +  (1-vaccA_RSV))))*S_A + omega_RSV*R_A_RSV
  dS_S <- -((lambdaS_RSV * ((vaccS_RSV * ve_RSV) +  (1-vaccS_RSV))))*S_S + omega_RSV*R_S_RSV
  
  ## RSVID
  ### Children Updates
  dE_C_RSV_vax <- (lambdaC_RSV * vaccC_RSV * ve_RSV)*S_C - epsilon_RSV*E_C_RSV_vax 
  dI_C_RSV_vax <- epsilon_RSV*E_C_RSV_vax - gammaI_RSV*I_C_RSV_vax
  dH_C_RSV_vax <-  -gammaH_RSV*H_C_RSV_vax + prop_can_be_hospitalized*probHC_RSV_vax*gammaI_RSV*I_C_RSV_vax
  dD_C_RSV_vax <- alphaC_RSV*gammaH_RSV*H_C_RSV_vax + (1-prop_can_be_hospitalized)*probHC_RSV_vax*gammaI_RSV*I_C_RSV_vax
  
  dE_C_RSV <- (lambdaC_RSV * (1-vaccC_RSV))*S_C - epsilon_RSV*E_C_RSV  
  dI_C_RSV <- epsilon_RSV*E_C_RSV - gammaI_RSV*I_C_RSV
  dH_C_RSV <- -gammaH_RSV*H_C_RSV + prop_can_be_hospitalized*probHC_RSV*gammaI_RSV*I_C_RSV
  dD_C_RSV <- alphaC_RSV*gammaH_RSV*H_C_RSV + (1-prop_can_be_hospitalized)*probHC_RSV*gammaI_RSV*I_C_RSV
  
  dR_C_RSV <- (1-probHC_RSV)*gammaI_RSV*I_C_RSV + (1-alphaC_RSV)*gammaH_RSV*H_C_RSV +
    (1-probHC_RSV_vax)*gammaI_RSV*I_C_RSV_vax + (1-alphaC_RSV)*gammaH_RSV*H_C_RSV_vax - omega_RSV*R_C_RSV 
  
  ### Older Children
  dE_OC_RSV_vax <- (lambdaOC_RSV * vaccOC_RSV * ve_RSV)*S_OC - epsilon_RSV*E_OC_RSV_vax 
  dI_OC_RSV_vax <- epsilon_RSV*E_OC_RSV_vax - gammaI_RSV*I_OC_RSV_vax
  dH_OC_RSV_vax <- - gammaH_RSV*H_OC_RSV_vax + prop_can_be_hospitalized*probHOC_RSV_vax*gammaI_RSV*I_OC_RSV_vax
  dD_OC_RSV_vax <- alphaOC_RSV*gammaH_RSV*H_OC_RSV_vax + (1-prop_can_be_hospitalized)*probHOC_RSV_vax*gammaI_RSV*I_OC_RSV_vax
  
  
  dE_OC_RSV <- (lambdaOC_RSV * (1-vaccOC_RSV))*S_OC - epsilon_RSV*E_OC_RSV 
  dI_OC_RSV <- epsilon_RSV*E_OC_RSV - gammaI_RSV*I_OC_RSV
  dH_OC_RSV <-  -gammaH_RSV*H_OC_RSV + prop_can_be_hospitalized*probHOC_RSV*gammaI_RSV*I_OC_RSV
  dD_OC_RSV <- alphaOC_RSV*gammaH_RSV*H_OC_RSV + (1-prop_can_be_hospitalized)*probHOC_RSV*gammaI_RSV*I_OC_RSV
  
  dR_OC_RSV <- (1-probHOC_RSV)*gammaI_RSV*I_OC_RSV + (1-alphaOC_RSV)*gammaH_RSV*H_OC_RSV +
    (1-probHOC_RSV_vax)*gammaI_RSV*I_OC_RSV_vax + (1-alphaOC_RSV)*gammaH_RSV*H_OC_RSV_vax - omega_RSV*R_OC_RSV 
  
  ### Adults
  dE_A_RSV_vax <- (lambdaA_RSV * vaccA_RSV * ve_RSV)*S_A - epsilon_RSV*E_A_RSV_vax 
  dI_A_RSV_vax <- epsilon_RSV*E_A_RSV_vax - gammaI_RSV*I_A_RSV_vax
  dH_A_RSV_vax <- -gammaH_RSV*H_A_RSV_vax + prop_can_be_hospitalized*probHA_RSV_vax*gammaI_RSV*I_A_RSV_vax
  dD_A_RSV_vax <- alphaA_RSV*gammaH_RSV*H_A_RSV_vax + (1-prop_can_be_hospitalized)*probHA_RSV_vax*gammaI_RSV*I_A_RSV_vax
  
  
  dE_A_RSV <- (lambdaA_RSV * (1-vaccA_RSV))*S_A - epsilon_RSV*E_A_RSV 
  dI_A_RSV <- epsilon_RSV*E_A_RSV - gammaI_RSV*I_A_RSV
  dH_A_RSV <- -gammaH_RSV*H_A_RSV + prop_can_be_hospitalized*probHA_RSV*gammaI_RSV*I_A_RSV
  dD_A_RSV <- alphaA_RSV*gammaH_RSV*H_A_RSV + (1-prop_can_be_hospitalized)*probHA_RSV*gammaI_RSV*I_A_RSV
  
  dR_A_RSV <- (1-probHA_RSV)*gammaI_RSV*I_A_RSV + (1-alphaA_RSV)*gammaH_RSV*H_A_RSV +
    (1-probHA_RSV_vax)*gammaI_RSV*I_A_RSV_vax + (1-alphaA_RSV)*gammaH_RSV*H_A_RSV_vax - omega_RSV*R_A_RSV 
  
  ### Seniors
  dE_S_RSV_vax <- (lambdaS_RSV * vaccS_RSV * ve_RSV)*S_S - epsilon_RSV*E_S_RSV_vax 
  dI_S_RSV_vax <- epsilon_RSV*E_S_RSV_vax - gammaI_RSV*I_S_RSV_vax
  dH_S_RSV_vax <- -gammaH_RSV*H_S_RSV_vax + prop_can_be_hospitalized*probHS_RSV_vax*gammaI_RSV*I_S_RSV_vax
  dD_S_RSV_vax <- alphaS_RSV*gammaH_RSV*H_S_RSV_vax + (1-prop_can_be_hospitalized)*probHS_RSV_vax*gammaI_RSV*I_S_RSV_vax
  
  
  dE_S_RSV <- (lambdaS_RSV * (1-vaccS_RSV))*S_S - epsilon_RSV*E_S_RSV 
  dI_S_RSV <- epsilon_RSV*E_S_RSV - gammaI_RSV*I_S_RSV
  dH_S_RSV <- -gammaH_RSV*H_S_RSV + prop_can_be_hospitalized*probHS_RSV*gammaI_RSV*I_S_RSV
  dD_S_RSV <- alphaS_RSV*gammaH_RSV*H_S_RSV + (1-prop_can_be_hospitalized)*probHS_RSV*gammaI_RSV*I_S_RSV
  
  
  dR_S_RSV <- (1-probHS_RSV)*gammaI_RSV*I_S_RSV + (1-alphaS_RSV)*gammaH_RSV*H_S_RSV +
    (1-probHS_RSV_vax)*gammaI_RSV*I_S_RSV_vax + (1-alphaS_RSV)*gammaH_RSV*H_S_RSV_vax - omega_RSV*R_S_RSV 
  
  list(c(
    dS_C, dS_OC, dS_A, dS_S,
    
    dE_C_RSV_vax, dI_C_RSV_vax, dH_C_RSV_vax, dD_C_RSV_vax,
    dE_C_RSV, dI_C_RSV, dH_C_RSV, dD_C_RSV, dR_C_RSV,
    
    dE_OC_RSV_vax, dI_OC_RSV_vax, dH_OC_RSV_vax, dD_OC_RSV_vax,
    dE_OC_RSV, dI_OC_RSV, dH_OC_RSV, dD_OC_RSV, dR_OC_RSV,
    
    dE_A_RSV_vax, dI_A_RSV_vax, dH_A_RSV_vax, dD_A_RSV_vax,
    dE_A_RSV, dI_A_RSV, dH_A_RSV, dD_A_RSV, dR_A_RSV,
    
    dE_S_RSV_vax, dI_S_RSV_vax, dH_S_RSV_vax, dD_S_RSV_vax,
    dE_S_RSV, dI_S_RSV, dH_S_RSV, dD_S_RSV, dR_S_RSV))
  
}

seir_FLU <- function(t, y, pars) {
  
  # Bring all params into current function environment 
  list2env(as.list(y), envir=environment())
  list2env(as.list(pars), envir=environment())
  
  
  FLU_C = c(E_C_FLU_vax, I_C_FLU_vax, H_C_FLU_vax,
            E_C_FLU, I_C_FLU, H_C_FLU)
  
  FLU_OC = c(E_OC_FLU_vax, I_OC_FLU_vax, H_OC_FLU_vax,
             E_OC_FLU, I_OC_FLU, H_OC_FLU)
  
  FLU_A = c(E_A_FLU_vax, I_A_FLU_vax, H_A_FLU_vax,
            E_A_FLU, I_A_FLU, H_A_FLU)
  
  FLU_S = c(E_S_FLU_vax, I_S_FLU_vax, H_S_FLU_vax,
            E_S_FLU, I_S_FLU, H_S_FLU)
  
  
  sumIC_FLU <- I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax
  popC <- S_C + sum(FLU_C)
  sumIOC_FLU <- I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax
  popOC <- S_OC + sum(FLU_OC)
  sumIA_FLU <- I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax
  popA <- S_A + sum(FLU_A)
  sumIS_FLU <- I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax
  popS <- S_S + sum(FLU_S)
  
  lambdaC_FLU <- beta_FLU(t)*(cCC*sumIC_FLU/popC + cCOC*sumIOC_FLU/popOC + cCA*sumIA_FLU/popA + cCS*sumIS_FLU/popS)
  lambdaOC_FLU <- beta_FLU(t)*(cCOC*sumIC_FLU/popC + cOCOC*sumIOC_FLU/popOC + cOCA*sumIA_FLU/popA + cOCS*sumIS_FLU/popS)
  lambdaA_FLU <- beta_FLU(t)*(cCA*sumIC_FLU/popC + cOCA*sumIOC_FLU/popOC + cAA*sumIA_FLU/popA + cAS*sumIS_FLU/popS)
  lambdaS_FLU <- beta_FLU(t)*(cCS*sumIC_FLU/popC + cOCS*sumIOC_FLU/popOC + cAS*sumIA_FLU/popA + cSS*sumIS_FLU/popS)
  
  prop_can_be_hospitalized <- 1
  
  # Updates
  dS_C <- -((lambdaC_FLU * ((vaccC_FLU * ve_FLU) +  (1-vaccC_FLU))))*S_C + omega_FLU*R_C_FLU
  dS_OC <- -((lambdaOC_FLU * ((vaccOC_FLU * ve_FLU) +  (1-vaccOC_FLU))))*S_OC + omega_FLU*R_OC_FLU
  dS_A <- -((lambdaA_FLU * ((vaccA_FLU * ve_FLU) +  (1-vaccA_FLU))))*S_A + omega_FLU*R_A_FLU
  dS_S <- -((lambdaS_FLU * ((vaccS_FLU * ve_FLU) +  (1-vaccS_FLU))))*S_S + omega_FLU*R_S_FLU
  
  ## FLUID
  ### Children Updates
  dE_C_FLU_vax <- (lambdaC_FLU * vaccC_FLU * ve_FLU)*S_C - epsilon_FLU*E_C_FLU_vax 
  dI_C_FLU_vax <- epsilon_FLU*E_C_FLU_vax - gammaI_FLU*I_C_FLU_vax
  dH_C_FLU_vax <-  -gammaH_FLU*H_C_FLU_vax + prop_can_be_hospitalized*probHC_FLU_vax*gammaI_FLU*I_C_FLU_vax
  dD_C_FLU_vax <- alphaC_FLU*gammaH_FLU*H_C_FLU_vax + (1-prop_can_be_hospitalized)*probHC_FLU_vax*gammaI_FLU*I_C_FLU_vax
  
  dE_C_FLU <- (lambdaC_FLU * (1-vaccC_FLU))*S_C - epsilon_FLU*E_C_FLU  
  dI_C_FLU <- epsilon_FLU*E_C_FLU - gammaI_FLU*I_C_FLU
  dH_C_FLU <- -gammaH_FLU*H_C_FLU + prop_can_be_hospitalized*probHC_FLU*gammaI_FLU*I_C_FLU
  dD_C_FLU <- alphaC_FLU*gammaH_FLU*H_C_FLU + (1-prop_can_be_hospitalized)*probHC_FLU*gammaI_FLU*I_C_FLU
  
  dR_C_FLU <- (1-probHC_FLU)*gammaI_FLU*I_C_FLU + (1-alphaC_FLU)*gammaH_FLU*H_C_FLU +
    (1-probHC_FLU_vax)*gammaI_FLU*I_C_FLU_vax + (1-alphaC_FLU)*gammaH_FLU*H_C_FLU_vax - omega_FLU*R_C_FLU 
  
  ### Older Children
  dE_OC_FLU_vax <- (lambdaOC_FLU * vaccOC_FLU * ve_FLU)*S_OC - epsilon_FLU*E_OC_FLU_vax 
  dI_OC_FLU_vax <- epsilon_FLU*E_OC_FLU_vax - gammaI_FLU*I_OC_FLU_vax
  dH_OC_FLU_vax <- - gammaH_FLU*H_OC_FLU_vax + prop_can_be_hospitalized*probHOC_FLU_vax*gammaI_FLU*I_OC_FLU_vax
  dD_OC_FLU_vax <- alphaOC_FLU*gammaH_FLU*H_OC_FLU_vax + (1-prop_can_be_hospitalized)*probHOC_FLU_vax*gammaI_FLU*I_OC_FLU_vax
  
  
  dE_OC_FLU <- (lambdaOC_FLU * (1-vaccOC_FLU))*S_OC - epsilon_FLU*E_OC_FLU 
  dI_OC_FLU <- epsilon_FLU*E_OC_FLU - gammaI_FLU*I_OC_FLU
  dH_OC_FLU <-  -gammaH_FLU*H_OC_FLU + prop_can_be_hospitalized*probHOC_FLU*gammaI_FLU*I_OC_FLU
  dD_OC_FLU <- alphaOC_FLU*gammaH_FLU*H_OC_FLU + (1-prop_can_be_hospitalized)*probHOC_FLU*gammaI_FLU*I_OC_FLU
  
  dR_OC_FLU <- (1-probHOC_FLU)*gammaI_FLU*I_OC_FLU + (1-alphaOC_FLU)*gammaH_FLU*H_OC_FLU +
    (1-probHOC_FLU_vax)*gammaI_FLU*I_OC_FLU_vax + (1-alphaOC_FLU)*gammaH_FLU*H_OC_FLU_vax - omega_FLU*R_OC_FLU 
  
  ### Adults
  dE_A_FLU_vax <- (lambdaA_FLU * vaccA_FLU * ve_FLU)*S_A - epsilon_FLU*E_A_FLU_vax 
  dI_A_FLU_vax <- epsilon_FLU*E_A_FLU_vax - gammaI_FLU*I_A_FLU_vax
  dH_A_FLU_vax <- -gammaH_FLU*H_A_FLU_vax + prop_can_be_hospitalized*probHA_FLU_vax*gammaI_FLU*I_A_FLU_vax
  dD_A_FLU_vax <- alphaA_FLU*gammaH_FLU*H_A_FLU_vax + (1-prop_can_be_hospitalized)*probHA_FLU_vax*gammaI_FLU*I_A_FLU_vax
  
  
  dE_A_FLU <- (lambdaA_FLU * (1-vaccA_FLU))*S_A - epsilon_FLU*E_A_FLU 
  dI_A_FLU <- epsilon_FLU*E_A_FLU - gammaI_FLU*I_A_FLU
  dH_A_FLU <- -gammaH_FLU*H_A_FLU + prop_can_be_hospitalized*probHA_FLU*gammaI_FLU*I_A_FLU
  dD_A_FLU <- alphaA_FLU*gammaH_FLU*H_A_FLU + (1-prop_can_be_hospitalized)*probHA_FLU*gammaI_FLU*I_A_FLU
  
  dR_A_FLU <- (1-probHA_FLU)*gammaI_FLU*I_A_FLU + (1-alphaA_FLU)*gammaH_FLU*H_A_FLU +
    (1-probHA_FLU_vax)*gammaI_FLU*I_A_FLU_vax + (1-alphaA_FLU)*gammaH_FLU*H_A_FLU_vax - omega_FLU*R_A_FLU 
  
  ### Seniors
  dE_S_FLU_vax <- (lambdaS_FLU * vaccS_FLU * ve_FLU)*S_S - epsilon_FLU*E_S_FLU_vax 
  dI_S_FLU_vax <- epsilon_FLU*E_S_FLU_vax - gammaI_FLU*I_S_FLU_vax
  dH_S_FLU_vax <- -gammaH_FLU*H_S_FLU_vax + prop_can_be_hospitalized*probHS_FLU_vax*gammaI_FLU*I_S_FLU_vax
  dD_S_FLU_vax <- alphaS_FLU*gammaH_FLU*H_S_FLU_vax + (1-prop_can_be_hospitalized)*probHS_FLU_vax*gammaI_FLU*I_S_FLU_vax
  
  
  dE_S_FLU <- (lambdaS_FLU * (1-vaccS_FLU))*S_S - epsilon_FLU*E_S_FLU 
  dI_S_FLU <- epsilon_FLU*E_S_FLU - gammaI_FLU*I_S_FLU
  dH_S_FLU <- -gammaH_FLU*H_S_FLU + prop_can_be_hospitalized*probHS_FLU*gammaI_FLU*I_S_FLU
  dD_S_FLU <- alphaS_FLU*gammaH_FLU*H_S_FLU + (1-prop_can_be_hospitalized)*probHS_FLU*gammaI_FLU*I_S_FLU
  
  
  dR_S_FLU <- (1-probHS_FLU)*gammaI_FLU*I_S_FLU + (1-alphaS_FLU)*gammaH_FLU*H_S_FLU +
    (1-probHS_FLU_vax)*gammaI_FLU*I_S_FLU_vax + (1-alphaS_FLU)*gammaH_FLU*H_S_FLU_vax - omega_FLU*R_S_FLU 
  
  list(c(
    dS_C, dS_OC, dS_A, dS_S,
    
    dE_C_FLU_vax, dI_C_FLU_vax, dH_C_FLU_vax, dD_C_FLU_vax,
    dE_C_FLU, dI_C_FLU, dH_C_FLU, dD_C_FLU, dR_C_FLU,
    
    dE_OC_FLU_vax, dI_OC_FLU_vax, dH_OC_FLU_vax, dD_OC_FLU_vax,
    dE_OC_FLU, dI_OC_FLU, dH_OC_FLU, dD_OC_FLU, dR_OC_FLU,
    
    dE_A_FLU_vax, dI_A_FLU_vax, dH_A_FLU_vax, dD_A_FLU_vax,
    dE_A_FLU, dI_A_FLU, dH_A_FLU, dD_A_FLU, dR_A_FLU,
    
    dE_S_FLU_vax, dI_S_FLU_vax, dH_S_FLU_vax, dD_S_FLU_vax,
    dE_S_FLU, dI_S_FLU, dH_S_FLU, dD_S_FLU, dR_S_FLU))
  
}

seas_matrix <- function(mu_janfeb, mu_marapr, mu_mayjun, mu_julaug, mu_septoct, mu_novdec){
  n <- 365
  x <- 0:(n-1)/(n-1);
  k<- 0:6/6
  matrix <- cSplineDes(x, k, ord = 4, derivs=0) %>%
    as.data.frame() %>%
    mutate(day = 1:365) %>%
    rename(JanFeb = V1, MarApr = V2, MayJun = V3, JulAug = V4, SeptOct = V5, NovDec = V6) %>%
    pivot_longer(JanFeb:NovDec, values_to = "Value", names_to = "Season") %>%
    mutate(spline_scalar = case_when(
      Season == "JanFeb" ~ mu_janfeb,
      Season == "MarApr" ~ mu_marapr,
      Season == "MayJun" ~ mu_mayjun,
      Season == "JulAug" ~ mu_julaug,
      Season == "SeptOct" ~ mu_septoct,
      Season == "NovDec" ~ mu_novdec
    )) %>%
    mutate(Value = spline_scalar*Value) %>%
    group_by(day) %>% 
    summarise(Value = mean(Value)) %>%
    ungroup() %>%
    mutate(day = if_else(day == 365, 0, day))
  return(matrix)
}

cov_loglik <- function(mu1, mu2, mu3, mu4, mu5, mu6,sig_dist) {
  
  # First, feed in standard parms
  paras <- currvac_parms
  # set seasonality
  set_mat <- seas_matrix(mu1, mu2, mu3, 
                         mu4, mu5, mu6)
  ## make sure mu is defined globally so that it actually passes into the seir function
  beta_COV <<- function(t){
    seas_function(t, matrix = set_mat)
  }
  
  # print("Try: ------------------")
  # print(c(mu_janfeb, mu_marapr, mu_mayjun,
  #                        mu_julaug, mu_septoct, mu_novdec))
  # print("-----------------------")
  
  # set initial conditions
  #pr ## prop recovered
  #pinf ## prop currently infected of non-recovered individuals
  
  run_init <- init[c("S_C", "S_OC", "S_A", "S_S",
                     "E_C_COV_vax", "I_C_COV_vax", "H_C_COV_vax", "D_C_COV_vax",
                     "E_C_COV", "I_C_COV", "H_C_COV", "D_C_COV", "R_C_COV",
                     
                     "E_OC_COV_vax", "I_OC_COV_vax", "H_OC_COV_vax", "D_OC_COV_vax",
                     "E_OC_COV", "I_OC_COV", "H_OC_COV", "D_OC_COV", "R_OC_COV",
                     
                     "E_A_COV_vax", "I_A_COV_vax", "H_A_COV_vax", "D_A_COV_vax",
                     "E_A_COV", "I_A_COV", "H_A_COV", "D_A_COV", "R_A_COV",
                     
                     "E_S_COV_vax", "I_S_COV_vax", "H_S_COV_vax", "D_S_COV_vax",
                     "E_S_COV", "I_S_COV", "H_S_COV", "D_S_COV", "R_S_COV")]
  
  # Set times
  times <- 0:365
  
  #vec_print <- c(mu1, mu2, mu3, 
  #               mu4, mu5, mu_novdec,
  #              alpha,
  #            sig_dist)
  
  #print("------- PARAS --------")
  #print(vec_print)
  
  # Run the model 
  out_calib1 <- ode(y=run_init, func = seir_COV, times=times, parms = paras, method = "rk4") %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(time, 
           H_C_COV_vax, H_C_COV, 
           H_OC_COV_vax, H_OC_COV,
           H_A_COV_vax, H_A_COV_vax,
           H_S_COV_vax, H_S_COV
    ) %>%
    group_by(time) %>%
    reframe(
      model_hosps = H_C_COV_vax + H_C_COV + H_OC_COV_vax + H_OC_COV + H_A_COV_vax + H_A_COV_vax + H_S_COV_vax + H_S_COV
    ) %>%
    arrange(time) %>%
    mutate(model_hosps = model_hosps - lag(model_hosps, 7)) %>% ## weekly
    ungroup() %>%
    rename(day = time) %>%
    inner_join(cov_data, by = c("day")) %>%
    # Normalize the data. Here, I'm using the maximum in the observed data for admissions
    arrange(day) %>%
    mutate(max_normal = max(observed_hosps, na.rm = T),
           observed_hosps = observed_hosps/max_normal,
           model_hosps = model_hosps/max_normal) %>%
    filter(!is.na(observed_hosps), !is.na(model_hosps)) %>%
    ungroup()
  
  # print(out_calib1)
  
  #print("Worked to here")
  
  # Now get the negative log likelihood. Note we are concurrently fitting the sd
  ll <- -sum(dnorm(x=out_calib1$model_hosps,mean=out_calib1$observed_hosps,sd=sig_dist,log=TRUE))
  
  # This code ensures that if the output of the log likelihood is NA, the MLE won't stop. Instead it will return
  # an extremely large, positive value of the negative log likelihood and keep iterating.
  ll_final = if_else(is.na(ll), 10^6, ll)
  
  # print(ll_final)
  
  return(ll_final)
}

rsv_loglik <- function(mu1, mu2, mu3, mu4, mu5, mu6,sig_dist) {
  
  # First, feed in standard parms
  paras <- currvac_parms
  # set seasonality
  set_mat <- seas_matrix(mu1, mu2, mu3, 
                         mu4, mu5, mu6)
  ## make sure mu is defined globally so that it actually passes into the seir function
  beta_RSV <<- function(t){
    seas_function(t, matrix = set_mat)
  }
  
  # print("Try: ------------------")
  # print(c(mu_janfeb, mu_marapr, mu_mayjun,
  #                        mu_julaug, mu_septoct, mu_novdec))
  # print("-----------------------")
  
  # set initial conditions
  #pr ## prop recovered
  #pinf ## prop currently infected of non-recovered individuals
  
  run_init <- init[c("S_C", "S_OC", "S_A", "S_S",
                     "E_C_RSV_vax", "I_C_RSV_vax", "H_C_RSV_vax", "D_C_RSV_vax",
                     "E_C_RSV", "I_C_RSV", "H_C_RSV", "D_C_RSV", "R_C_RSV",
                     
                     "E_OC_RSV_vax", "I_OC_RSV_vax", "H_OC_RSV_vax", "D_OC_RSV_vax",
                     "E_OC_RSV", "I_OC_RSV", "H_OC_RSV", "D_OC_RSV", "R_OC_RSV",
                     
                     "E_A_RSV_vax", "I_A_RSV_vax", "H_A_RSV_vax", "D_A_RSV_vax",
                     "E_A_RSV", "I_A_RSV", "H_A_RSV", "D_A_RSV", "R_A_RSV",
                     
                     "E_S_RSV_vax", "I_S_RSV_vax", "H_S_RSV_vax", "D_S_RSV_vax",
                     "E_S_RSV", "I_S_RSV", "H_S_RSV", "D_S_RSV", "R_S_RSV")]
  
  # Set times
  times <- 0:365
  
  #vec_print <- c(mu1, mu2, mu3, 
  #               mu4, mu5, mu_novdec,
  #              alpha,
  #            sig_dist)
  
  #print("------- PARAS --------")
  #print(vec_print)
  
  # Run the model 
  out_calib1 <- ode(y=run_init, func = seir_RSV, times=times, parms = paras, method = "rk4") %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(time, 
           H_C_RSV_vax, H_C_RSV, 
           H_OC_RSV_vax, H_OC_RSV,
           H_A_RSV_vax, H_A_RSV_vax,
           H_S_RSV_vax, H_S_RSV
    ) %>%
    group_by(time) %>%
    reframe(
      model_hosps = H_C_RSV_vax + H_C_RSV + H_OC_RSV_vax + H_OC_RSV + H_A_RSV_vax + H_A_RSV_vax + H_S_RSV_vax + H_S_RSV
    ) %>%
    arrange(time) %>%
    mutate(model_hosps = model_hosps - lag(model_hosps, 7)) %>% ## weekly
    ungroup() %>%
    rename(day = time) %>%
    inner_join(rsv_data, by = c("day")) %>%
    # Normalize the data. Here, I'm using the maximum in the observed data for admissions
    arrange(day) %>%
    mutate(max_normal = max(observed_hosps, na.rm = T),
           observed_hosps = observed_hosps/max_normal,
           model_hosps = model_hosps/max_normal) %>%
    filter(!is.na(observed_hosps), !is.na(model_hosps)) %>%
    ungroup()
  
  # print(out_calib1)
  
  #print("Worked to here")
  
  # Now get the negative log likelihood. Note we are concurrently fitting the sd
  ll <- -sum(dnorm(x=out_calib1$model_hosps,mean=out_calib1$observed_hosps,sd=sig_dist,log=TRUE))
  
  # This code ensures that if the output of the log likelihood is NA, the MLE won't stop. Instead it will return
  # an extremely large, positive value of the negative log likelihood and keep iterating.
  ll_final = if_else(is.na(ll), 10^6, ll)
  
  # print(ll_final)
  
  return(ll_final)
}

flu_loglik <- function(mu1, mu2, mu3, mu4, mu5, mu6,sig_dist) {
  
  # First, feed in standard parms
  paras <- currvac_parms
  # set seasonality
  set_mat <- seas_matrix(mu1, mu2, mu3, 
                         mu4, mu5, mu6)
  ## make sure mu is defined globally so that it actually passes into the seir function
  beta_FLU <<- function(t){
    seas_function(t, matrix = set_mat)
  }
  
  # print("Try: ------------------")
  # print(c(mu_janfeb, mu_marapr, mu_mayjun,
  #                        mu_julaug, mu_septoct, mu_novdec))
  # print("-----------------------")
  
  # set initial conditions
  #pr ## prop recovered
  #pinf ## prop currently infected of non-recovered individuals
  
  run_init <- init[c("S_C", "S_OC", "S_A", "S_S",
                     "E_C_FLU_vax", "I_C_FLU_vax", "H_C_FLU_vax", "D_C_FLU_vax",
                     "E_C_FLU", "I_C_FLU", "H_C_FLU", "D_C_FLU", "R_C_FLU",
                     
                     "E_OC_FLU_vax", "I_OC_FLU_vax", "H_OC_FLU_vax", "D_OC_FLU_vax",
                     "E_OC_FLU", "I_OC_FLU", "H_OC_FLU", "D_OC_FLU", "R_OC_FLU",
                     
                     "E_A_FLU_vax", "I_A_FLU_vax", "H_A_FLU_vax", "D_A_FLU_vax",
                     "E_A_FLU", "I_A_FLU", "H_A_FLU", "D_A_FLU", "R_A_FLU",
                     
                     "E_S_FLU_vax", "I_S_FLU_vax", "H_S_FLU_vax", "D_S_FLU_vax",
                     "E_S_FLU", "I_S_FLU", "H_S_FLU", "D_S_FLU", "R_S_FLU")]
  
  # Set times
  times <- 0:365
  
  #vec_print <- c(mu1, mu2, mu3, 
  #               mu4, mu5, mu_novdec,
  #              alpha,
  #            sig_dist)
  
  #print("------- PARAS --------")
  #print(vec_print)
  
  # Run the model 
  out_calib1 <- ode(y=run_init, func = seir_FLU, times=times, parms = paras, method = "rk4") %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(time, 
           H_C_FLU_vax, H_C_FLU, 
           H_OC_FLU_vax, H_OC_FLU,
           H_A_FLU_vax, H_A_FLU_vax,
           H_S_FLU_vax, H_S_FLU
    ) %>%
    group_by(time) %>%
    reframe(
      model_hosps = H_C_FLU_vax + H_C_FLU + H_OC_FLU_vax + H_OC_FLU + H_A_FLU_vax + H_A_FLU_vax + H_S_FLU_vax + H_S_FLU
    ) %>%
    arrange(time) %>%
    mutate(model_hosps = model_hosps - lag(model_hosps, 7)) %>% ## weekly
    ungroup() %>%
    rename(day = time) %>%
    inner_join(flu_data, by = c("day")) %>%
    # Normalize the data. Here, I'm using the maximum in the observed data for admissions
    arrange(day) %>%
    mutate(max_normal = max(observed_hosps, na.rm = T),
           observed_hosps = observed_hosps/max_normal,
           model_hosps = model_hosps/max_normal) %>%
    filter(!is.na(observed_hosps), !is.na(model_hosps)) %>%
    ungroup()
  
  # print(out_calib1)
  
  #print("Worked to here")
  
  # Now get the negative log likelihood. Note we are concurrently fitting the sd
  ll <- -sum(dnorm(x=out_calib1$model_hosps,mean=out_calib1$observed_hosps,sd=sig_dist,log=TRUE))
  
  # This code ensures that if the output of the log likelihood is NA, the MLE won't stop. Instead it will return
  # an extremely large, positive value of the negative log likelihood and keep iterating.
  ll_final = if_else(is.na(ll), 10^6, ll)
  
  # print(ll_final)
  
  return(ll_final)
}