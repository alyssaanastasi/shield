seir_COV <- function(t, y, pars) {
  cCC <- pars['cCC'] # contact between children 
  cCOC <- pars['cCOC'] # contact between children and older children
  cCA <- pars['cCA'] # contact between children and adults
  cCS <- pars['cCS'] # contact between children and seniors
  cOCOC <- pars['cOCOC'] #contact between older children
  cOCA <- pars['cOCA'] #contact between older children and adults
  cOCS <- pars['cOCS'] #contact between older children and seniors
  cAA <- pars['cAA'] # contact between adults
  cAS <- pars['cAS']# contact between seniors and adults
  cSS <- pars['cSS'] # contact between seniors
  
  ## COV
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
  
  S_C <- y["S_C"]
  S_OC <- y["S_OC"]
  S_A <- y["S_A"]
  S_S <- y["S_S"]
  
  ## COV
  
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
  
  
  sumIC_COV <- I_C_COV + si_COV*I_C_COV_vax + H_C_COV + si_COV*H_C_COV_vax
  popC <- S_C + sum(COV_C)
  sumIOC_COV <- I_OC_COV + si_COV*I_OC_COV_vax + H_OC_COV + si_COV*H_OC_COV_vax
  popOC <- S_OC + sum(COV_OC)
  sumIA_COV <- I_A_COV + si_COV*I_A_COV_vax + H_A_COV + si_COV*H_A_COV_vax
  popA <- S_A + sum(COV_A)
  sumIS_COV <- I_S_COV + si_COV*I_S_COV_vax + H_S_COV + si_COV*H_S_COV_vax
  popS <- S_S + sum(COV_S)
  
  lambdaC_COV <- beta_COV*(cCC*sumIC_COV/popC + cCOC*sumIOC_COV/popOC + cCA*sumIA_COV/popA + cCS*sumIS_COV/popS)
  lambdaOC_COV <- beta_COV*(cCOC*sumIC_COV/popC + cOCOC*sumIOC_COV/popOC + cOCA*sumIA_COV/popA + cOCS*sumIS_COV/popS)
  # print(lambdaOC_COV)
  lambdaA_COV <- beta_COV*(cCA*sumIC_COV/popC + cOCA*sumIOC_COV/popOC + cAA*sumIA_COV/popA + cAS*sumIS_COV/popS)
  lambdaS_COV <- beta_COV*(cCS*sumIC_COV/popC + cOCS*sumIOC_COV/popOC + cAS*sumIA_COV/popA + cSS*sumIS_COV/popS)
  
  # Updates
  dS_C <- -(lambdaC_COV)*S_C + (1-probHC_COV_vax)*gammaI_COV*I_C_COV_vax + (1-alphaC_COV)*gammaH_COV*H_C_COV_vax +
    (1-probHC_COV)*gammaI_COV*I_C_COV + (1-alphaC_COV)*gammaH_COV*H_C_COV 
  dS_OC <- -(lambdaOC_COV)*S_OC + (1-probHOC_COV_vax)*gammaI_COV*I_OC_COV_vax + (1-alphaOC_COV)*gammaH_COV*H_OC_COV_vax +
    (1-probHOC_COV)*gammaI_COV*I_OC_COV + (1-alphaOC_COV)*gammaH_COV*H_OC_COV 
  dS_A <- -(lambdaA_COV)*S_A + (1-probHA_COV_vax)*gammaI_COV*I_A_COV_vax + (1-alphaA_COV)*gammaH_COV*H_A_COV_vax +
    (1-probHA_COV)*gammaI_COV*I_A_COV + (1-alphaA_COV)*gammaH_COV*H_A_COV 
  dS_S <- -(lambdaS_COV)*S_S + (1-probHS_COV_vax)*gammaI_COV*I_S_COV_vax + (1-alphaS_COV)*gammaH_COV*H_S_COV_vax +
    (1-probHS_COV)*gammaI_COV*I_S_COV + (1-alphaS_COV)*gammaH_COV*H_S_COV 
  
  ## COV 
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
  
  list(c(
    dS_C, dS_OC, dS_A, dS_S,
    
    dE_C_COV_vax, dI_C_COV_vax, dH_C_COV_vax, dD_C_COV_vax,
    dE_C_COV, dI_C_COV, dH_C_COV, dD_C_COV,
    
    dE_OC_COV_vax, dI_OC_COV_vax, dH_OC_COV_vax, dD_OC_COV_vax,
    dE_OC_COV, dI_OC_COV, dH_OC_COV, dD_OC_COV,
    
    dE_A_COV_vax, dI_A_COV_vax, dH_A_COV_vax, dD_A_COV_vax,
    dE_A_COV, dI_A_COV, dH_A_COV, dD_A_COV,
    
    dE_S_COV_vax, dI_S_COV_vax, dH_S_COV_vax, dD_S_COV_vax,
    dE_S_COV, dI_S_COV, dH_S_COV, dD_S_COV))
  
}