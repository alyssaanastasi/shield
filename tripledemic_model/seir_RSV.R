seir_RSV <- function(t, y, pars) {
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
  
  
  sumIC_RSV <- I_C_RSV + si_RSV*I_C_RSV_vax + H_C_RSV + si_RSV*H_C_RSV_vax
  popC <- S_C + sum(RSV_C)
  sumIOC_RSV <- I_OC_RSV + si_RSV*I_OC_RSV_vax + H_OC_RSV + si_RSV*H_OC_RSV_vax
  popOC <- S_OC + sum(RSV_OC)
  sumIA_RSV <- I_A_RSV + si_RSV*I_A_RSV_vax + H_A_RSV + si_RSV*H_A_RSV_vax
  popA <- S_A + sum(RSV_A)
  sumIS_RSV <- I_S_RSV + si_RSV*I_S_RSV_vax + H_S_RSV + si_RSV*H_S_RSV_vax
  popS <- S_S + sum(RSV_S)
  
  lambdaC_RSV <- beta_RSV*(cCC*sumIC_RSV/popC + cCOC*sumIOC_RSV/popOC + cCA*sumIA_RSV/popA + cCS*sumIS_RSV/popS)
  lambdaOC_RSV <- beta_RSV*(cCOC*sumIC_RSV/popC + cOCOC*sumIOC_RSV/popOC + cOCA*sumIA_RSV/popA + cOCS*sumIS_RSV/popS)
  # print(lambdaOC_RSV)
  lambdaA_RSV <- beta_RSV*(cCA*sumIC_RSV/popC + cOCA*sumIOC_RSV/popOC + cAA*sumIA_RSV/popA + cAS*sumIS_RSV/popS)
  lambdaS_RSV <- beta_RSV*(cCS*sumIC_RSV/popC + cOCS*sumIOC_RSV/popOC + cAS*sumIA_RSV/popA + cSS*sumIS_RSV/popS)
  
  # Updates
  dS_C <- -(lambdaC_RSV)*S_C + (1-probHC_RSV_vax)*gammaI_RSV*I_C_RSV_vax + (1-alphaC_RSV)*gammaH_RSV*H_C_RSV_vax +
            (1-probHC_RSV)*gammaI_RSV*I_C_RSV + (1-alphaC_RSV)*gammaH_RSV*H_C_RSV 
  dS_OC <- -(lambdaOC_RSV)*S_OC + (1-probHOC_RSV_vax)*gammaI_RSV*I_OC_RSV_vax + (1-alphaOC_RSV)*gammaH_RSV*H_OC_RSV_vax +
            (1-probHOC_RSV)*gammaI_RSV*I_OC_RSV + (1-alphaOC_RSV)*gammaH_RSV*H_OC_RSV 
  dS_A <- -(lambdaA_RSV)*S_A + (1-probHA_RSV_vax)*gammaI_RSV*I_A_RSV_vax + (1-alphaA_RSV)*gammaH_RSV*H_A_RSV_vax +
            (1-probHA_RSV)*gammaI_RSV*I_A_RSV + (1-alphaA_RSV)*gammaH_RSV*H_A_RSV 
  dS_S <- -(lambdaS_RSV)*S_S + (1-probHS_RSV_vax)*gammaI_RSV*I_S_RSV_vax + (1-alphaS_RSV)*gammaH_RSV*H_S_RSV_vax +
            (1-probHS_RSV)*gammaI_RSV*I_S_RSV + (1-alphaS_RSV)*gammaH_RSV*H_S_RSV 
  
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
  
  list(c(
    dS_C, dS_OC, dS_A, dS_S,
    
    dE_C_RSV_vax, dI_C_RSV_vax, dH_C_RSV_vax, dD_C_RSV_vax,
    dE_C_RSV, dI_C_RSV, dH_C_RSV, dD_C_RSV,
    
    dE_OC_RSV_vax, dI_OC_RSV_vax, dH_OC_RSV_vax, dD_OC_RSV_vax,
    dE_OC_RSV, dI_OC_RSV, dH_OC_RSV, dD_OC_RSV,
    
    dE_A_RSV_vax, dI_A_RSV_vax, dH_A_RSV_vax, dD_A_RSV_vax,
    dE_A_RSV, dI_A_RSV, dH_A_RSV, dD_A_RSV,
    
    dE_S_RSV_vax, dI_S_RSV_vax, dH_S_RSV_vax, dD_S_RSV_vax,
    dE_S_RSV, dI_S_RSV, dH_S_RSV, dD_S_RSV))
  
}