### Population-level COVID SEIR Model

#' COVID SEIR model for use in ODE function in deSolve package 
#' Returns list of gradients for each compartment at current timestep t
#' 
#' @param t current timestep
#' @param y vector of current compartment counts 
#' @param pars parameters of seir model (sourced from seir_source.R) 
#' @return list of gradients for each compartment 
seir_COV <- function(t, y, pars) {
  
  # Bring all params into current function environment 
  list2env(as.list(y), envir=environment())
  list2env(as.list(pars), envir=environment())
  
  
  # Total exposed or infected population
  COV_all = c(E_COV_vax, I_COV_vax, H_COV_vax,
              E_COV, I_COV, H_COV, R_COV)
  
  # Infectious individuals (vaccinated are scaled by si_COV)
  sumI_COV <- I_COV + si_COV*I_COV_vax + H_COV + si_COV*H_COV_vax
  pop <- S + sum(COV_all)
  
  # Force of infection (single homogeneous population, so no contact matrix)
  lambda_COV <- beta_COV*sumI_COV/pop
  
  prop_can_be_hospitalized <- 1
  
  # Updates
  dS <- -((lambda_COV * ((vacc_COV * ve_COV) + (1-vacc_COV))))*S + omega_COV*R_COV
  
  ## COVID
  ### Vaccinated
  dE_COV_vax <- (lambda_COV * vacc_COV * ve_COV)*S - epsilon_COV*E_COV_vax 
  dI_COV_vax <- epsilon_COV*E_COV_vax - gammaI_COV*I_COV_vax
  dH_COV_vax <- -gammaH_COV*H_COV_vax + prop_can_be_hospitalized*probH_COV_vax*gammaI_COV*I_COV_vax
  dD_COV_vax <- alpha_COV*gammaH_COV*H_COV_vax + (1-prop_can_be_hospitalized)*probH_COV_vax*gammaI_COV*I_COV_vax
  
  ### Unvaccinated
  dE_COV <- (lambda_COV * (1-vacc_COV))*S - epsilon_COV*E_COV  
  dI_COV <- epsilon_COV*E_COV - gammaI_COV*I_COV
  dH_COV <- -gammaH_COV*H_COV + prop_can_be_hospitalized*probH_COV*gammaI_COV*I_COV
  dD_COV <- alpha_COV*gammaH_COV*H_COV + (1-prop_can_be_hospitalized)*probH_COV*gammaI_COV*I_COV
  
  ### Recovered
  dR_COV <- (1-probH_COV)*gammaI_COV*I_COV + (1-alpha_COV)*gammaH_COV*H_COV +
    (1-probH_COV_vax)*gammaI_COV*I_COV_vax + (1-alpha_COV)*gammaH_COV*H_COV_vax - omega_COV*R_COV 
  
  list(c(
    dS,
    
    dE_COV_vax, dI_COV_vax, dH_COV_vax, dD_COV_vax,
    dE_COV, dI_COV, dH_COV, dD_COV, dR_COV))
  
}
