source("seir_source.R")

#### Tripledemic SEIR model ####

seir2 <- function(t, y, pars){
  # For COVID, Flu, & RSV
  # S -> E -> I -> H -> S or D
  #             -> D
  
  # We have 4 distinct age groups abbreviated as follows:
  # C - Children 0-5
  # OC - Older Children 6-18
  # A - Adults 18-65
  # S - Seniors 65+
  
  diseases <- c("RSV", "COV", "FLU")
  ages <- c("C", "OC", "A", "S")
  # Parameters
  ## General
  # H_capacity <- pars['H_capacity'] # hospital constraint
  
  ## Move all start vars to local environment
  # list2env(as.list(pars), envir = environment())
  # list2env(as.list(y), envir = environment())
  
  # State variables:
  # 4 groups: Children, Older Children, Adults, Seniors
  # 3 diseases: RSV, COVID, Flu
  # For each disease: Vax & Unvax
  
  # Set lambdas using get_lambdas() function for consistency
  lambda_vec <- get_lambdas(parms, y, cmat)
  
  # set up each S
  s_vec <- c()
  for (age in ages){
    curr_S <- y[paste0("S_", age)]
    var_name <- paste0("dS_", age)
    delta_term <- 0
    sum_lambda <- 0
    for (disease in diseases){
      sum_lambda <- sum_lambda + lambda_vec[paste0("lambda", age, "_", disease)]
      delta_term <- delta_term + parms[paste0("omega_", disease)]*y[paste0("R_", age, "_", disease)]
    }
    delta_term <- delta_term - (sum_lambda)*curr_S
    s_vec <- c(s_vec, setNames(delta_term, var_name))
  }
  
  # set each flow for each disease
  diff_eq_vec <- c()
  for (disease in diseases){
    for (age in ages){
      # make sure each flow is consistent for each group
      # dE
      diff_eq_vec[paste0("dE_", age, "_", disease, "_", vax)] <- 0
      
    }
  }
  
  dE_C_RSV_vax <- (lambdaC_RSV * vaccC_RSV * ve_RSV)*S_C - epsilon_RSV*E_C_RSV_vax +
    (lambdaC_RSV * vaccC_RSV * ve_RSV)*R_C_COV +
    (lambdaC_RSV * vaccC_RSV * ve_RSV)*R_C_FLU 
  dI_C_RSV_vax <- epsilon_RSV*E_C_RSV_vax - gammaI_RSV*I_C_RSV_vax
  dH_C_RSV_vax <- probHC_RSV_vax*gammaI_RSV*I_C_RSV_vax - gammaH_RSV*H_C_RSV_vax
  dD_C_RSV_vax <- alphaC_RSV*gammaH_RSV*H_C_RSV_vax
  
  dE_C_RSV <- (lambdaC_RSV * (1-vaccC_RSV))*S_C - epsilon_RSV*E_C_RSV + 
    (lambdaC_RSV * (1-vaccC_RSV))*R_C_COV +
    (lambdaC_RSV * (1-vaccC_RSV))*R_C_FLU 
  dI_C_RSV <- epsilon_RSV*E_C_RSV - gammaI_RSV*I_C_RSV
  dH_C_RSV <- probHC_RSV*gammaI_RSV*I_C_RSV - gammaH_RSV*H_C_RSV
  dD_C_RSV <- alphaC_RSV*gammaH_RSV*H_C_RSV
  
  dR_C_RSV <- (1-probHC_RSV)*gammaI_RSV*I_C_RSV + (1-alphaC_RSV)*gammaH_RSV*H_C_RSV - omega_RSV*R_C_RSV -
    (lambdaC_COV * vaccC_COV * ve_COV)*R_C_RSV -
    (lambdaC_COV * (1 - vaccC_COV)) * R_C_RSV -
    (lambdaC_FLU * vaccC_FLU * ve_FLU) * R_C_RSV -
    (lambdaC_FLU * (1 - vaccC_FLU)) * R_C_RSV
 
  
 
  return(lambda_vec)
}