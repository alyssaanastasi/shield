library(deSolve)
library(tidyverse)
source("seir_source.R")

#### Tripledemic SEIR model ####

#' Tripledemic SEIR model for use in ODE function in deSolve package 
#' Returns list of gradients for each compartment at current timestep t
#' 
#' @param t current timestep
#' @param y vector of current compartment counts 
#' @param pars parameters of tripledemic model 
#' @return list of gradients for each compartment 
seir <- function(t, y, pars){
  # For COVID, Flu, & RSV
  # S -> E -> I -> H -> R or D
  #             -> D
  
  # We have 4 distinct age groups abbreviated as follows:
  # C - Children 0-5
  # OC - Older Children 6-18
  # A - Adults 18-65
  # S - Seniors 65+
  # Parameters
  ## General
  
  # Add all parameters & initial states to current function environment
  list2env(as.list(y), envir=environment())
  list2env(as.list(pars), envir=environment())
  
  # force of infection: 
  # Lambda = x to y population contact * mu * prop of y infected
  list2env(as.list(get_lambdas(pars, y, cmat)), envir=environment()) #get_lambdas is function used to compute lambda for each age & disease
  
  # Check Hospitalization Condition
  # Set H_cap parameters: 
  ### H_cap_h is the parameter for hospitalizations. 1 if hospitals have not reached capacity, 0 if they have not
  ### H_cap_d is the parameter for deaths. 1 if hospitals have reached capacity (and people die instead), 0 if they have not 
  H_cap_h <- 1
  H_cap_d <- 0 
  # H_cap_reached <- FALSE #Debugging Parm
  if (get_H_total(y) > H_cap){
  #   H_cap_reached <- TRUE
     H_cap_h <- 0
     H_cap_d <- 1
  }
  
  # print(paste0("At Timestep: ", t, " Total Hospitalizations: ", get_H_total(y)))
  # print(paste0("At Timestep: ", t, " Hospital Capacity Reached: ", H_cap_reached))
  
  # Updates
  dS_C <- -(lambdaC_RSV + lambdaC_COV + lambdaC_FLU)*S_C + omega_RSV*R_C_RSV + omega_COV*R_C_COV + omega_FLU*R_C_FLU
  dS_OC <- -(lambdaOC_RSV + lambdaOC_COV + lambdaOC_FLU)*S_OC + omega_RSV*R_OC_RSV + omega_COV*R_OC_COV + omega_FLU*R_OC_FLU
  dS_A <- -(lambdaA_RSV + lambdaA_COV + lambdaA_FLU)*S_A + omega_RSV*R_A_RSV + omega_COV*R_A_COV + omega_FLU*R_A_FLU
  dS_S <- -(lambdaS_RSV + lambdaS_COV + lambdaS_FLU)*S_S + omega_RSV*R_S_RSV + omega_COV*R_S_COV + omega_FLU*R_S_FLU
  
  ## RSV 
  ### Children Updates
  dE_C_RSV_vax <- (lambdaC_RSV * vaccC_RSV * ve_RSV)*S_C - epsilon_RSV*E_C_RSV_vax +
    (lambdaC_RSV * vaccC_RSV * ve_RSV)*R_C_COV +
    (lambdaC_RSV * vaccC_RSV * ve_RSV)*R_C_FLU 
  dI_C_RSV_vax <- epsilon_RSV*E_C_RSV_vax - gammaI_RSV*I_C_RSV_vax
  dH_C_RSV_vax <- -gammaH_RSV*H_C_RSV_vax + H_cap_h*probHC_RSV_vax*gammaI_RSV*I_C_RSV_vax
  dD_C_RSV_vax <- alphaC_RSV*gammaH_RSV*H_C_RSV_vax + H_cap_d*probHC_RSV_vax*gammaI_RSV*I_C_RSV_vax
  
  dE_C_RSV <- (lambdaC_RSV * (1-vaccC_RSV))*S_C - epsilon_RSV*E_C_RSV + 
    (lambdaC_RSV * (1-vaccC_RSV))*R_C_COV +
    (lambdaC_RSV * (1-vaccC_RSV))*R_C_FLU 
  dI_C_RSV <- epsilon_RSV*E_C_RSV - gammaI_RSV*I_C_RSV
  dH_C_RSV <- -gammaH_RSV*H_C_RSV + H_cap_h*probHC_RSV*gammaI_RSV*I_C_RSV
  dD_C_RSV <- alphaC_RSV*gammaH_RSV*H_C_RSV + H_cap_d*probHC_RSV*gammaI_RSV*I_C_RSV
  
  dR_C_RSV <- (1-probHC_RSV)*gammaI_RSV*I_C_RSV + (1-alphaC_RSV)*gammaH_RSV*H_C_RSV - omega_RSV*R_C_RSV -
    (lambdaC_COV * vaccC_COV * ve_COV)*R_C_RSV -
    (lambdaC_COV * (1 - vaccC_COV)) * R_C_RSV -
    (lambdaC_FLU * vaccC_FLU * ve_FLU) * R_C_RSV -
    (lambdaC_FLU * (1 - vaccC_FLU)) * R_C_RSV
  
  ### Older Children
  dE_OC_RSV_vax <- (lambdaOC_RSV * vaccOC_RSV * ve_RSV)*S_OC - epsilon_RSV*E_OC_RSV_vax +
    (lambdaOC_RSV * vaccOC_RSV * ve_RSV)*R_OC_COV +
    (lambdaOC_RSV * vaccOC_RSV * ve_RSV)*R_OC_FLU 
  dI_OC_RSV_vax <- epsilon_RSV*E_OC_RSV_vax - gammaI_RSV*I_OC_RSV_vax
  dH_OC_RSV_vax <- probHOC_RSV_vax*gammaI_RSV*I_OC_RSV_vax - gammaH_RSV*H_OC_RSV_vax
  dD_OC_RSV_vax <- alphaOC_RSV*gammaH_RSV*H_OC_RSV_vax
  
  dE_OC_RSV <- (lambdaOC_RSV * (1-vaccOC_RSV))*S_OC - epsilon_RSV*E_OC_RSV +
    (lambdaOC_RSV * (1-vaccOC_RSV))*R_OC_COV +
    (lambdaOC_RSV * (1-vaccOC_RSV))*R_OC_FLU 
  dI_OC_RSV <- epsilon_RSV*E_OC_RSV - gammaI_RSV*I_OC_RSV
  dH_OC_RSV <-  - gammaH_RSV*H_OC_RSV + H_cap_h*probHOC_RSV*gammaI_RSV*I_OC_RSV
  dD_OC_RSV <- alphaOC_RSV*gammaH_RSV*H_OC_RSV + H_cap_d*probHOC_RSV*gammaI_RSV*I_OC_RSV
  
  dR_OC_RSV <- (1-probHOC_RSV)*gammaI_RSV*I_OC_RSV + (1-alphaOC_RSV)*gammaH_RSV*H_OC_RSV - omega_RSV*R_OC_RSV -
    (lambdaOC_COV * vaccOC_COV * ve_COV)*R_OC_RSV -
    (lambdaOC_COV * (1 - vaccOC_COV)) * R_OC_RSV -
    (lambdaOC_FLU * vaccOC_FLU * ve_FLU) * R_OC_RSV -
    (lambdaOC_FLU * (1 - vaccOC_FLU)) * R_OC_RSV
  
  
  ### Adults
  dE_A_RSV_vax <- (lambdaA_RSV * vaccA_RSV * ve_RSV)*S_A - epsilon_RSV*E_A_RSV_vax +
    (lambdaA_RSV * vaccA_RSV * ve_RSV)*R_A_COV +
    (lambdaA_RSV * vaccA_RSV * ve_RSV)*R_A_FLU 
  dI_A_RSV_vax <- epsilon_RSV*E_A_RSV_vax - gammaI_RSV*I_A_RSV_vax
  dH_A_RSV_vax <- -gammaH_RSV*H_A_RSV_vax + H_cap_h*probHA_RSV_vax*gammaI_RSV*I_A_RSV_vax
  dD_A_RSV_vax <- alphaA_RSV*gammaH_RSV*H_A_RSV_vax + H_cap_d*probHA_RSV_vax*gammaI_RSV*I_A_RSV_vax
  
  dE_A_RSV <- (lambdaA_RSV * (1-vaccA_RSV))*S_A - epsilon_RSV*E_A_RSV +
    (lambdaA_RSV * (1-vaccA_RSV))*R_A_COV +
    (lambdaA_RSV * (1-vaccA_RSV))*R_A_FLU 
  dI_A_RSV <- epsilon_RSV*E_A_RSV - gammaI_RSV*I_A_RSV
  dH_A_RSV <-  -gammaH_RSV*H_A_RSV + H_cap_h*probHA_RSV*gammaI_RSV*I_A_RSV
  dD_A_RSV <- alphaA_RSV*gammaH_RSV*H_A_RSV + H_cap_d*probHA_RSV*gammaI_RSV*I_A_RSV
  
  dR_A_RSV <- (1-probHA_RSV)*gammaI_RSV*I_A_RSV + (1-alphaA_RSV)*gammaH_RSV*H_A_RSV - omega_RSV*R_A_RSV -
    (lambdaA_COV * vaccA_COV * ve_COV)*R_A_RSV -
    (lambdaA_COV * (1 - vaccA_COV)) * R_A_RSV -
    (lambdaA_FLU * vaccA_FLU * ve_FLU) * R_A_RSV -
    (lambdaA_FLU * (1 - vaccA_FLU)) * R_A_RSV
  
  ### Seniors
  dE_S_RSV_vax <- (lambdaS_RSV * vaccS_RSV * ve_RSV)*S_S - epsilon_RSV*E_S_RSV_vax +
    (lambdaS_RSV * vaccS_RSV * ve_RSV)*R_S_COV +
    (lambdaS_RSV * vaccS_RSV * ve_RSV)*R_S_FLU 
  dI_S_RSV_vax <- epsilon_RSV*E_S_RSV_vax - gammaI_RSV*I_S_RSV_vax
  dH_S_RSV_vax <-  -gammaH_RSV*H_S_RSV_vax + H_cap_h*probHS_RSV_vax*gammaI_RSV*I_S_RSV_vax
  dD_S_RSV_vax <- alphaS_RSV*gammaH_RSV*H_S_RSV_vax + H_cap_d*probHS_RSV_vax*gammaI_RSV*I_S_RSV_vax
  
  dE_S_RSV <- (lambdaS_RSV * (1-vaccS_RSV))*S_S - epsilon_RSV*E_S_RSV +
    (lambdaS_RSV * (1-vaccS_RSV))*R_S_COV +
    (lambdaS_RSV * (1-vaccS_RSV))*R_S_FLU 
  dI_S_RSV <- epsilon_RSV*E_S_RSV - gammaI_RSV*I_S_RSV
  dH_S_RSV <-  -gammaH_RSV*H_S_RSV + H_cap_h*probHS_RSV*gammaI_RSV*I_S_RSV
  dD_S_RSV <- alphaS_RSV*gammaH_RSV*H_S_RSV + H_cap_d*probHS_RSV*gammaI_RSV*I_S_RSV
  
  dR_S_RSV <- (1-probHS_RSV)*gammaI_RSV*I_S_RSV + (1-alphaS_RSV)*gammaH_RSV*H_S_RSV - omega_RSV*R_S_RSV -
    (lambdaS_COV * vaccS_COV * ve_COV)*R_S_RSV -
    (lambdaS_COV * (1 - vaccA_COV)) * R_S_RSV -
    (lambdaS_FLU * vaccS_FLU * ve_FLU) * R_S_RSV -
    (lambdaS_FLU * (1 - vaccS_FLU)) * R_S_RSV
  
  ## COVID
  ### Children Updates
  dE_C_COV_vax <- (lambdaC_COV * vaccC_COV * ve_COV)*S_C - epsilon_COV*E_C_COV_vax +
    (lambdaC_COV * vaccC_COV * ve_COV)*R_C_RSV +
    (lambdaC_COV * vaccC_COV * ve_COV)*R_C_FLU 
  dI_C_COV_vax <- epsilon_COV*E_C_COV_vax - gammaI_COV*I_C_COV_vax
  dH_C_COV_vax <-  -gammaH_COV*H_C_COV_vax + H_cap_h*probHC_COV_vax*gammaI_COV*I_C_COV_vax
  dD_C_COV_vax <- alphaC_COV*gammaH_COV*H_C_COV_vax + H_cap_d*probHC_COV_vax*gammaI_COV*I_C_COV_vax
  
  dE_C_COV <- (lambdaC_COV * (1-vaccC_COV))*S_C - epsilon_COV*E_C_COV +
    (lambdaC_COV * (1-vaccC_COV))*R_C_RSV +
    (lambdaC_COV * (1-vaccC_COV))*R_C_FLU 
  dI_C_COV <- epsilon_COV*E_C_COV - gammaI_COV*I_C_COV
  dH_C_COV <- -gammaH_COV*H_C_COV + H_cap_h*probHC_COV*gammaI_COV*I_C_COV
  dD_C_COV <- alphaC_COV*gammaH_COV*H_C_COV + H_cap_d*probHC_COV*gammaI_COV*I_C_COV
  
  dR_C_COV <- (1-probHC_COV)*gammaI_COV*I_C_COV + (1-alphaC_COV)*gammaH_COV*H_C_COV - omega_COV*R_C_COV -
    (lambdaC_RSV * vaccC_RSV * ve_RSV)*R_C_COV -
    (lambdaC_RSV * (1 - vaccC_RSV)) * R_C_COV -
    (lambdaC_FLU * vaccC_FLU * ve_FLU) * R_C_COV -
    (lambdaC_FLU * (1 - vaccC_FLU)) * R_C_COV
  
  ### Older Children
  dE_OC_COV_vax <- (lambdaOC_COV * vaccOC_COV * ve_COV)*S_OC - epsilon_COV*E_OC_COV_vax +
    (lambdaOC_COV * vaccOC_COV * ve_COV)*R_OC_RSV +
    (lambdaOC_COV * vaccOC_COV * ve_COV)*R_OC_FLU 
  dI_OC_COV_vax <- epsilon_COV*E_OC_COV_vax - gammaI_COV*I_OC_COV_vax
  dH_OC_COV_vax <- - gammaH_COV*H_OC_COV_vax + H_cap_h*probHOC_COV_vax*gammaI_COV*I_OC_COV_vax
  dD_OC_COV_vax <- alphaOC_COV*gammaH_COV*H_OC_COV_vax + H_cap_d*probHOC_COV_vax*gammaI_COV*I_OC_COV_vax
  
  
  dE_OC_COV <- (lambdaOC_COV * (1-vaccOC_COV))*S_OC - epsilon_COV*E_OC_COV +
    (lambdaOC_COV * (1-vaccOC_COV))*R_OC_RSV +
    (lambdaOC_COV * (1-vaccOC_COV))*R_OC_FLU 
  dI_OC_COV <- epsilon_COV*E_OC_COV - gammaI_COV*I_OC_COV
  dH_OC_COV <-  -gammaH_COV*H_OC_COV + H_cap_h*probHOC_COV*gammaI_COV*I_OC_COV
  dD_OC_COV <- alphaOC_COV*gammaH_COV*H_OC_COV + H_cap_d*probHOC_COV*gammaI_COV*I_OC_COV
  
  dR_OC_COV <- (1-probHOC_COV)*gammaI_COV*I_OC_COV + (1-alphaOC_COV)*gammaH_COV*H_OC_COV - omega_COV*R_OC_COV -
    (lambdaOC_RSV * vaccOC_RSV * ve_RSV)*R_OC_COV -
    (lambdaOC_RSV * (1 - vaccOC_RSV)) * R_OC_COV -
    (lambdaOC_FLU * vaccOC_FLU * ve_FLU) * R_OC_COV -
    (lambdaOC_FLU * (1 - vaccOC_FLU)) * R_OC_COV
  
  ### Adults
  dE_A_COV_vax <- (lambdaA_COV * vaccA_COV * ve_COV)*S_A - epsilon_COV*E_A_COV_vax +
    (lambdaA_COV * vaccA_COV * ve_COV)*R_A_RSV +
    (lambdaA_COV * vaccA_COV * ve_COV)*R_A_FLU 
  dI_A_COV_vax <- epsilon_COV*E_A_COV_vax - gammaI_COV*I_A_COV_vax
  dH_A_COV_vax <- -gammaH_COV*H_A_COV_vax + H_cap_h*probHA_COV_vax*gammaI_COV*I_A_COV_vax
  dD_A_COV_vax <- alphaA_COV*gammaH_COV*H_A_COV_vax + H_cap_d*probHA_COV_vax*gammaI_COV*I_A_COV_vax
  
  
  dE_A_COV <- (lambdaA_COV * (1-vaccA_COV))*S_A - epsilon_COV*E_A_COV +
    (lambdaA_COV * (1-vaccA_COV))*R_A_RSV +
    (lambdaA_COV * (1-vaccA_COV))*R_A_FLU 
  dI_A_COV <- epsilon_COV*E_A_COV - gammaI_COV*I_A_COV
  dH_A_COV <- -gammaH_COV*H_A_COV + H_cap_h*probHA_COV*gammaI_COV*I_A_COV
  dD_A_COV <- alphaA_COV*gammaH_COV*H_A_COV + H_cap_d*probHA_COV*gammaI_COV*I_A_COV
  
  dR_A_COV <- (1-probHA_COV)*gammaI_COV*I_A_COV + (1-alphaA_COV)*gammaH_COV*H_A_COV - omega_COV*R_A_COV -
    (lambdaA_RSV * vaccA_RSV * ve_RSV)*R_A_COV -
    (lambdaA_RSV * (1 - vaccA_RSV)) * R_A_COV -
    (lambdaA_FLU * vaccA_FLU * ve_FLU) * R_A_COV -
    (lambdaA_FLU * (1 - vaccA_FLU)) * R_A_COV
  
  ### Seniors
  dE_S_COV_vax <- (lambdaS_COV * vaccS_COV * ve_COV)*S_S - epsilon_COV*E_S_COV_vax +
    (lambdaS_COV * vaccS_COV * ve_COV)*R_S_RSV +
    (lambdaS_COV * vaccS_COV * ve_COV)*R_S_FLU 
  dI_S_COV_vax <- epsilon_COV*E_S_COV_vax - gammaI_COV*I_S_COV_vax
  dH_S_COV_vax <- -gammaH_COV*H_S_COV_vax + H_cap_h*probHS_COV_vax*gammaI_COV*I_S_COV_vax
  dD_S_COV_vax <- alphaS_COV*gammaH_COV*H_S_COV_vax + H_cap_d*probHS_COV_vax*gammaI_COV*I_S_COV_vax
  
  
  dE_S_COV <- (lambdaS_COV * (1-vaccS_COV))*S_S - epsilon_COV*E_S_COV +
    (lambdaS_COV * (1-vaccS_COV))*R_S_RSV +
    (lambdaS_COV * (1-vaccS_COV))*R_S_FLU 
  dI_S_COV <- epsilon_COV*E_S_COV - gammaI_COV*I_S_COV
  dH_S_COV <- -gammaH_COV*H_S_COV + H_cap_h*probHS_COV*gammaI_COV*I_S_COV
dD_S_COV <- alphaS_COV*gammaH_COV*H_S_COV + H_cap_d*probHS_COV*gammaI_COV*I_S_COV

  
  dR_S_COV <- (1-probHS_COV)*gammaI_COV*I_S_COV + (1-alphaS_COV)*gammaH_COV*H_S_COV - omega_COV*R_S_COV -
    (lambdaS_RSV * vaccS_RSV * ve_RSV)*R_S_COV -
    (lambdaS_RSV * (1 - vaccS_RSV)) * R_S_COV -
    (lambdaS_FLU * vaccS_FLU * ve_FLU) * R_S_COV -
    (lambdaS_FLU * (1 - vaccS_FLU)) * R_S_COV
  
  ## FLU
  ### Children Updates
  dE_C_FLU_vax <- (lambdaC_FLU * vaccC_FLU * ve_FLU)*S_C - epsilon_FLU*E_C_FLU_vax +
    (lambdaC_FLU * vaccC_FLU * ve_FLU)*R_C_RSV +
    (lambdaC_FLU * vaccC_FLU * ve_FLU)*R_C_COV 
  dI_C_FLU_vax <- epsilon_FLU*E_C_FLU_vax - gammaI_FLU*I_C_FLU_vax
  dH_C_FLU_vax <- -gammaH_FLU*H_C_FLU_vax + H_cap_h*probHC_FLU_vax*gammaI_FLU*I_C_FLU_vax
  dD_C_FLU_vax <- alphaC_FLU*gammaH_FLU*H_C_FLU_vax + H_cap_d*probHC_FLU_vax*gammaI_FLU*I_C_FLU_vax

  
  dE_C_FLU <- (lambdaC_FLU * (1-vaccC_FLU))*S_C - epsilon_FLU*E_C_FLU +
    (lambdaC_FLU * (1-vaccC_FLU))*R_C_RSV +
    (lambdaC_FLU * (1-vaccC_FLU))*R_C_COV 
  dI_C_FLU <- epsilon_FLU*E_C_FLU - gammaI_FLU*I_C_FLU
  dH_C_FLU <-  -gammaH_FLU*H_C_FLU + H_cap_h*probHC_FLU*gammaI_FLU*I_C_FLU
  dD_C_FLU <- alphaC_FLU*gammaH_FLU*H_C_FLU + H_cap_d*probHC_FLU*gammaI_FLU*I_C_FLU
  
  dR_C_FLU <- (1-probHC_FLU)*gammaI_FLU*I_C_FLU + (1-alphaC_FLU)*gammaH_FLU*H_C_FLU - omega_FLU*R_C_FLU -
    (lambdaC_RSV * vaccC_RSV * ve_RSV)*R_C_FLU -
    (lambdaC_RSV * (1 - vaccC_RSV)) * R_C_FLU -
    (lambdaC_COV * vaccC_COV * ve_COV) * R_C_FLU -
    (lambdaC_COV * (1 - vaccC_COV)) * R_C_FLU
  
  ### Older Children
  dE_OC_FLU_vax <- (lambdaOC_FLU * vaccOC_FLU * ve_FLU)*S_OC - epsilon_FLU*E_OC_FLU_vax +
    (lambdaOC_FLU * vaccOC_FLU * ve_FLU)*R_OC_RSV +
    (lambdaOC_FLU * vaccOC_FLU * ve_FLU)*R_OC_COV
  dI_OC_FLU_vax <- epsilon_FLU*E_OC_FLU_vax - gammaI_FLU*I_OC_FLU_vax 
  dH_OC_FLU_vax <- -gammaH_FLU*H_OC_FLU_vax + H_cap_h*probHOC_FLU_vax*gammaI_FLU*I_OC_FLU_vax
  dD_OC_FLU_vax <- alphaOC_FLU*gammaH_FLU*H_OC_FLU_vax + H_cap_d*probHOC_FLU_vax*gammaI_FLU*I_OC_FLU_vax
  
  
  dE_OC_FLU <- (lambdaOC_FLU * (1-vaccOC_FLU))*S_OC - epsilon_FLU*E_OC_FLU +
    (lambdaOC_FLU * (1-vaccOC_FLU))*R_OC_RSV +
    (lambdaOC_FLU * (1-vaccOC_FLU))*R_OC_COV 
  dI_OC_FLU <- epsilon_FLU*E_OC_FLU - gammaI_FLU*I_OC_FLU
  dH_OC_FLU <- -gammaH_FLU*H_OC_FLU + H_cap_h*probHOC_FLU*gammaI_FLU*I_OC_FLU
  dD_OC_FLU <- alphaOC_FLU*gammaH_FLU*H_OC_FLU + H_cap_d*probHOC_FLU*gammaI_FLU*I_OC_FLU
  
  
  dR_OC_FLU <- (1-probHOC_FLU)*gammaI_FLU*I_OC_FLU + (1-alphaOC_FLU)*gammaH_FLU*H_OC_FLU - omega_FLU*R_OC_FLU -
    (lambdaOC_RSV * vaccOC_RSV * ve_RSV)*R_OC_FLU -
    (lambdaOC_RSV * (1 - vaccOC_RSV)) * R_OC_FLU -
    (lambdaOC_COV * vaccOC_COV * ve_COV) * R_OC_FLU -
    (lambdaOC_COV * (1 - vaccOC_COV)) * R_OC_FLU
  
  ### Adults
  dE_A_FLU_vax <- (lambdaA_FLU * vaccA_FLU * ve_FLU)*S_A - epsilon_FLU*E_A_FLU_vax +
    (lambdaA_FLU * vaccA_FLU * ve_FLU)*R_A_RSV +
    (lambdaA_FLU * vaccA_FLU * ve_FLU)*R_A_COV
  dI_A_FLU_vax <- epsilon_FLU*E_A_FLU_vax - gammaI_FLU*I_A_FLU_vax
  dH_A_FLU_vax <- -gammaH_FLU*H_A_FLU_vax + H_cap_h*probHA_FLU_vax*gammaI_FLU*I_A_FLU_vax
  dD_A_FLU_vax <- alphaA_FLU*gammaH_FLU*H_A_FLU_vax + H_cap_d*probHA_FLU_vax*gammaI_FLU*I_A_FLU_vax
  
  
  
  dE_A_FLU <- (lambdaA_FLU * (1-vaccA_FLU))*S_A - epsilon_FLU*E_A_FLU +
    (lambdaA_FLU * (1-vaccA_FLU))*R_A_RSV +
    (lambdaA_FLU * (1-vaccA_FLU))*R_A_COV 
  dI_A_FLU <- epsilon_FLU*E_A_FLU - gammaI_FLU*I_A_FLU
  dH_A_FLU <- -gammaH_FLU*H_A_FLU + H_cap_h*probHA_FLU*gammaI_FLU*I_A_FLU
  dD_A_FLU <- alphaA_FLU*gammaH_FLU*H_A_FLU + H_cap_d*probHA_FLU*gammaI_FLU*I_A_FLU
  
  
  dR_A_FLU <- (1-probHA_FLU)*gammaI_FLU*I_A_FLU + (1-alphaA_FLU)*gammaH_FLU*H_A_FLU - omega_FLU*R_A_FLU -
    (lambdaA_RSV * vaccA_RSV * ve_RSV)*R_A_FLU -
    (lambdaA_RSV * (1 - vaccA_RSV)) * R_A_FLU -
    (lambdaA_COV * vaccA_COV * ve_COV) * R_A_FLU -
    (lambdaA_COV * (1 - vaccA_COV)) * R_A_FLU
  
  ### Seniors
  dE_S_FLU_vax <- (lambdaS_FLU * vaccS_FLU * ve_FLU)*S_S - epsilon_FLU*E_S_FLU_vax +
    (lambdaS_FLU * vaccS_FLU * ve_FLU)*R_S_RSV +
    (lambdaS_FLU * vaccS_FLU * ve_FLU)*R_S_COV
  dI_S_FLU_vax <- epsilon_FLU*E_S_FLU_vax - gammaI_FLU*I_S_FLU_vax
  dH_S_FLU_vax <- -gammaH_FLU*H_S_FLU_vax + H_cap_h*probHS_FLU_vax*gammaI_FLU*I_S_FLU_vax
  dD_S_FLU_vax <- alphaS_FLU*gammaH_FLU*H_S_FLU_vax + H_cap_d*probHS_FLU_vax*gammaI_FLU*I_S_FLU_vax
  
  
  dE_S_FLU <- (lambdaS_FLU * (1-vaccS_FLU))*S_S - epsilon_FLU*E_S_FLU +
    (lambdaS_FLU * (1-vaccS_FLU))*R_S_RSV +
    (lambdaS_FLU * (1-vaccS_FLU))*R_S_COV 
  dI_S_FLU <- epsilon_FLU*E_S_FLU - gammaI_FLU*I_S_FLU
  dH_S_FLU <- -gammaH_FLU*H_S_FLU + H_cap_h*probHS_FLU*gammaI_FLU*I_S_FLU
  dD_S_FLU <- alphaS_FLU*gammaH_FLU*H_S_FLU + H_cap_d*probHS_FLU*gammaI_FLU*I_S_FLU
  
  
  dR_S_FLU <- (1-probHS_FLU)*gammaI_FLU*I_S_FLU + (1-alphaS_FLU)*gammaH_FLU*H_S_FLU - omega_FLU*R_S_FLU -
    (lambdaS_RSV * vaccS_RSV * ve_RSV)*R_S_FLU -
    (lambdaS_RSV * (1 - vaccS_RSV)) * R_S_FLU -
    (lambdaS_COV * vaccS_COV * ve_COV) * R_S_FLU -
    (lambdaS_COV * (1 - vaccS_COV)) * R_S_FLU
  
  # Return list of gradients
  list(c(
    dS_C, dS_OC, dS_A, dS_S,
    
    dE_C_RSV_vax, dI_C_RSV_vax, dH_C_RSV_vax, dD_C_RSV_vax,
    dE_C_RSV, dI_C_RSV, dH_C_RSV, dD_C_RSV, dR_C_RSV,
    
    dE_OC_RSV_vax, dI_OC_RSV_vax, dH_OC_RSV_vax, dD_OC_RSV_vax,
    dE_OC_RSV, dI_OC_RSV, dH_OC_RSV, dD_OC_RSV,dR_OC_RSV,
    
    dE_A_RSV_vax, dI_A_RSV_vax, dH_A_RSV_vax, dD_A_RSV_vax,
    dE_A_RSV, dI_A_RSV, dH_A_RSV, dD_A_RSV, dR_A_RSV,
    
    dE_S_RSV_vax, dI_S_RSV_vax, dH_S_RSV_vax, dD_S_RSV_vax,
    dE_S_RSV, dI_S_RSV, dH_S_RSV, dD_S_RSV, dR_S_RSV,
    
    dE_C_COV_vax, dI_C_COV_vax, dH_C_COV_vax, dD_C_COV_vax,
    dE_C_COV, dI_C_COV, dH_C_COV, dD_C_COV, dR_C_COV,
    
    dE_OC_COV_vax, dI_OC_COV_vax, dH_OC_COV_vax, dD_OC_COV_vax,
    dE_OC_COV, dI_OC_COV, dH_OC_COV, dD_OC_COV, dR_OC_COV,
    
    dE_A_COV_vax, dI_A_COV_vax, dH_A_COV_vax, dD_A_COV_vax,
    dE_A_COV, dI_A_COV, dH_A_COV, dD_A_COV, dR_A_COV,
    
    dE_S_COV_vax, dI_S_COV_vax, dH_S_COV_vax, dD_S_COV_vax,
    dE_S_COV, dI_S_COV, dH_S_COV, dD_S_COV, dR_S_COV,
    
    dE_C_FLU_vax, dI_C_FLU_vax, dH_C_FLU_vax, dD_C_FLU_vax,
    dE_C_FLU, dI_C_FLU, dH_C_FLU, dD_C_FLU, dR_C_FLU,
    
    dE_OC_FLU_vax, dI_OC_FLU_vax, dH_OC_FLU_vax, dD_OC_FLU_vax,
    dE_OC_FLU, dI_OC_FLU, dH_OC_FLU, dD_OC_FLU, dR_OC_FLU,
    
    dE_A_FLU_vax, dI_A_FLU_vax, dH_A_FLU_vax, dD_A_FLU_vax,
    dE_A_FLU, dI_A_FLU, dH_A_FLU, dD_A_FLU, dR_A_FLU,
    
    dE_S_FLU_vax, dI_S_FLU_vax, dH_S_FLU_vax, dD_S_FLU_vax,
    dE_S_FLU, dI_S_FLU, dH_S_FLU, dD_S_FLU, dR_S_FLU
  ))
  
  
}