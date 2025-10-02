# Source functions for SEIR model

library(deSolve)
library(tidyverse)


# List of ages & diseases in Model
ages <- c("C", "OC", "A", "S")
diseases <- c("RSV", "COV", "FLU")

#' Returns a vector of totals by group of (age, disease) 
#' 
#' @param y vector of current compartment counts 
#' @return vector of totals in each group by (age, disease) like RSV_C: 2
make_group_totals <- function(y) {
  list2env(as.list(y), envir = environment())
  return_vec <- c()
  for (age in ages){
    for (disease in diseases){
      names <- c(
        paste0(c("E","I","H"), "_", age, "_", disease, "_vax"),
        paste0(c("E","I","H","R"), "_", age, "_", disease)
      )
      curr_name <- paste0(disease,"_", age)
      # print(curr_name)
      curr <- mget(names, inherits=TRUE)
      #print(curr)
      curr_total <- sum(unlist(curr))
      return_vec <- c(return_vec, setNames(curr_total,curr_name))
    }
  }
  return(return_vec)
}

#' Returns sum of infected individuals in by each age & disease
#' 
#' @param y vector of current compartment counts
#' @param si_RSV scale infectiousness for RSV
#' @param si_COV scale infectiousness for COVID
#' @param si_FLU scale infectiousness for FLU
#' @return vector of total infected individuals bby (age, disease) with name sumI_age_disease 
get_infected_sums <- function(y, si_RSV, si_COV, si_FLU){
  list2env(as.list(y), envir = environment())
  si_vals <- c("RSV" = si_RSV, "COV" = si_COV, "FLU" = si_FLU)
  # print(si_vals)
  return_vec <- c()
  for (age in ages){
    for (disease in diseases){
      names_no_vax <- c(paste0(c("I","H"), "_", age, "_", disease))
      curr_no_vax <- mget(names_no_vax, inherits=TRUE)
      names_vax <- c(paste0(c("I","H"), "_", age, "_", disease, "_vax"))
      curr_vax <- mget(names_vax, inherits=TRUE)
      curr_name <- paste0("sumI", age, "_", disease)
      curr_sum <- sum(unlist(curr_no_vax)) + si_vals[disease] * sum(unlist(curr_vax))
      return_vec <- c(return_vec, setNames(curr_sum, curr_name))
    }
  }
  return(return_vec)
}

#' Returns total population for each age group 
#' 
#' @param y vector of current compartment counts 
#' @param group_totals vector of group totals by disease and age like RSV_C = 2
#' @return vector of total population for each age group in format : "C" = popC, "OC" = popOC, "A" = popA, "S" = popS
get_pop <- function(y, group_totals){
  list2env(as.list(y), envir=environment())
  list2env(as.list(group_totals), envir=environment())
  popC <- S_C + sum(RSV_C) + sum(COV_C) + sum(FLU_C)
  popOC <- S_OC + sum(RSV_OC) + sum(COV_OC) + sum(FLU_OC)
  popA <- S_A + sum(RSV_A) + sum(COV_A) + sum(FLU_A)
  popS <- S_S + sum(RSV_S) + sum(COV_S) + sum(FLU_S)
  return(c("C" = popC, "OC" = popOC, "A" = popA, "S" = popS))
}

#' Returns lambda values for each age group and disease for use in Tripledemic SEIR
#' 
#' @param parms disease parameters for SIR model
#' @param y vector of current compartment counts 
#' @param cmat contact matrix
#' @return vector of lambdas for each age & disease group in format lambda_C_RSV etc
get_lambdas <- function(parms, y, cmat){
  # list2env(as.list(parms), envir=environment())
  group_totals <- make_group_totals(y)
  infected_sums <- get_infected_sums(y, unname(parms["si_RSV"]), unname(parms["si_COV"]), unname(parms["si_FLU"]))
  age_pops <- get_pop(y, group_totals)
  lambda_vec <- c()
  for (age in ages) {
    curr_age <- 1
    for (disease in diseases) {
      lambda_name <- paste0("lambda", age, "_", disease)
      beta_name <- paste0("beta", "_", disease)
      
      # make contact sum
      contact_sum <- 0
      i <- 1
      for (age2 in ages){
        c <- cmat[curr_age, i]
        I_name <- paste0("sumI", age2, "_", disease)
        curr_sum <- c * infected_sums[I_name] / age_pops[age2]
        contact_sum <- contact_sum + curr_sum
        i <- i + 1
      }
      curr_lambda <- parms[beta_name] * contact_sum
      lambda_vec <- c(lambda_vec, setNames(curr_lambda, lambda_name))
      
    }
    curr_age <- curr_age + 1
  }
  return(lambda_vec)
}

#' Returns overall total number of people current in hospitalized compartment
#' 
#' @param y vector of current compartment counts
#' @return Integer of total people currently hospitalized 
get_H_total <- function(y){
  list2env(as.list(y), envir = environment())
  names <-c()
  for (age in ages){
    for (disease in diseases){
      names <- c(names,
        paste0(c("H"), "_", age, "_", disease, "_vax"),
        paste0(c("H"), "_", age, "_", disease)
      )
    }}
  all_H <- mget(names, inherits=TRUE)
  H_total <- sum(unlist(all_H))
  return(H_total)
}

#' Returns modification for beta for seasonality 
#' 
#' NOT FINISHED
#' @param t current timestep
#' @param disease disease beta that needs modifying
#' @return A number to modify/scale current beta value 
delta_modify <- function(t, disease){
  return(0.1*sin(2*pi*(t-365/2)/365))
}