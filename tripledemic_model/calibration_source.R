# Functions for use for model calibration

#' Seasonal forcing curve from a 6-parameter cyclic cubic spline
#' Returns cubic spline matrix for 6 seasonality parameters
#' 
#' @param mu_janfeb mu value for january - february
#' @param mu_marapr mu value for march - april
#' @param mu_mayjun mu value for may - june
#' @param mu_julaug mu value for july - august
#' @param mu_septoct mu value for september - october
#' @param mu_novdec mu value for november to december 
#' @return Dataframe with 365 rows and 2 columns: 
#' \describe{
#'   \item{day}{Day of year, indexed 0-364 (day 365 is recoded as 0).}
#'   \item{Value}{The seasonal curve value for that day: the mean of the six
#'   parameter-weighted spline basis functions.}
#' }
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


#' Get current seasonality parameter 
#' 
#' @param t timestep
#' @param matrix seasonality matrix, as given from seasonality_matrix() function
#' @return mu value at time t
seas_function <- function(t, matrix){
  matrix <- matrix %>%
    filter(day == floor(t) %% 365)
  mu <- matrix$Value[1]
  return(mu)
}


#' Negative log-likelihood for calibrating COVID-only SEIR model to ICU admission data 
#' Saves CSV for estimates for parameters and returns log-likelihood score 
#' 
#' @param mu1 mu value for january - february
#' @param mu2 mu value for march - april
#' @param mu3 mu value for may - june
#' @param mu4 mu value for july - august
#' @param mu5 mu value for september - october
#' @param mu6 mu value for november to december 
#' @param sig_dist standard deviation of the normal observation model top be fitted jointly with mu parms 
#' @return Negative log-likelihood estimate. Lower is better. 
cov_loglik <- function(mu1, mu2, mu3, mu4, mu5, mu6, sig_dist) {
  
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
  
  pr <- 0.04
  pexp <- 0.0001
  pinf <- 0.0001
  run_init <- init
  
  ## Set up initial conditions 
  run_init["S_C"] <- (1-pinf)*(1-pr)*(1-pexp)*init["S_C"]
  run_init["E_C_COV"] <- (1-paras["vaccC_COV"])*pexp*(1-pinf)*(1-pr)*init["S_C"]
  run_init["I_C_COV"] <- (1-paras["vaccC_COV"])*(1-paras['probHC_COV'])*pinf*((1-pr)*init["S_C"])
  run_init["H_C_COV"] <- (1-paras["vaccC_COV"])*(paras['probHC_COV'])*pinf*((1-pr)*init["S_C"])
  run_init["E_C_COV_vax"] <- (paras["vaccC_COV"])*pexp*(1-pinf)*(1-pr)*init["S_C"]
  run_init["I_C_COV_vax"] <- (paras["vaccC_COV"])*(1-paras['probHC_COV'])*pinf*((1-pr)*init["S_C"])
  run_init["H_C_COV_vax"] <- (paras["vaccC_COV"])*(paras['probHC_COV'])*pinf*((1-pr)*init["S_C"])
  
  run_init["R_C_COV"] <- pr*init["S_C"]
  
  run_init["S_OC"] <- (1-pinf)*((1-pr)*(1-pexp)*init["S_OC"])
  run_init["E_OC_COV"] <- (1-paras["vaccOC_COV"])*pexp*(1-pinf)*(1-pr)*init["S_OC"]
  run_init["I_OC_COV"] <- (1-paras["vaccOC_COV"])*(1-paras['probHOC_COV'])*pinf*((1-pr)*init["S_OC"])
  run_init["H_OC_COV"] <- (1-paras["vaccOC_COV"])*(paras['probHOC_COV'])*pinf*((1-pr)*init["S_OC"])
  run_init["E_OC_COV_vax"] <- (paras["vaccOC_COV"])*pexp*(1-pinf)*(1-pr)*init["S_OC"]
  run_init["I_OC_COV_vax"] <- (paras["vaccOC_COV"])*(1-paras['probHOC_COV'])*pinf*((1-pr)*init["S_OC"])
  run_init["H_OC_COV_vax"] <- (paras["vaccOC_COV"])*(paras['probHOC_COV'])*pinf*((1-pr)*init["S_OC"])
  
  run_init["R_OC_COV"] <- pr*init["S_OC"]
  
  run_init["S_A"] <- (1-pinf)*((1-pr)*(1-pexp)*init["S_A"])
  run_init["E_A_COV"] <- (1-paras["vaccA_COV"])*pexp*(1-pinf)*(1-pr)*init["S_A"]
  run_init["I_A_COV"] <- (1-paras["vaccA_COV"])*(1-paras['probHA_COV'])*pinf*((1-pr)*init["S_A"])
  run_init["H_A_COV"] <- (1-paras["vaccA_COV"])*(paras['probHA_COV'])*pinf*((1-pr)*init["S_A"])
  run_init["E_A_COV_vax"] <- (paras["vaccA_COV"])*pexp*(1-pinf)*(1-pr)*init["S_A"]
  run_init["I_A_COV_vax"] <- (paras["vaccA_COV"])*(1-paras['probHA_COV'])*pinf*((1-pr)*init["S_A"])
  run_init["H_A_COV_vax"] <- (paras["vaccA_COV"])*(paras['probHA_COV'])*pinf*((1-pr)*init["S_A"])
  
  run_init["R_A_COV"] <- pr*init["S_A"]
  
  run_init["S_S"] <- (1-pinf)*((1-pr)*(1-pexp)*init["S_S"])
  run_init["E_S_COV"] <- (1-paras["vaccS_COV"])*pexp*(1-pinf)*(1-pr)*init["S_S"]
  run_init["I_S_COV"] <- (1-paras["vaccS_COV"])*(1-paras['probHS_COV'])*pinf*((1-pr)*init["S_S"])
  run_init["H_S_COV"] <- (1-paras["vaccS_COV"])*(paras['probHS_COV'])*pinf*((1-pr)*init["S_S"])
  run_init["E_S_COV_vax"] <- (paras["vaccS_COV"])*pexp*(1-pinf)*(1-pr)*init["S_S"]
  run_init["I_S_COV_vax"] <- (paras["vaccS_COV"])*(1-paras['probHS_COV'])*pinf*((1-pr)*init["S_S"])
  run_init["H_S_COV_vax"] <- (paras["vaccS_COV"])*(paras['probHS_COV'])*pinf*((1-pr)*init["S_S"])
  
  run_init["R_S_COV"] <- pr*init["S_S"]
  
  
  run_init <- run_init[c("S_C", "S_OC", "S_A", "S_S",
                     "E_C_COV_vax", "I_C_COV_vax", "H_C_COV_vax", "D_C_COV_vax",
                     "E_C_COV", "I_C_COV", "H_C_COV", "D_C_COV", "R_C_COV",
                     
                     "E_OC_COV_vax", "I_OC_COV_vax", "H_OC_COV_vax", "D_OC_COV_vax",
                     "E_OC_COV", "I_OC_COV", "H_OC_COV", "D_OC_COV", "R_OC_COV",
                     
                     "E_A_COV_vax", "I_A_COV_vax", "H_A_COV_vax", "D_A_COV_vax",
                     "E_A_COV", "I_A_COV", "H_A_COV", "D_A_COV", "R_A_COV",
                     
                     "E_S_COV_vax", "I_S_COV_vax", "H_S_COV_vax", "D_S_COV_vax",
                     "E_S_COV", "I_S_COV", "H_S_COV", "D_S_COV", "R_S_COV")]
  
  # print(run_init)
  
  # Set times
  times <- 0:max(cov_data$day)
  
  #vec_print <- c(mu1, mu2, mu3, 
  #               mu4, mu5, mu_novdec,
  #              alpha,
  #            sig_dist)
  
  #print("------- PARAS --------")
  #print(vec_print)
  
  # Run the model 
  out_calib1 <- ode(y=run_init, func = seir_COV, times=times, parms = paras, method = "euler") %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(time, 
           H_C_COV_vax, H_C_COV, 
           H_OC_COV_vax, H_OC_COV,
           H_A_COV_vax, H_A_COV,
           H_S_COV_vax, H_S_COV
    ) %>%
    group_by(time) %>%
    reframe(
      model_hosps = H_C_COV_vax + H_C_COV + H_OC_COV_vax + H_OC_COV + H_A_COV + H_A_COV_vax + H_S_COV_vax + H_S_COV
    ) %>%
    arrange(time) %>%
    filter(time %in% cov_data$day) %>% 
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

#' Negative log-likelihood for calibrating RSV-only SEIR model to ICU admission data 
#' Saves CSV for estimates for parameters and returns log-likelihood score 
#' 
#' @param mu1 mu value for january - february
#' @param mu2 mu value for march - april
#' @param mu3 mu value for may - june
#' @param mu4 mu value for july - august
#' @param mu5 mu value for september - october
#' @param mu6 mu value for november to december 
#' @param sig_dist standard deviation of the normal observation model top be fitted jointly with mu parms 
#' @return Negative log-likelihood estimate. Lower is better. 
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

#' Negative log-likelihood for calibrating FLU-only SEIR model to ICU admission data 
#' Saves CSV for estimates for parameters and returns log-likelihood score 
#' 
#' @param mu1 mu value for january - february
#' @param mu2 mu value for march - april
#' @param mu3 mu value for may - june
#' @param mu4 mu value for july - august
#' @param mu5 mu value for september - october
#' @param mu6 mu value for november to december 
#' @param sig_dist standard deviation of the normal observation model top be fitted jointly with mu parms 
#' @return Negative log-likelihood estimate. Lower is better. 
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
  
  print("Try: ------------------")
  print(c(mu1, mu2, mu3,
          mu4, mu5, mu6))
  print("-----------------------")
  
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

