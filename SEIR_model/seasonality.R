### Seasonality Parameters

#### Seasonality ###

#### Seasonal function ####
seas_matrix <- function(mu_janmar, mu_aprjun, mu_julsep, mu_octdec){
  n <- 365
  x <- 0:(n-1)/(n-1); #normalizing sequence
  k<- 0:4/4 # number of parameters wanted & normalized 
  # below is a cubic spline 
  matrix <- cSplineDes(x, k, ord = 4, derivs=0) %>%
    as.data.frame() %>%
    mutate(day = 1:365) %>%
    rename(JanMar = V1, AprJun = V2, JulSep = V3, OctDec = V4) %>%
    pivot_longer(JanMar:OctDec, values_to = "Value", names_to = "Season") %>%
    mutate(spline_scalar = case_when(
      Season == "JanMar" ~ mu_janmar,
      Season == "AprJun" ~ mu_aprjun,
      Season == "JulSep" ~ mu_julsep,
      Season == "OctDec" ~ mu_octdec
    )) %>%
    mutate(Value = spline_scalar*Value) %>%
    group_by(day) %>% 
    summarise(Value = mean(Value)) %>%
    ungroup() %>%
    mutate(day = if_else(day == 365, 0, day))
  return(matrix)
}

seas_function <- function(t, matrix){
  matrix <- matrix %>%
    filter(day == round(t) %% 365)
  mu <- matrix$Value[1]
  return(mu)
}

#### loglik ####
loglik <- function(
    mu_janmar, mu_aprjun, mu_julsep, mu_octdec, # emulate 4 seasons
    sig_dist
){
  
  # set seasonality
  set_mat <- seas_matrix(mu_janmar, mu_aprjun, mu_julsep, mu_octdec)
  
  ## make sure mu is defined globally so that it actually passes into the seir function
  mu <<- function(t){
    seas_function(t, matrix = set_mat)
  }
  
  # Set times
  times <- 1:1461
  
  # vec_print <- c(mu_janfeb, mu_marapr, mu_mayjun, 
  #                mu_julaug, mu_septoct, mu_novdec,
  #                alpha,
  #                sig_dist)
  # 
  # print("------- PARAS --------")
  # print(vec_print)
  
  # Run the model 
  out_calib1 <- ode(y=init, func=seir, times=times, parms = covid_parms_full) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(time, 
           D_C1, D_C2,
           D_CA1, D_CA2,
           D_P1, D_P2,
           D_S1, D_S2
    ) %>%
    group_by(time) %>%
    reframe(
      `total_d` = D_C1 + D_C2 + D_CA1 + D_CA2 + D_P1 + D_P2 + D_S1 + D_S2
    ) %>%
    ungroup() %>%
    pivot_longer(-time) %>%
    # separate(name, into = c("target", "age_group"), sep = "_") %>%
    # group_by(target, age_group) %>%
    arrange(time) %>%
    mutate(value = value - lag(value, 7)) %>% ## weekly
    ungroup() %>%
    mutate(date = as.Date("01-01-2022", "%m-%d-%Y") + time) %>%
    select(-time) %>%
    right_join(covid_mortality, by = "date") %>%
    # Normalize the data. Here, I'm using the maximum in the observed data for each target and 
    # age combination. This step is very important. Without it, the MLE weights targets and age differently,
    # and doesn't converge. 
    # group_by(target, age_group) %>%
    arrange(date) %>%
    mutate(max_normal = max(death, na.rm = T),
          death = death/max_normal,
          value = value/max_normal) %>%
   filter(!is.na(death), !is.na(value)) %>%
    ungroup()
  
  # Now get the negative log likelihood. Note we are concurrently fitting the sd
  ll <- -sum(dnorm(x=out_calib1$value,mean=out_calib1$death,sd=sig_dist,log=TRUE))
  
  # This code ensures that if the output of the log likelihood is NA, the MLE won't stop. Instead it will return
  # an extremely large, positive value of the negative log likelihood and keep iterating.
  ll_final = if_else(is.na(ll), 10^6, ll)
  
  return(ll_final)
}