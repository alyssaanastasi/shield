library(tidyverse)
library(deSolve)

#### ODE Equations ####
seir <- function(t, y, parms){
  # Parameters
  beta <- parms["beta"] ## transmission rate
  gamma <- parms["gamma"] ## rate of transition from I to H or R
  omega <- parms["omega"] ## rate of transition from H to R or D
  sigma <- parms["sigma"] ## proportion of infected that progress to hospitalization
  alpha <- parms["alpha"] ## proportion of hospitalized that end up dying
  h_cap <- parms["h_cap"] ## hospitalization capacity
  
  # State variables
  S <- y[1]
  I <- y[2]
  H <- y[3]
  R <- y[4]
  D <- y[5]
  
  #Total count
  N <- S + I + H + R
  I_eff <- H + I
  
  ## hospitalization capacity 
  
  if (t < 2) {
    prop_can_be_hospitalized <- 1
  } else{
   
  ## At this step, we need the previous hospitalization value
    ## NEED TO IMPLEMENT T-TIMESTEP instead of t-1
  H_tminus1 <- lagvalue(t-1, 3) ## takes in t (timepoint) and nr (number of lagged value, in this case H which is at position 3)
  
  print(H_tminus1)
  
  check_sign <- H - H_tminus1
  
  #print(check_sign)
  
  ## now calculate the proportion that can be hospitalized
  ## if the hospitalization compartment is stable or shrinking, no problem transitioning new hospitalizations
    if (check_sign <= 0){
      prop_can_be_hospitalized <- 1
    } else {
      ## get ratio of remaining capacity to new hospitalizations
      fractional_term <- (h_cap - H_tminus1)/(H-H_tminus1)
      ## if capacity exceeds new adds, then term is 1 because 100% can go to hospital. Otherwise, ratio is the proportion that can go
      prop_can_be_hospitalized = min(fractional_term, 1)
    }
  }
  
  ## Differential equations
  # Vaccinated parent-child pairs
  dS <- -beta*S*I_eff/N
  dI <- beta*S*I_eff/N - gamma*I
  dH <- prop_can_be_hospitalized*sigma*gamma*I - omega*H
  dR <- (1-sigma)*gamma*I + (1-alpha)*omega*H
  dD <- alpha*omega*H + (1-prop_can_be_hospitalized)*sigma*gamma*I
  
  # Return list of gradients
  list(c(
    dS, dI, dH, dR, dD
  ))
}

#### Parms ####

parms <- c(beta = 0.25, 
           gamma = 1/7,
           omega = 1/7,
           sigma = 0.5,
           alpha = 0.05,
           h_cap = 1000)

#### init ####

names_var <- c("S", "I", "H", "R", "D")
init <- rep(0,length(names_var))
names(init) <- names_var

init["S"] <- 10000
init["I"] <- 100
init["H"] <- 100

#### Run ####
times <- c(1:365)
out_data <- dede(y=init, func = seir, 
                times=times, parms = parms) 

x <- diagnostics(out_data) # , x$rstate[1] # will give me the last timestep size
## x <- diagnostics(out_data), x$rstate[2] will give me the timestep to be attempted next
## but I would like to do this WITHIN the solver

plot_out <- as.data.frame(out_data) %>% 
  pivot_longer(-time) %>%
  ggplot(aes(x = time, y = value, color= name)) + 
  geom_line() + 
  geom_hline(aes(yintercept = 1000))
plot_out






