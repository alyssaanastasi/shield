library(deSolve)
library(tidyverse)

#### General parameters ####
PopT <- 1.2*10^7 ## 20 millions total

percentAdult <- 5398257/12812508
percentParent <- (883257+109092+70752+265126)/4998395
percentAdultParent <- percentParent / percentAdult

Pop_child <- PopT/3
Pop_childless_adult <- PopT/3 * (1-percentAdultParent)
Pop_parent <- PopT/3 * percentAdultParent
Pop_senior <- PopT/3
names_var <- c("S_C1", "E_C1", "I_C1", "R_C1", "D_C1",
              "S_C2", "E_C2", "I_C2", "R_C2", "D_C2",
              "S_CA1", "E_CA1", "I_CA1", "R_CA1", "D_CA1",
              "S_CA2", "E_CA2", "I_CA2", "R_CA2", "D_CA2",
              "S_P1", "E_P1", "I_P1", "R_P1", "D_P1",
              "S_P2", "E_P2", "I_P2", "R_P2", "D_P2",
              "S_S1", "E_S1", "I_S1", "R_S1", "D_S1",
              "S_S2", "E_S2", "I_S2", "R_S2", "D_S2")

init <- rep(0,length(names_var))
names(init) <- names_var
init["S_C1"] <- Pop_child - 1
init["E_C1"] <- 1
init["S_CA1"] <- Pop_childless_adult - 1
init["E_CA1"] <- 1
init["S_P1"] <- Pop_parent - 1
init["E_P1"] <- 1
init["S_S1"] <- Pop_senior - 1
init["E_S2"] <- 1

#sophie's model parms
# covid_parms_wt <- c(beta = 10*(0.189)/5, epsilon = 1, phi = 1/3, gamma = 1/5, sigma = 0.017, omega = 1/(6*30) )
                    
# source for covid death rates: https://wwwnc.cdc.gov/eid/article/30/6/23-1285-t2    
# source for age contact rates: 
covid_parms <- c(epsilon = 1/3, omega = 1/(6*30),  b = 1,  mu = 10*(0.189/5), gamma = 1/5,     
          alphaC1 = 0.0015, alphaC2 = .1*0.0015,
          alphaA1 = 0.005, alphaA2 = .1*0.005, 
          alphaS1 = 0.025, alphaS2 = .1*0.025,
          cCC = 2.1, cCCA = 1, cCP = 2, cCS = 0.2, cAA = 3, cSA = 0.5, cSS = 1,
          ss = 0.5, si = 0.5)

# sophie's model parms
# flu_parms <- c(beta = 10*((0.257 + 0.305)/2)/7, epsilon = 1, phi = 1, gamma = 1/7, sigma = 0.000159, omega = 1/(15*30))

#source for flu death rates: https://pmc.ncbi.nlm.nih.gov/articles/PMC10279999/table/irv13146-tbl-0001/ 
flu_parms <- c(epsilon = 1, omega = 1/(15*30),  b = 1,  mu = 10*((0.257 + 0.305)/2)/7, gamma = 1/7,     
               alphaC1 = 0.00001, alphaC2 = .1*0.00001, 
               alphaA1 = 0.00005, alphaA2 = .1*0.00005,
               alphaS1 = 0.001, alphaS2 = .1*0.001,
               cCC = 2.1, cCCA = 1, cCP = 2, cCS = 0.2, cAA = 3, cSA = 0.5, cSS = 1,
               ss = 0.5, si = 0.5)

pertussis_parms <- c(epsilon = 1/7, omega = 1/(55*365),  b = 1,  mu = 10*(0.11/5), gamma = 1/(3*7),     
                     alphaC1 = 0.0015, alphaC2 = .1*0.0015,
                     alphaA1 = 0.005, alphaA2 = .1*0.005, 
                     alphaS1 = 0.025, alphaS2 = .1*0.025,
                     cCC = 2.1, cCCA = 1, cCP = 2, cCS = 0.2, cAA = 3, cSA = 0.5, cSS = 1,
                     ss = 0.5, si = 0.5)

rsv_parms <- c(epsilon = 1/5, omega = 1/(5*30),  b = 1,  mu = 10*(0.27/5), gamma = 1/5,     
               alphaC1 = 0.0015, alphaC2 = .1*0.0015,
               alphaA1 = 0.005, alphaA2 = .1*0.005, 
               alphaS1 = 0.025, alphaS2 = .1*0.025,
               cCC = 2.1, cCCA = 1, cCP = 2, cCS = 0.2, cAA = 3, cSA = 0.5, cSS = 1,
               ss = 0.5, si = 0.5)




