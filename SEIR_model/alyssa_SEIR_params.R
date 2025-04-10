library(deSolve)
library(tidyverse)

#### General parameters ####
PopT <- 2*10^7 ## 20 millions total
Pop_child <- PopT/3
Pop_childless_adult <- PopT/3
Pop_parent <- PopT/3
names_var <- c("S_C1", "E_C1", "I_C1A", "I_C1S", "R_C1", "D_C1",
                "S_C2", "E_C2", "I_C2A", "I_C2S", "R_C2", "D_C2",
                "S_CA1", "E_CA1", "I_CA1A", "I_CA1S", "R_CA1", "D_CA1",
                "S_CA2", "E_CA2", "I_CA2A", "I_CA2S", "R_CA2", "D_CA2",
                "S_P1", "E_P1", "I_P1A", "I_P1S", "R_P1", "D_P1",
                "S_P2", "E_P2", "I_P2A", "I_P2S", "R_P2", "D_P2",
                "Inf_C1", "Inf_C2", "Inf_CA1", "Inf_CA2", "Inf_P1", "Inf_P2")
init <- rep(0,length(names_var))
names(init) <- names_var
init["S_C1"] <- Pop_child - 1
init["E_C1"] <- 1
init["S_CA1"] <- Pop_childless_adult - 1
init["E_CA1"] <- 1
init["S_P1"] <- Pop_childless_adult - 1
init["E_P1"] <- 1

parms <- c(epsilon = 1/3, omega = 1/120, b = 0.5, mu = 0.42,
            aC1 = 0.5, aC2 = 0.7, aCA1 = 0.5, aCA2 = 0.7, aP1 = 0.5, aP2 = 0.7,
            gammaA = 1/5, gammaS = 1/10,
            alphaC1 = 0.02, alphaC2 = 0.015, alphaCA1 = 0.01, alphaCA2 = 0.008, alphaP1 = 0.005, alphaP2 = 0.004,
            cCC = 3 * 0.7,
            cCCA = 2 * 0.5,   
            cCP = 2 * 0.6,    
            cCACA = 3 * 0.4,  
            cCAP = 2 * 0.4, 
            cPP = 3 * 0.5,
            ss = 0.50, si = 0.5)


