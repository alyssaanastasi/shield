library(deSolve)
library(tidyverse)
library(socialmixr)

#### General parameters ####

times <- 1:500
PopT <- 1.2*10^7 ## 20 million total


Pop_children <- PopT/4
Pop_older_children <- PopT/4 
Pop_adult <- PopT/4
Pop_senior <- PopT/4


#Set contact rates globally
data(polymod, package = "socialmixr")
cm <- contact_matrix(polymod, countries = "United Kingdom", age.limits = c(0, 6, 19, 66), symmetric = TRUE)
cmat <- cm[["matrix"]]
C_vals <- cmat["[0,6)", ]
cCC <- C_vals[1]
cCOC <- C_vals[2]
cCA <- C_vals[3]
cCS <- C_vals[4]
OC_vals <- cmat["[6,19)", ]
cOCC <- OC_vals[1]
cOCOC <- OC_vals[2]
cOCA <- OC_vals[3]
cOCS <- OC_vals[4]
A_vals <- cmat["[19,66)", ]
cAC <- A_vals[1]
cAOC <- A_vals[2]
cAA <- A_vals[3]
cAS <- A_vals[4]
S_vals <- cmat["66+", ]
cSC <- S_vals[1]
cSOC <- S_vals[2]
cSA <- S_vals[3]
cSS <- S_vals[4]


names_var <- c("S_C", "S_OC", "S_A", "S_S",
                "E_C_RSV_vax", "I_C_RSV_vax", "H_C_RSV_vax", "D_C_RSV_vax",
                "E_C_RSV", "I_C_RSV", "H_C_RSV", "D_C_RSV",
                "E_OC_RSV_vax", "I_OC_RSV_vax", "H_OC_RSV_vax", "D_OC_RSV_vax",
                "E_OC_RSV", "I_OC_RSV", "H_OC_RSV", "D_OC_RSV",
                "E_A_RSV_vax", "I_A_RSV_vax", "H_A_RSV_vax", "D_A_RSV_vax",
                "E_A_RSV", "I_A_RSV", "H_A_RSV", "D_A_RSV",
                "E_S_RSV_vax", "I_S_RSV_vax", "H_S_RSV_vax", "D_S_RSV_vax",
                "E_S_RSV", "I_S_RSV", "H_S_RSV", "D_S_RSV", 
               
                 "E_C_COV_vax", "I_C_COV_vax", "H_C_COV_vax", "D_C_COV_vax",
                 "E_C_COV", "I_C_COV", "H_C_COV", "D_C_COV",
                 "E_OC_COV_vax", "I_OC_COV_vax", "H_OC_COV_vax", "D_OC_COV_vax",
                 "E_OC_COV", "I_OC_COV", "H_OC_COV", "D_OC_COV",
                 "E_A_COV_vax", "I_A_COV_vax", "H_A_COV_vax", "D_A_COV_vax",
                 "E_A_COV", "I_A_COV", "H_A_COV", "D_A_COV",
                 "E_S_COV_vax", "I_S_COV_vax", "H_S_COV_vax", "D_S_COV_vax",
                 "E_S_COV", "I_S_COV", "H_S_COV", "D_S_COV",
               
               "E_C_FLU_vax", "I_C_FLU_vax", "H_C_FLU_vax", "D_C_FLU_vax",
               "E_C_FLU", "I_C_FLU", "H_C_FLU", "D_C_FLU",
               "E_OC_FLU_vax", "I_OC_FLU_vax", "H_OC_FLU_vax", "D_OC_FLU_vax",
               "E_OC_FLU", "I_OC_FLU", "H_OC_FLU", "D_OC_FLU",
               "E_A_FLU_vax", "I_A_FLU_vax", "H_A_FLU_vax", "D_A_FLU_vax",
               "E_A_FLU", "I_A_FLU", "H_A_FLU", "D_A_FLU",
               "E_S_FLU_vax", "I_S_FLU_vax", "H_S_FLU_vax", "D_S_FLU_vax",
               "E_S_FLU", "I_S_FLU", "H_S_FLU", "D_S_FLU"
                )



init <- rep(0,length(names_var))
names(init) <- names_var
init["S_C"] <- Pop_children - 6
init["I_C_RSV_vax"] <- 1
init["I_C_RSV"] <- 1
init["I_C_COV_vax"] <- 1
init["I_C_COV"] <- 1
init["I_C_FLU_vax"] <- 1
init["I_C_FLU"] <- 1
init["S_OC"] <- Pop_older_children - 6
init["I_OC_RSV_vax"] <- 1
init["I_OC_RSV"] <- 1
init["I_OC_COV_vax"] <- 1
init["I_OC_COV"] <- 1
init["I_OC_FLU_vax"] <- 1
init["I_OC_FLU"] <- 1
init["S_A"] <- Pop_adult - 6
init["I_A_RSV_vax"] <- 1
init["I_A_RSV"] <- 1
init["I_A_COV_vax"] <- 1
init["I_A_COV"] <- 1
init["I_A_FLU_vax"] <- 1
init["I_A_FLU"] <- 1
init["S_S"] <- Pop_senior - 6
init["I_S_RSV_vax"] <- 1
init["I_S_RSV"] <- 1
init["I_S_COV_vax"] <- 1
init["I_S_COV"] <- 1
init["I_S_FLU_vax"] <- 1
init["I_S_FLU"] <- 1

total_pop <- c(
  "Seniors" = Pop_senior,
  "Adults" = Pop_adult,
  "Older Children" = Pop_older_children,
  "Children" = Pop_children
)

parms <- c( # 10*(0.27/5)
               beta_RSV=0.031, epsilon_RSV = 1/5, omega_RSV = 1/(5*30), gammaI_RSV = 1/8, gammaH_RSV = 1/15,    
               alphaC_RSV = 0.00002, alphaOC_RSV = 0.000009, alphaA_RSV = 0.00011, alphaS_RSV = 0.00046,
               probHC_RSV = 0.08, probHC_RSV_vax = .5*0.08, # source of reduction rate is CDC 
               probHOC_RSV = 0.0013, probHOC_RSV_vax = .5*0.0013,
               probHA_RSV = 0.00118, probHA_RSV_vax = .56*0.00118,
               probHS_RSV = 0.00939, probHS_RSV_vax = .5*0.00939,
               ss_RSV = 0.5, si_RSV = 0.5,
               vaccC_RSV = 0.3, vaccOC_RSV = 0.3, vaccA_RSV = 0.3, vaccS_RSV = 0.3,
               ve_RSV = .81,
            
              # 10*(0.189)/5
              beta_COV=0.032, epsilon_COV = 1/3, omega_COV = 1/(6*30), gammaI_COV = 1/5, gammaH_COV = 1/15, 
              alphaC_COV = 0.0015, alphaOC_COV = 0.0015, alphaA_COV = 0.0015, alphaS_COV = 0.025,
              probHC_COV = 0.01, probHC_COV_vax = .3*0.01, # source of reduction rate is CDC 
              probHOC_COV = 0.01, probHOC_COV_vax = .3*0.01,
              probHA_COV = 0.035, probHA_COV_vax = .5*0.035,
              probHS_COV = 0.2, probHS_COV_vax = .5*0.2,
              ss_COV = 0.5, si_COV = 0.5,
              vaccC_COV = 0.3, vaccOC_COV = 0.3, vaccA_COV = 0.3, vaccS_COV = 0.3,
              ve_COV = .94,
            
              #10*((0.257 + 0.305)/2)/7
              beta_FLU=0.015, epsilon_FLU = 1/3, omega_FLU = 1/(15*30), gammaI_FLU = 1/7, gammaH_FLU = 1/11,  
              alphaC_FLU = 0.00001, alphaOC_FLU = 0.00001, alphaA_FLU = 0.00005, alphaS_FLU = 0.001,
              probHC_FLU = 0.00013, probHC_FLU_vax = .5*0.00013, # source of reduction rate is CDC 
              probHOC_FLU = 0.00013, probHOC_FLU_vax = .5*0.00013,
              probHA_FLU = 0.00017, probHA_FLU_vax = 56*0.00017,
              probHS_FLU = 0.0012, probHS_FLU_vax = .58*0.0012,
              ss_FLU = 0.5, si_FLU = 0.5,
              vaccC_FLU = 0.3, vaccOC_FLU = 0.3, vaccA_FLU = 0.3, vaccS_FLU = 0.3,
              ve_FLU = 0.9)


# RSV

rsv_names_var <- c("S_C", "S_OC", "S_A", "S_S",
                    "E_C_RSV_vax", "I_C_RSV_vax", "H_C_RSV_vax", "D_C_RSV_vax",
                    "E_C_RSV", "I_C_RSV", "H_C_RSV", "D_C_RSV",
                    "E_OC_RSV_vax", "I_OC_RSV_vax", "H_OC_RSV_vax", "D_OC_RSV_vax",
                    "E_OC_RSV", "I_OC_RSV", "H_OC_RSV", "D_OC_RSV",
                    "E_A_RSV_vax", "I_A_RSV_vax", "H_A_RSV_vax", "D_A_RSV_vax",
                    "E_A_RSV", "I_A_RSV", "H_A_RSV", "D_A_RSV",
                    "E_S_RSV_vax", "I_S_RSV_vax", "H_S_RSV_vax", "D_S_RSV_vax",
                    "E_S_RSV", "I_S_RSV", "H_S_RSV", "D_S_RSV")

init_rsv <- rep(0,length(rsv_names_var))
names(init_rsv) <- rsv_names_var
init_rsv["S_C"] <- Pop_children - 2
init_rsv["I_C_RSV_vax"] <- 1
init_rsv["I_C_RSV"] <- 1
init_rsv["S_OC"] <- Pop_older_children - 2
init_rsv["I_OC_RSV_vax"] <- 1
init_rsv["I_OC_RSV"] <- 1
init_rsv["S_A"] <- Pop_adult - 2
init_rsv["I_A_RSV_vax"] <- 1
init_rsv["I_A_RSV"] <- 1
init_rsv["S_S"] <- Pop_senior - 2
init_rsv["I_S_RSV_vax"] <- 2
init_rsv["I_S_RSV"] <- 2

rsv_parms <- c(cCC = 2.1, cCOC = 2, cCA = 2, cCS = 0.2, 
               cOCOC = 2.1, cOCA = 2, cOCS = 0.2, 
               cAA = 3, cAS = 0.5, cSS = 1,
               
               beta_RSV=10*(0.27/5), epsilon_RSV = 1/5, omega_RSV = 1/(5*30), gammaI_RSV = 1/5, gammaH_RSV = 1/15, #gammaH placeholder    
               alphaC_RSV = 0.0015, alphaOC_RSV = 0.0015, alphaA_RSV = 0.005, alphaS_RSV = 0.025,
               probHC_RSV = 0.00013, probHC_RSV_vax = .5*0.01, # source of reduction rate is CDC 
               probHOC_RSV = 0.00013, probHOC_RSV_vax = .5*0.01,
               probHA_RSV = 0.00017, probHA_RSV_vax = .56*0.035,
               probHS_RSV = 0.0012, probHS_RSV_vax = .58*0.2,
               ss_RSV = 0.5, si_RSV = 0.5,
               vaccC_RSV = 0, vaccOC_RSV = 0, vaccA_RSV = 0, vaccS_RSV = 0,
               ve_RSV = .88)

# COV

COV_names_var <- c("S_C", "S_OC", "S_A", "S_S",
                   "E_C_COV_vax", "I_C_COV_vax", "H_C_COV_vax", "D_C_COV_vax",
                   "E_C_COV", "I_C_COV", "H_C_COV", "D_C_COV",
                   "E_OC_COV_vax", "I_OC_COV_vax", "H_OC_COV_vax", "D_OC_COV_vax",
                   "E_OC_COV", "I_OC_COV", "H_OC_COV", "D_OC_COV",
                   "E_A_COV_vax", "I_A_COV_vax", "H_A_COV_vax", "D_A_COV_vax",
                   "E_A_COV", "I_A_COV", "H_A_COV", "D_A_COV",
                   "E_S_COV_vax", "I_S_COV_vax", "H_S_COV_vax", "D_S_COV_vax",
                   "E_S_COV", "I_S_COV", "H_S_COV", "D_S_COV")

init_COV <- rep(0,length(COV_names_var))
names(init_COV) <- COV_names_var
init_COV["S_C"] <- Pop_children - 2
init_COV["I_C_COV_vax"] <- 1
init_COV["I_C_COV"] <- 1
init_COV["S_OC"] <- Pop_older_children - 2
init_COV["I_OC_COV_vax"] <- 1
init_COV["I_OC_COV"] <- 1
init_COV["S_A"] <- Pop_adult - 2
init_COV["I_A_COV_vax"] <- 1
init_COV["I_A_COV"] <- 1
init_COV["S_S"] <- Pop_senior - 2
init_COV["I_S_COV_vax"] <- 2
init_COV["I_S_COV"] <- 2

COV_parms <- c(cCC = 2.1, cCOC = 2, cCA = 2, cCS = 0.2, 
               cOCOC = 2.1, cOCA = 2, cOCS = 0.2, 
               cAA = 3, cAS = 0.5, cSS = 1,
               
               beta_COV=10*(0.189)/5, epsilon_COV = 1/3, omega_COV = 1/(6*30), gammaI_COV = 1/5, gammaH_COV = 1/15, 
               alphaC_COV = 0.0015, alphaOC_COV = 0.0015, alphaA_COV = 0.0015, alphaS_COV = 0.025,
               probHC_COV = 0.01, probHC_COV_vax = .3*0.01, # source of reduction rate is CDC 
               probHOC_COV = 0.01, probHOC_COV_vax = .3*0.01,
               probHA_COV = 0.035, probHA_COV_vax = .5*0.035,
               probHS_COV = 0.2, probHS_COV_vax = .5*0.2,
               ss_COV = 0.5, si_COV = 0.5,
               vaccC_COV = 0.3, vaccOC_COV = 0.3, vaccA_COV = 0.3, vaccS_COV = 0.3,
               ve_COV = .94)

