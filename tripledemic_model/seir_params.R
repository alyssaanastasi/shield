library(deSolve)
library(tidyverse)
library(socialmixr)

#### General parameters ####

times <- 1:5000
PopT <- 1.2*10^7 ## 12 million total (Illinois )


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
                "E_C_RSV", "I_C_RSV", "H_C_RSV", "D_C_RSV", "R_C_RSV",
               
                "E_OC_RSV_vax", "I_OC_RSV_vax", "H_OC_RSV_vax", "D_OC_RSV_vax",
                "E_OC_RSV", "I_OC_RSV", "H_OC_RSV", "D_OC_RSV", "R_OC_RSV",
               
                "E_A_RSV_vax", "I_A_RSV_vax", "H_A_RSV_vax", "D_A_RSV_vax",
                "E_A_RSV", "I_A_RSV", "H_A_RSV", "D_A_RSV", "R_A_RSV",
               
                "E_S_RSV_vax", "I_S_RSV_vax", "H_S_RSV_vax", "D_S_RSV_vax",
                "E_S_RSV", "I_S_RSV", "H_S_RSV", "D_S_RSV", "R_S_RSV",
               
                 "E_C_COV_vax", "I_C_COV_vax", "H_C_COV_vax", "D_C_COV_vax",
                 "E_C_COV", "I_C_COV", "H_C_COV", "D_C_COV", "R_C_COV",
               
                 "E_OC_COV_vax", "I_OC_COV_vax", "H_OC_COV_vax", "D_OC_COV_vax",
                 "E_OC_COV", "I_OC_COV", "H_OC_COV", "D_OC_COV", "R_OC_COV",
               
                 "E_A_COV_vax", "I_A_COV_vax", "H_A_COV_vax", "D_A_COV_vax",
                 "E_A_COV", "I_A_COV", "H_A_COV", "D_A_COV", "R_A_COV",
               
                 "E_S_COV_vax", "I_S_COV_vax", "H_S_COV_vax", "D_S_COV_vax",
                 "E_S_COV", "I_S_COV", "H_S_COV", "D_S_COV", "R_S_COV",
               
                 "E_C_FLU_vax", "I_C_FLU_vax", "H_C_FLU_vax", "D_C_FLU_vax",
                 "E_C_FLU", "I_C_FLU", "H_C_FLU", "D_C_FLU", "R_C_FLU",
               
                 "E_OC_FLU_vax", "I_OC_FLU_vax", "H_OC_FLU_vax", "D_OC_FLU_vax",
                 "E_OC_FLU", "I_OC_FLU", "H_OC_FLU", "D_OC_FLU", "R_OC_FLU",
               
                 "E_A_FLU_vax", "I_A_FLU_vax", "H_A_FLU_vax", "D_A_FLU_vax",
                 "E_A_FLU", "I_A_FLU", "H_A_FLU", "D_A_FLU", "R_A_FLU",
               
                 "E_S_FLU_vax", "I_S_FLU_vax", "H_S_FLU_vax", "D_S_FLU_vax",
                 "E_S_FLU", "I_S_FLU", "H_S_FLU", "D_S_FLU", "R_S_FLU"
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

novac_parms <- c(   #R0 - 3
               beta_RSV=0.031, epsilon_RSV = 1/5, omega_RSV = 1/(5*30), gammaI_RSV = 1/8, gammaH_RSV = 1/15,    
               alphaC_RSV = 0.00002, alphaOC_RSV = 0.000009, alphaA_RSV = 0.00011, alphaS_RSV = 0.00046,
               probHC_RSV = 0.08, probHC_RSV_vax = .5*0.08, # source of reduction rate is CDC 
               probHOC_RSV = 0.0013, probHOC_RSV_vax = .5*0.0013,
               probHA_RSV = 0.00118, probHA_RSV_vax = .56*0.00118,
               probHS_RSV = 0.00939, probHS_RSV_vax = .5*0.00939,
               ss_RSV = 0.5, si_RSV = 0.5,
               vaccC_RSV = 0, vaccOC_RSV = 0, vaccA_RSV = 0, vaccS_RSV = 0,
               ve_RSV = .81,
            
              # R0 - 3.82
              beta_COV=0.063, epsilon_COV = 1/3, omega_COV = 1/(6*30), gammaI_COV = 1/5, gammaH_COV = 1/15, 
              alphaC_COV = 0.008084800574919152, alphaOC_COV = 0.008084800574919152, alphaA_COV = 0.09281291984653527, alphaS_COV = 0.27612645839610195,
              probHC_COV = 6.122813945959567e-4, probHC_COV_vax = .3*6.122813945959567e-4, # source of reduction rate is CDC 
              probHOC_COV = 6.122813945959567e-4, probHOC_COV_vax = .3*6.122813945959567e-4,
              probHA_COV = 0.0026015249282712754, probHA_COV_vax = .5*0.0026015249282712754,
              probHS_COV = 0.03686573055858461, probHS_COV_vax = .5*0.03686573055858461,
              ss_COV = 0.5, si_COV = 0.5,
              vaccC_COV = 0.0, vaccOC_COV = 0.0, vaccA_COV = 0.0, vaccS_COV = 0.0,
              ve_COV = .94,
            
              # R0 - 3
              beta_FLU=0.036, epsilon_FLU = 1/3, omega_FLU = 1/(3*30), gammaI_FLU = 1/7, gammaH_FLU = 1/11,  
              alphaC_FLU = 0.02066115702479339, alphaOC_FLU = 0.02066115702479339, alphaA_FLU = 0.06416913946587537, alphaS_FLU = 0.12145254806225206,
              probHC_FLU = 5.370400336733193e-4, probHC_FLU_vax = .5*5.370400336733193e-4, # source of reduction rate is CDC 
              probHOC_FLU = 5.370400336733193e-4, probHOC_FLU_vax = .5*5.370400336733193e-4,
              probHA_FLU = 0.0011383064259089782, probHA_FLU_vax = .56*0.0011383064259089782,
              probHS_FLU = 0.036483639168077384, probHS_FLU_vax = .58*0.036483639168077384,
              ss_FLU = 0.5, si_FLU = 0.5,
              vaccC_FLU = 0.0, vaccOC_FLU = 0.0, vaccA_FLU = 0.0, vaccS_FLU = 0.0,
              ve_FLU = 0.9)

currvac_parms <- c(   #R0 - 3
              beta_RSV=0.031, epsilon_RSV = 1/5, omega_RSV = 1/(5*30), gammaI_RSV = 1/8, gammaH_RSV = 1/15,    
              alphaC_RSV = 0.00002, alphaOC_RSV = 0.000009, alphaA_RSV = 0.00011, alphaS_RSV = 0.00046,
              probHC_RSV = 0.08, probHC_RSV_vax = .5*0.08, # source of reduction rate is CDC 
              probHOC_RSV = 0.0013, probHOC_RSV_vax = .5*0.0013,
              probHA_RSV = 0.00118, probHA_RSV_vax = .56*0.00118,
              probHS_RSV = 0.00939, probHS_RSV_vax = .5*0.00939,
              ss_RSV = 0.5, si_RSV = 0.5,
              vaccC_RSV = 0.29, vaccOC_RSV = .222, vaccA_RSV = .222, vaccS_RSV = .222,
              ve_RSV = .81,
              
              # R0 - 3.82
              beta_COV=0.063, epsilon_COV = 1/3, omega_COV = 1/(6*30), gammaI_COV = 1/5, gammaH_COV = 1/15, 
              alphaC_COV = 0.008084800574919152, alphaOC_COV = 0.008084800574919152, alphaA_COV = 0.09281291984653527, alphaS_COV = 0.27612645839610195,
              probHC_COV = 6.122813945959567e-4, probHC_COV_vax = .3*6.122813945959567e-4, # source of reduction rate is CDC 
              probHOC_COV = 6.122813945959567e-4, probHOC_COV_vax = .3*6.122813945959567e-4,
              probHA_COV = 0.0026015249282712754, probHA_COV_vax = .5*0.0026015249282712754,
              probHS_COV = 0.03686573055858461, probHS_COV_vax = .5*0.03686573055858461,
              ss_COV = 0.5, si_COV = 0.5,
              vaccC_COV = 0.061, vaccOC_COV = 0.051, vaccA_COV = 0.07, vaccS_COV = 0.292,
              ve_COV = .94,
              
              # R0 - 3
              beta_FLU=0.036, epsilon_FLU = 1/3, omega_FLU = 1/(3*30), gammaI_FLU = 1/7, gammaH_FLU = 1/11,  
              alphaC_FLU = 0.02066115702479339, alphaOC_FLU = 0.02066115702479339, alphaA_FLU = 0.06416913946587537, alphaS_FLU = 0.12145254806225206,
              probHC_FLU = 5.370400336733193e-4, probHC_FLU_vax = .5*5.370400336733193e-4, # source of reduction rate is CDC 
              probHOC_FLU = 5.370400336733193e-4, probHOC_FLU_vax = .5*5.370400336733193e-4,
              probHA_FLU = 0.0011383064259089782, probHA_FLU_vax = .56*0.0011383064259089782,
              probHS_FLU = 0.036483639168077384, probHS_FLU_vax = .58*0.036483639168077384,
              ss_FLU = 0.5, si_FLU = 0.5,
              vaccC_FLU = 0.364, vaccOC_FLU = 0.216, vaccA_FLU = 0.2, vaccS_FLU = 0.58,
              ve_FLU = 0.9)

seniorvac_parms <- c(   #R0 - 3
  beta_RSV=0.031, epsilon_RSV = 1/5, omega_RSV = 1/(5*30), gammaI_RSV = 1/8, gammaH_RSV = 1/15,    
  alphaC_RSV = 0.00002, alphaOC_RSV = 0.000009, alphaA_RSV = 0.00011, alphaS_RSV = 0.00046,
  probHC_RSV = 0.08, probHC_RSV_vax = .5*0.08, # source of reduction rate is CDC 
  probHOC_RSV = 0.0013, probHOC_RSV_vax = .5*0.0013,
  probHA_RSV = 0.00118, probHA_RSV_vax = .56*0.00118,
  probHS_RSV = 0.00939, probHS_RSV_vax = .5*0.00939,
  ss_RSV = 0.5, si_RSV = 0.5,
  vaccC_RSV = 0.29, vaccOC_RSV = .222, vaccA_RSV = .222, vaccS_RSV = .322,
  ve_RSV = .81,
  
  # R0 - 3.82
  beta_COV=0.063, epsilon_COV = 1/3, omega_COV = 1/(6*30), gammaI_COV = 1/5, gammaH_COV = 1/15, 
  alphaC_COV = 0.008084800574919152, alphaOC_COV = 0.008084800574919152, alphaA_COV = 0.09281291984653527, alphaS_COV = 0.27612645839610195,
  probHC_COV = 6.122813945959567e-4, probHC_COV_vax = .3*6.122813945959567e-4, # source of reduction rate is CDC 
  probHOC_COV = 6.122813945959567e-4, probHOC_COV_vax = .3*6.122813945959567e-4,
  probHA_COV = 0.0026015249282712754, probHA_COV_vax = .5*0.0026015249282712754,
  probHS_COV = 0.03686573055858461, probHS_COV_vax = .5*0.03686573055858461,
  ss_COV = 0.5, si_COV = 0.5,
  vaccC_COV = 0.061, vaccOC_COV = 0.051, vaccA_COV = 0.07, vaccS_COV = 0.392,
  ve_COV = .94,
  
  # R0 - 3
  beta_FLU=0.036, epsilon_FLU = 1/3, omega_FLU = 1/(3*30), gammaI_FLU = 1/7, gammaH_FLU = 1/11,  
  alphaC_FLU = 0.02066115702479339, alphaOC_FLU = 0.02066115702479339, alphaA_FLU = 0.06416913946587537, alphaS_FLU = 0.12145254806225206,
  probHC_FLU = 5.370400336733193e-4, probHC_FLU_vax = .5*5.370400336733193e-4, # source of reduction rate is CDC 
  probHOC_FLU = 5.370400336733193e-4, probHOC_FLU_vax = .5*5.370400336733193e-4,
  probHA_FLU = 0.0011383064259089782, probHA_FLU_vax = .56*0.0011383064259089782,
  probHS_FLU = 0.036483639168077384, probHS_FLU_vax = .58*0.036483639168077384,
  ss_FLU = 0.5, si_FLU = 0.5,
  vaccC_FLU = 0.364, vaccOC_FLU = 0.216, vaccA_FLU = 0.2, vaccS_FLU = 0.68,
  ve_FLU = 0.9)

childrenvac_parms <- c(   #R0 - 3
  beta_RSV=0.031, epsilon_RSV = 1/5, omega_RSV = 1/(5*30), gammaI_RSV = 1/8, gammaH_RSV = 1/15,    
  alphaC_RSV = 0.00002, alphaOC_RSV = 0.000009, alphaA_RSV = 0.00011, alphaS_RSV = 0.00046,
  probHC_RSV = 0.08, probHC_RSV_vax = .5*0.08, # source of reduction rate is CDC 
  probHOC_RSV = 0.0013, probHOC_RSV_vax = .5*0.0013,
  probHA_RSV = 0.00118, probHA_RSV_vax = .56*0.00118,
  probHS_RSV = 0.00939, probHS_RSV_vax = .5*0.00939,
  ss_RSV = 0.5, si_RSV = 0.5,
  vaccC_RSV = 0.39, vaccOC_RSV = .222, vaccA_RSV = .222, vaccS_RSV = .222,
  ve_RSV = .81,
  
  # R0 - 3.82
  beta_COV=0.063, epsilon_COV = 1/3, omega_COV = 1/(6*30), gammaI_COV = 1/5, gammaH_COV = 1/15, 
  alphaC_COV = 0.008084800574919152, alphaOC_COV = 0.008084800574919152, alphaA_COV = 0.09281291984653527, alphaS_COV = 0.27612645839610195,
  probHC_COV = 6.122813945959567e-4, probHC_COV_vax = .3*6.122813945959567e-4, # source of reduction rate is CDC 
  probHOC_COV = 6.122813945959567e-4, probHOC_COV_vax = .3*6.122813945959567e-4,
  probHA_COV = 0.0026015249282712754, probHA_COV_vax = .5*0.0026015249282712754,
  probHS_COV = 0.03686573055858461, probHS_COV_vax = .5*0.03686573055858461,
  ss_COV = 0.5, si_COV = 0.5,
  vaccC_COV = 0.161, vaccOC_COV = 0.051, vaccA_COV = 0.07, vaccS_COV = 0.292,
  ve_COV = .94,
  
  # R0 - 3
  beta_FLU=0.036, epsilon_FLU = 1/3, omega_FLU = 1/(3*30), gammaI_FLU = 1/7, gammaH_FLU = 1/11,  
  alphaC_FLU = 0.02066115702479339, alphaOC_FLU = 0.02066115702479339, alphaA_FLU = 0.06416913946587537, alphaS_FLU = 0.12145254806225206,
  probHC_FLU = 5.370400336733193e-4, probHC_FLU_vax = .5*5.370400336733193e-4, # source of reduction rate is CDC 
  probHOC_FLU = 5.370400336733193e-4, probHOC_FLU_vax = .5*5.370400336733193e-4,
  probHA_FLU = 0.0011383064259089782, probHA_FLU_vax = .56*0.0011383064259089782,
  probHS_FLU = 0.036483639168077384, probHS_FLU_vax = .58*0.036483639168077384,
  ss_FLU = 0.5, si_FLU = 0.5,
  vaccC_FLU = 0.464, vaccOC_FLU = 0.216, vaccA_FLU = 0.2, vaccS_FLU = 0.58,
  ve_FLU = 0.9)