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
               
               beta_COV=10*(0.27/5), epsilon_COV = 1/5, omega_COV = 1/(5*30), gammaI_COV = 1/5, gammaH_COV = 1/15, #gammaH placeholder    
               alphaC_COV = 0.0015, alphaOC_COV = 0.0015, alphaA_COV = 0.005, alphaS_COV = 0.025,
               probHC_COV = 0.00013, probHC_COV_vax = .5*0.01, # source of reduction rate is CDC 
               probHOC_COV = 0.00013, probHOC_COV_vax = .5*0.01,
               probHA_COV = 0.00017, probHA_COV_vax = .56*0.035,
               probHS_COV = 0.0012, probHS_COV_vax = .58*0.2,
               ss_COV = 0.5, si_COV = 0.5,
               vaccC_COV = 0, vaccOC_COV = 0, vaccA_COV = 0, vaccS_COV = 0,
               ve_COV = .88)
