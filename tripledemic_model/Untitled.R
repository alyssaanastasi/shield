# FLU



```{r}
# We write down the matrices to compute R0 (WITHOUT isolation incorporated -- including T compartments gives a singular matrix )

#FLU

# a vector naming all infected classes
istates=c("E_C_FLU_vax", "I_C_FLU_vax", "H_C_FLU_vax", 
          "E_C_FLU", "I_C_FLU", "H_C_FLU", 
          "E_OC_FLU_vax", "I_OC_FLU_vax", "H_OC_FLU_vax", 
          "E_OC_FLU", "I_OC_FLU", "H_OC_FLU", 
          "E_A_FLU_vax", "I_A_FLU_vax", "H_A_FLU_vax", 
          "E_A_FLU", "I_A_FLU", "H_A_FLU", 
          "E_S_FLU_vax", "I_S_FLU_vax", "H_S_FLU_vax", 
          "E_S_FLU", "I_S_FLU", "H_S_FLU")

## a list that contains equations (as quotes) for completely new infections entering each infected compartment for each class
flist=c(
  dE_C_FLU_vax=quote((beta_FLU*
                        (cCC*(I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax)/popC + 
                           cCOC*(I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax)/popOC + 
                           cCA*(I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax)/popA + 
                           cCS*(I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax)/popS) *
                        vaccC_FLU * ve_FLU)*S_C),
  dI_C_FLU_vax=quote(0),
  dH_C_FLU_vax=quote(0),
  dE_C_FLU=quote(((beta_FLU*
                     (cCC*(I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax)/popC + 
                        cCOC*(I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax)/popOC + 
                        cCA*(I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax)/popA + 
                        cCS*(I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax)/popS)) * 
                    (1-vaccC_FLU))*S_C), 
  dI_C_FLU=quote(0),
  dH_C_FLU=quote(0),
  dE_OC_FLU_vax=quote((beta_FLU*
                         (cOCC*(I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax)/popC + 
                            cOCOC*(I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax)/popOC + 
                            cOCA*(I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax)/popA + 
                            cOCS*(I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax)/popS) *
                         vaccOC_FLU * ve_FLU)*S_OC),
  dI_OC_FLU_vax=quote(0),
  dH_OC_FLU_vax=quote(0),
  dE_OC_FLU=quote(((beta_FLU*
                      (cOCC*(I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax)/popC + 
                         cOCOC*(I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax)/popOC + 
                         cOCA*(I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax)/popA + 
                         cOCS*(I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax)/popS)) * 
                     (1-vaccOC_FLU))*S_OC), 
  dI_OC_FLU=quote(0),
  dH_OC_FLU=quote(0),
  dE_A_FLU_vax=quote((beta_FLU*
                        (cAC*(I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax)/popC + 
                           cAOC*(I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax)/popOC + 
                           cAA*(I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax)/popA + 
                           cAS*(I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax)/popS) *
                        vaccA_FLU * ve_FLU)*S_A),
  dI_A_FLU_vax=quote(0),
  dH_A_FLU_vax=quote(0),
  dE_A_FLU=quote(((beta_FLU*
                     (cAC*(I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax)/popC + 
                        cAOC*(I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax)/popOC + 
                        cAA*(I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax)/popA + 
                        cAS*(I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax)/popS)) * 
                    (1-vaccA_FLU))*S_A), 
  dI_A_FLU=quote(0),
  dH_A_FLU=quote(0),
  dE_S_FLU_vax=quote((beta_FLU*
                        (cSC*(I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax)/popC + 
                           cSOC*(I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax)/popOC + 
                           cSA*(I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax)/popA + 
                           cSS*(I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax)/popS) *
                        vaccS_FLU * ve_FLU)*S_S),
  dI_S_FLU_vax=quote(0),
  dH_S_FLU_vax=quote(0),
  dE_S_FLU=quote(((beta_FLU*
                     (cSC*(I_C_FLU + si_FLU*I_C_FLU_vax + H_C_FLU + si_FLU*H_C_FLU_vax)/popC + 
                        cSOC*(I_OC_FLU + si_FLU*I_OC_FLU_vax + H_OC_FLU + si_FLU*H_OC_FLU_vax)/popOC + 
                        cSA*(I_A_FLU + si_FLU*I_A_FLU_vax + H_A_FLU + si_FLU*H_A_FLU_vax)/popA + 
                        cSS*(I_S_FLU + si_FLU*I_S_FLU_vax + H_S_FLU + si_FLU*H_S_FLU_vax)/popS)) * 
                    (1-vaccS_FLU))*S_S), 
  dI_S_FLU=quote(0),
  dH_S_FLU=quote(0))



#a list that contains the equations (as quotes) for losses out of each infected compartment minus the equations (as quotes) for all gains into each infected compartment that does not represent new infections but transfers among infectious classes

vlist_gains <- c(
  # Order: E I H
  ## C
  quote(0), quote(epsilon_FLU*E_C_FLU_vax), quote(probHC_FLU_vax*gammaI_FLU*I_C_FLU_vax),
  quote(0), quote(epsilon_FLU*E_C_FLU), quote(probHC_FLU_vax*gammaI_FLU*I_C_FLU),
  
  ## OC
  quote(0), quote(epsilon_FLU*E_OC_FLU_vax), quote(probHOC_FLU_vax*gammaI_FLU*I_OC_FLU_vax),
  quote(0), quote(epsilon_FLU*E_OC_FLU), quote(probHOC_FLU_vax*gammaI_FLU*I_OC_FLU),
  
  ## A
  quote(0), quote(epsilon_FLU*E_A_FLU_vax), quote(probHA_FLU_vax*gammaI_FLU*I_A_FLU_vax),
  quote(0), quote(epsilon_FLU*E_A_FLU), quote(probHA_FLU_vax*gammaI_FLU*I_A_FLU),
  
  ## S
  quote(0), quote(epsilon_FLU*E_S_FLU_vax), quote(probHS_FLU_vax*gammaI_FLU*I_S_FLU_vax),
  quote(0), quote(epsilon_FLU*E_S_FLU), quote(probHS_FLU_vax*gammaI_FLU*I_S_FLU)
  
)

vlist_losses = c(
  # Order: E I H
  ## C
  quote(epsilon_FLU*E_C_FLU_vax), quote(gammaI_FLU*I_C_FLU_vax), quote(gammaH_FLU*H_C_FLU_vax),
  quote(epsilon_FLU*E_C_FLU), quote(gammaI_FLU*I_C_FLU), quote(gammaH_FLU*H_C_FLU),
  
  ## OC
  quote(epsilon_FLU*E_OC_FLU_vax), quote(gammaI_FLU*I_OC_FLU_vax), quote(gammaH_FLU*H_OC_FLU_vax),
  quote(epsilon_FLU*E_OC_FLU), quote(gammaI_FLU*I_OC_FLU), quote(gammaH_FLU*H_OC_FLU),
  
  ## A
  quote(epsilon_FLU*E_A_FLU_vax), quote(gammaI_FLU*I_A_FLU_vax), quote(gammaH_FLU*H_A_FLU_vax),
  quote(epsilon_FLU*E_A_FLU), quote(gammaI_FLU*I_A_FLU), quote(gammaH_FLU*H_A_FLU),
  
  ## S
  quote(epsilon_FLU*E_S_FLU_vax), quote(gammaI_FLU*I_S_FLU_vax), quote(gammaH_FLU*H_S_FLU_vax),
  quote(epsilon_FLU*E_S_FLU), quote(gammaI_FLU*I_S_FLU), quote(gammaH_FLU*H_S_FLU)
)

vlist <- NULL
for (i in 1:24){
  x <- substitute(a-b, unlist(list(a=vlist_losses[i], b=vlist_gains[i])))
  vlist <- c(vlist, x)
}


para = list( beta_FLU=10*(0.27/5), epsilon_FLU = 1/5, omega_FLU = 1/(5*30), gammaI_FLU = 1/8, gammaH_FLU = 1/15,    
             alphaC_FLU = 0.00002, alphaOC_FLU = 0.000009, alphaA_FLU = 0.00011, alphaS_FLU = 0.00046,
             probHC_FLU = 0.08, probHC_FLU_vax = .5*0.08, # source of reduction rate is CDC 
             probHOC_FLU = 0.0013, probHOC_FLU_vax = .5*0.0013,
             probHA_FLU = 0.00118, probHA_FLU_vax = .56*0.00118,
             probHS_FLU = 0.00939, probHS_FLU_vax = .5*0.00939,
             ss_FLU = 0.5, si_FLU = 0.5,
             vaccC_FLU = 0, vaccOC_FLU = 0, vaccA_FLU = 0, vaccS_FLU = 0,
             ve_FLU = .81,
             popC = Pop_children, popOC = Pop_older_children, popA = Pop_adult, popS = Pop_senior
)

df = list(S_C = Pop_children, S_OC = Pop_older_children, S_A = Pop_adult, S_S = Pop_senior,
          E_C_FLU_vax=0, I_C_FLU_vax=0, H_C_FLU_vax=0, 
          E_C_FLU=0, I_C_FLU=0, H_C_FLU=0, 
          E_OC_FLU_vax=0, I_OC_FLU_vax=0, H_OC_FLU_vax=0, 
          E_OC_FLU=0, I_OC_FLU=0, H_OC_FLU=0, 
          E_A_FLU_vax=0, I_A_FLU_vax=0, H_A_FLU_vax=0, 
          E_A_FLU=0, I_A_FLU=0, H_A_FLU=0, 
          E_S_FLU_vax=0, I_S_FLU_vax=0, H_S_FLU_vax=0, 
          E_S_FLU=0, I_S_FLU=0, H_S_FLU=0
)

nextgenR0(Istates=istates, Flist=flist, Vlist=vlist, parameters=para, dfe=df)

## calculate R0 across levels of beta2

beta_seq <- seq(0.01,0.2,0.001)

R0_grid <- NULL

# FLU target RO = 3.0
# https://pubmed.ncbi.nlm.nih.gov/27716828/

for (i in seq_along(beta_seq)){
  
  para['beta_FLU'] = beta_seq[i]
  
  R0 <- nextgenR0(Istates=istates, Flist=flist, Vlist=vlist, parameters=para, dfe=df)
  
  R0_grid <- R0_grid %>% rbind(tibble(R0_value = R0, beta = beta_seq[i]))
  
}

## beta = 0.183
```