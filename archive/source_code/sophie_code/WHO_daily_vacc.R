library(deSolve)
library(tidyverse)

params <- read_csv("../../Data/data_figures/paramsTable.csv")


type_III_resp <- function (t, Vmax, WVH, k) {
  return(Vmax*(t^k)/((WVH^k) + (t^k)))
}

vacc_weekly <- function (Vmax, WVH, k){
  datOut <- tibble(Week = 1:60, Vac = type_III_resp(1:60, Vmax,WVH, k)) %>%
    mutate(dailyVac =  c(Vac[1],diff(Vac)),
           VacCumSum = cumsum(dailyVac)) %>% 
    arrange(Week) %>%
    mutate(Day = Week*7-7+1, dailyVac=dailyVac/7)
  
  vaccF <- splinefun(x = datOut$Day, y = datOut$dailyVac)
  return(vaccF)
}

#### Daily faccinations ####
paramsWHO <- params %>% filter(Name %in% c("India", "Ecuador", "Malaysia"))
countriesWHO <- c("India", "Ecuador", "Malaysia")

dataAll <- tibble(NULL)
for(i in countriesWHO){
  temp <- paramsWHO %>% filter(Name ==i)
  for(j in c("H","L")){
    tempSES <- temp %>% filter(SES == j)
    vacc <- vacc_weekly(Vmax=tempSES$MaxVac, WVH=tempSES$WVH, k=tempSES$h)
    output <- tibble(Country=i, SES=j, Time=1:720, daily_vac=vacc(Time))
    dataAll <- rbind(dataAll, output)
  }
}


dataAll <- dataAll %>% mutate(daily_vac = pmax(0,daily_vac))
write_csv(dataAll, "../../../WHO_project/Vaccine Inequity Data/daily_vaccination.csv")

