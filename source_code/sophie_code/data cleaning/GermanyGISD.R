library(tidyverse)
library(readxl) 

SES <- read_excel("GISD_KREIS2012.xlsx")[c("Kreis2012", "GISD_2012")] %>% 
  rename(location = "Kreis2012") %>%
  rename(value = "GISD_2012") %>%
  add_column(measure = "GISD_2012")
pop <- read.csv("population.csv", sep = ";")[c("Regional.code", "Population")] %>% distinct() %>%
  rename(location = "Regional.code")


write.csv(SES,"C:/Users/sophi/Box/Vaccine_inequity/updated data/GE_Set_2.csv", row.names = FALSE)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Germany/COVID-19-Impfungen_in_Deutschland-master")

vac <- read_csv("Aktuell_Deutschland_Landkreise_COVID-19-Impfungen.csv") %>% 
  rename(location = 2, date = 1, dose= 4, vnum = 5) %>%
  group_by(date, location, dose) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  arrange(date) %>%
  group_by(location, dose) %>%
  mutate(vcum = cumsum(vnum)) %>%
  ungroup() %>% 
  mutate(location = as.integer(location))
vac <- inner_join(vac, pop, by = "location")

write.csv(vac,"C:/Users/sophi/Box/Vaccine_inequity/updated data/GE_Set_1.csv", row.names = FALSE)



