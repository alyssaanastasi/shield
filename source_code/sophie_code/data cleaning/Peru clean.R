library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Peru")

centers <- read_csv("TB_CENTRO_VACUNACION (1).csv")[c("id_centro_vacunacion", "id_ubigeo")] %>%
  arrange(id_ubigeo)

## we check CHISQUILLA to see that they are using ubigeo inei codes, not reniec
codes <- read_csv("TB_UBIGEOS.csv")[c("id_ubigeo", "ubigeo_inei")]

Set1 <- read_csv("TB_VACUNACION_COVID19.csv.")
  
  test <- Set1 %>% filter(dosis == 1) 
  
  %>%
  distinct() %>%
summarise("fecha_vacunacion", "id_centro_vacunacion", "dosis") %>%
   mutate(count = as.numeric("1")) %>%  mutate(year = substring(fecha_vacunacion,1,4), month = substr(fecha_vacunacion, 5,6), 
         day = substr(fecha_vacunacion, 7,8)) %>%
  mutate(date = paste(year, month, day, sep = "-"))

## make date
Set1 <- Set1[c("id_centro_vacunacion", "dosis", "count", "date")] %>%
  mutate(date = as.Date(date, "%Y-%m-%d"))

## join codes
Setfull <- left_join(Set1, centers) %>%
  left_join(codes) %>%
  rename(dose = dosis, location = 3) %>%
  mutate(location = as.numeric(location))

FinalSet <- read_excel("SES clean.xlsx") %>%
  rename(location = 1) %>%
  mutate(location = as.numeric(location)) %>%
  left_join(Setfull) %>%
  filter(!is.na(location)) %>%
  rename(pop = 4, value = 6) %>%
  mutate(measure = "poverty") %>%
  mutate(pop = as.numeric(gsub(",", "", pop))) %>%
  ## cumulative sum 
  group_by(dose, date, location, pop, value) %>%
  summarise(count = sum(count, na.rm = T)) %>%
  ungroup() %>%
  arrange(date) %>%
  group_by(dose, location) %>%
  mutate(vcum = cumsum(count)) %>%
  ungroup() %>%
  mutate(vperc = 100*vcum/pop)

test <- FinalSet %>% filter(vperc > 100)
  

