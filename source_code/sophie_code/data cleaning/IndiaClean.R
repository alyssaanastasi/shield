library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/India")

## some pops are duplicated, just average them since they are close
pop <- read_csv("IndiaPop.csv")[c(1,3)] %>% distinct() %>%
  group_by(region_iso) %>%
  mutate(population = mean(population)) %>% distinct()

old <- read_csv("IndiaVaccinations.csv")[c(1,2,3,5,7)] %>%
  rename(country = 1, location = 2, date = 3, partial = 5) %>%
  pivot_longer(5, names_to = "dose", values_to = "vcum")

getnames <- old[c(2,4)] %>% distinct()

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/India/time series")

## load data ####
fileNames <- list.files()
dataName <- tibble(x = fileNames) %>% 
  rowid_to_column("ID") %>%
  mutate(x = str_remove(x, ".xlsx"))

data1 <- tibble(NULL)
for(i in 1:length(fileNames)){
  temp <- read_excel(fileNames[i], sheet = "WeeklyVaccination") %>% 
    mutate(ID = i) %>%
    mutate(week = row_number()) %>%
    left_join(dataName)
  data1 <- data1 %>% bind_rows(temp)
}

data1$x[data1$x == "Andaman"] <- "Andaman and Nicobar Islands"
data1$x[data1$x == "Andhra"] <- "Andhra Pradesh"
data1$x[data1$x == "Arunachal"] <- "Arunachal Pradesh"
data1$x[data1$x == "Dadra"] <- "Dadra and Nagar Haveli"
data1$x[data1$x == "Daman"] <- "Daman and Diu"
data1$x[data1$x == "Himachal"] <- "Himachal Pradesh"
data1$x[data1$x == "Jammu"] <- "Jammu and Kashmir"
data1$x[data1$x == "Madhya"] <- "Madhya Pradesh"
data1$x[data1$x == "UttarPradesh"] <- "Uttar Pradesh"
data1$x[data1$x == "Chhattisargh"] <- "Chhattisgarh"


data1 <- data1 %>%
  rename(location = x, vnum = dose1) %>%
  mutate(dose = "partial", country = "India") %>%
  group_by(country, location, dose, week) %>%
  summarise(vnum = sum(vnum)) %>%
  ungroup() %>%
  arrange(week) %>%
  group_by(country, location, dose) %>%
  mutate(vcum = cumsum(vnum)) %>%
  ungroup()

data1 <- inner_join(data1, getnames, by = "location") %>%
  inner_join(pop, by = "region_iso") %>%
  mutate(region_iso = NULL, vperc = 100*vcum/population)


setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/India")  

Set2 <- read_excel("povertyclean.xlsx") %>%
  rename(poverty = 2) %>%
  pivot_longer(2, names_to = "measure", values_to = "value")

write.csv(data1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/IN_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/IN_Set_2.csv", row.names = FALSE)
