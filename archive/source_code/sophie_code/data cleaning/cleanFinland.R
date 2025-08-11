library(tidyverse)
library(readxl)

###### Southwest Finland #####
SouthOstro <- read_csv("South Ostrobothnia Hospital District.csv")%>%
  mutate(location = "South Ostrobothnia Hospital District") 
Lansi <- read_csv("Lansi-Pohja Hospital District.csv")%>%
  mutate(location = "Lansi-Pohja Hospital District") 
NorthKar <- read_csv("North Karelia Hospital District.csv")%>%
  mutate(location = "North Karelia Hospital District")
SWFin <- read_csv("Southwest Finland Hospital District.csv")%>%
  mutate(location = "Southwest Finland Hospital District") 
Vaasa <- read_csv("Vaasa Hospital District.csv") %>%
  mutate(location = "Vaasa Hospital District")
Satakunta <- read_csv("Satakunta Hospital District.csv") %>%
  mutate(location = "Satakunta Hospital District")
Paijat <- read_csv("Paijat-Hame Hospital District.csv") %>%
  mutate(location = "Paijat-Hame Hospital District")
NorthSavo <- read_csv("North Savo Hospital District.csv") %>%
  mutate(location = "North Savo Hospital District")
NorthOstro <- read_csv("North Ostrobothnia Hospital District.csv") %>%
  mutate(location = "North Ostrobothnia Hospital District")
Pirkanmaa <- read_csv("Pirkanmaa Hospital District.csv") %>%
  mutate(location = "Prikanmaa Hospital District")
Lappi <- read_csv("Lappi Hospital District.csv") %>%
  mutate(location = "Lappi Hospital District")
Kymenlaakso <- read_csv("Kymenlaakso Hospital District.csv") %>%
  mutate(location = "Kymenlaakso Hospital District")
CentralFin <- read_csv("Central Finland Hospital District.csv") %>%
  mutate(location = "Central Finland Hospital District")
CentralOstro <- read_csv("Central Ostrobothnia Hospital District.csv") %>%
  mutate(location = "Central Ostrobothnia Hospital District")
Kanta <- read_csv("Kanta-Hame Hospital District.csv") %>%
  mutate(location = "Kanta-Hame Hospital District")
Kainuu <- read_csv("Kainuu Hospital District.csv") %>%
  mutate(location = "Kainuu Hospital District")
ItaSavo <- read_csv("Ita-Savo Hospital District.csv") %>%
  mutate(location = "Ita-Savo Hospital District")
Helsinki <- read_csv("Helsinki and Uusimaa Hospital District.csv") %>%
  mutate(location = "Helsinki and Uusimaa Hospital District")
SouthSavo <- read_csv("South Savo Hospital District.csv") %>%
  mutate(location = "South Savo Hospital District")
SouthKar <- read_csv("South Karelia Hospital District.csv") %>%
  mutate(location = "South Karelia Hospital District")
Aland <- read_csv("Aland.csv") %>%
  mutate(location = "Aland")

Set1 <- SWFin %>%
  rbind(Vaasa) %>%
  rbind(Satakunta) %>%
  rbind(Paijat) %>%
  rbind(NorthSavo) %>%
  rbind(NorthOstro) %>%
  rbind(Pirkanmaa) %>%
  rbind(Lappi) %>%
  rbind(Kymenlaakso) %>%
  rbind(CentralFin) %>%
  rbind(CentralOstro) %>%
  rbind(Kanta) %>%
  rbind(Kainuu) %>%
  rbind(ItaSavo) %>%
  rbind(Helsinki) %>%
  rbind(SouthSavo) %>%
  rbind(SouthKar) %>%
  rbind(Aland) %>%
  rbind(Lansi) %>%
  rbind(SouthOstro) %>%
  rbind(NorthKar)

## vaccination started around Dec 27, which is when the data starts (\pm 1 week). https://www.statista.com/statistics/1200114/number-of-coronavirus-vaccinations-by-region-finland/
Set1 <- Set1 %>%
  filter(Age == "18+" | Age == "-17") %>%
  separate(col = Time, sep = " ", into = c("a", "year", "b", "week")) %>%
  mutate(a = NULL, b = NULL, week = as.numeric(week), year = as.numeric(year)) %>%
  mutate(week = ifelse(year == 2020, week - 52, week), 
         week = ifelse(year == 2021, week + 53 - 52, week), 
         week = ifelse(year == 2022, week + 105 - 52, week
  )) %>%
  group_by(week, location)  %>%
  summarise(vnum = sum(val, na.rm =T)) %>%
  ungroup() %>%
  arrange(week) %>%
  group_by(location) %>%
  mutate(vcum = cumsum(vnum)) %>%
  ungroup() %>%
  filter(!is.na(week)) %>%
  mutate(location = tolower(location))

pop <- read_excel("pop.xls")[c(3,6)] %>%
  ## convert names of locations to the form in the vaccine data
  mutate(location = gsub("ä","a", location)) %>%
  mutate(location = gsub("Å", "a", location)) %>%
  mutate(location = gsub("Lapland", "Lappi", location)) %>% 
  mutate(location = gsub("aland hospital district", "aland", location)) %>%
  mutate(location = tolower(location)) %>%
  mutate(location = gsub("pirkanmaa", "prikanmaa", location))
  
  

Set1 <- Set1 %>% left_join(pop) %>%
  rename(pop = 5) %>%
  group_by(week,location) %>%
  summarise(vperc = 100*vcum/pop) %>%
  ungroup() %>%
  mutate(country = "Finland", dose = "partial")

Set2 <- read_excel("SES.xls")[c(1,4)] %>%
  rename(location = 1, socasst = 2) %>%
  ## convert names of locations to the form in the vaccine data
  mutate(location = gsub("ä","a", location)) %>%
  mutate(location = gsub("Å", "a", location)) %>%
  mutate(location = gsub("Lapland", "Lappi", location)) %>% 
  mutate(location = gsub("aland hospital district", "aland", location)) %>%
  mutate(location = tolower(location)) %>%
  mutate(location = gsub("pirkanmaa", "prikanmaa", location)) %>%
  rename(value = socasst) %>%
  mutate(measure = "value")

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/FI_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/FI_Set_2.csv", row.names = FALSE)

test <- left_join(Set2, Set1)%>% filter(!is.na(vperc)) %>% 
  select(location, value) %>% distinct()
