library(tidyverse)

Set <- read_csv("BR_TEMP_Set.csv")[c("date","state","areaname","areacode","DOSE","COUNT","poverty_percentage", "population")] %>%
  filter(DOSE == 1) %>%
  mutate(DOSE = NULL) %>%
  group_by(date, areaname, areacode, state, population) %>%
  summarise(vnum = sum(COUNT, na.rm = T)) %>%
  ungroup() %>%
  arrange(date) %>% 
  group_by(areaname, areacode, state) %>%
  mutate(vcum = cumsum(vnum)) %>%
  ungroup() %>%
  mutate(vperc = 100*vcum/population)

Set %>% ggplot() + geom_line(aes(x = date, y = vperc, group = interaction(areaname,areacode)))

date <- Set %>% mutate(mindate = min(date, na.rm = T))

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Brazil/brazil All files used")
Set1 <- read.csv("SESRS_VAC_APLICACAO_23_03_2022-07_33.csv", sep=";")

count <- Set1 %>% select(CD_MUNICIPIO) %>% distinct()

Set1 <- Set1 %>% rename(dose = DOSE, date = DATA, location = CD_MUNICIPIO, count = COUNT) %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  group_by(date, location, dose) %>%
  summarise(vnum = sum(count, na.rm = T)) %>%
  ungroup() %>%
  arrange(date) %>%
  group_by(location, dose) %>%
  mutate(vcum = cumsum(vnum)) %>%
  ungroup()



setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data")
Set <- read_csv("BR_TEMP_Set.csv")[c("date","state","areaname","areacode","DOSE","COUNT","poverty_percentage", "population")]

Set <- Set[c(4,7,8)] %>%
  rename(location = areacode)
Set <- Set %>% distinct() %>% arrange(location) %>% mutate(location = as.character(location))


Set1 <- Set1 %>% mutate(location = as.character(location))
FullSet <- inner_join(Set1, Set, by = "location")

plot <- FullSet %>% 
  filter(dose == 1) %>% 
  ggplot() + geom_line(aes(x = date, y = vcum/population, group = location, color = poverty_percentage))
plot


write.csv(FullSet,"C:/Users/sophi/Box/Vaccine_inequity/updated data/BR_Full_Set.csv", row.names = FALSE)


