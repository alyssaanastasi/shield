library(tidyverse)
library(readxl)

## Vaccine
setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Denmark/vaccinationsdata-dashboard-covid19-22062022-mkim/Vaccine_DB")

Set1 <- read.csv("Vaccine_dato_kommune.csv", sep = ";")[c(1,2,12)] %>%
  rename(location = 2, date = 1, vperc = 3)

## SES
setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Denmark")

Set2 <- read_excel("SES.xlsx")[c(4,5)] %>%
  rename(location = 1, value = 2) %>%
  mutate(measure = "income")

Set2$location[Set2$location == "Aarhus"] <- "Århus"
Set2$location[Set2$location == "Høje-Taastrup"] <- "Høje Tåstrup"
Set2$location[Set2$location == "Copenhagen"] <- "København"
Set2$location[Set2$location == "Nordfyns"] <- "Nordfyn"
Set2$location[Set2$location == "Vesthimmerlands"] <- "Vesthimmerland"


Set <- inner_join(Set1, Set2)

## all names work
names1 <- Set1 %>% select(location) %>% distinct() %>% arrange(location)
names2 <- Set2 %>% select(location) %>% distinct() %>% arrange(location)
names <- Set %>% select(location) %>% distinct() %>% arrange(location) %>%
  mutate(x = 1)

check <- left_join(names1, names) %>% filter(is.na(x))

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/DK_Set.csv", row.names = FALSE)


