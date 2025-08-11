library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/Data/final/Cape Verde")
## https://en.wikipedia.org/wiki/List_of_cities_and_towns_in_Cape_Verde

Set1 <- read_excel("time series.xlsx")[c(1,2,3)] %>%
  rename(date = 1, location = 2, vcum = 3) %>%
  mutate(dose = "partial") %>% arrange(location) %>%
  filter(location != "Cabo Verde")

Set1$location[Set1$location == "Tarrafal de S?o Nicolau"] <- "Tarrafal de SN"
Set1$location[Set1$location == "Tarrafal de Santiago"] <- "Tarrafal de ST"
Set1$location[Set1$location == "Pa?l"] <- "Paul"
Set1$location[Set1$location == "S?o Miguel"] <- "S Miguel"
Set1$location[Set1$location == "S?o. Salvador do. Mundo"] <- "S Salvador do Mundo"
Set1$location[Set1$location == "S?o Salvador do Mundo"] <- "S Salvador do Mundo"
Set1$location[Set1$location == "S?o Salvador de Mundo"] <- "S Salvador do Mundo"
Set1$location[Set1$location == "Ribeira Grande S. Ant?o"] <- "RGSA"
Set1$location[Set1$location == "Ribeira Grande de Santo Ant?o"] <- "RGSA"
Set1$location[Set1$location == "Ribeira Grande de S. Ant?o"] <- "RGSA"
Set1$location[Set1$location == "Ribeira Grande de S?o Ant?o"] <- "RGSA"
Set1$location[Set1$location == "Ribeira Grande Santo Ant?o"] <- "RGSA"
Set1$location[Set1$location == "Ribeira Grande Santiago"] <- "RG ST"
Set1$location[Set1$location == "Ribeira Grande de Santiago"] <- "RG ST"
Set1$location[Set1$location == "S?o Vicente"] <- "S Vicente"
Set1$location[Set1$location == "Santa Catarina de FG"] <- "S Catarina FG"
Set1$location[Set1$location == "Santa  Catarina de Fogo"] <- "Santa Catarina FG"
Set1$location[Set1$location == "Santa Catarina de Fogo"] <- "Santa Catarina FG"
Set1$location[Set1$location == "Santa Catarina do Fogo"] <- "Santa Catarina FG"
Set1$location[Set1$location == "S?o  Domingos"] <- "S Domingos"
Set1$location[Set1$location == "S?o Domingos"] <- "S Domingos"
Set1$location[Set1$location == "S?o  Miguel"] <- "S Miguel"
Set1$location[Set1$location == "Santa Catarina de  Santiago"] <- "S Catarina ST"
Set1$location[Set1$location == "Santa Catarina Santiago"] <- "S Catarina ST"
Set1$location[Set1$location == "Tarrafal de S?o  Nicolau"] <- "Tarrafal de SN"
Set1$location[Set1$location == "Tarrafal de SN"] <- "Tarrafal SN"
Set1$location[Set1$location == "Ribeira Brava"] <- "R Brava"
Set1$location[Set1$location == "S?o Filipe"] <- "S Filipe"
Set1$location[Set1$location == "Santa  Catarina de Santiago"] <- "Santa Catarina ST"
Set1$location[Set1$location == "Santa Catarina de Santiago"] <- "Santa Catarina ST"
Set1$location[Set1$location == "Tarrafal Santiago"] <- "Tarrafal ST"
Set1$location[Set1$location == "Tarrafal de ST"] <- "Tarrafal ST"
Set1$location[Set1$location == "?rg?os"] <- "S?o Louren?o dos ?rg?os"


## test shows no repeats on each date, so the names are correctly merged
test <- Set1 %>% group_by(location, date) %>% filter(vcum != max(vcum)) %>% ungroup()

Set2 <- read_excel("SES.xlsx") %>%
  rename(location = 1, value = 2, pop = 3) %>%
  mutate(measure = "poverty")

## page 21 gives a key to which islands correspond to each city
Set2$location[Set2$location == "Ribeira Grande de Santiago"] <- "RG ST"
Set2$location[Set2$location == "Ribeira Grande"] <- "RGSA"
Set2$location[Set2$location == "Ribeira Brava"] <- "R Brava"
Set2$location[Set2$location == "Santa Catarina do Fogo"] <- "Santa Catarina FG"
Set2$location[Set2$location == "Santa Catarina"] <- "Santa Catarina ST"
Set2$location[Set2$location == "Tarrafal"] <- "Tarrafal ST"
Set2$location[Set2$location == "Tarrafal de S Nicolau"] <- "Tarrafal SN"
Set2$location[Set2$location == "S Lourenco dos Orgaos"] <- "S?o Louren?o dos ?rg?os"

names1 <- Set1 %>% select(location) %>% distinct() %>% arrange(location)
names2 <- Set2 %>% select(location) %>% distinct() %>% arrange(location)

test <- inner_join(Set1, Set2) %>% select(location) %>% distinct()

## done matching names

Set <- inner_join(Set1, Set2)

name <- Set %>% select(location) %>% distinct()

testing <- Set %>% filter(location == "Maio")

## THERE IS AN ANOMOLY ON DEC 5
## filter out this date

test <- Set %>% 
  filter(date != as.Date("2021-12-05")) %>%
  ggplot(aes(x = date, y = vcum/pop, group = location)) + geom_line()
test

Set <- Set %>%
  filter(date != as.Date("2021-12-05"))

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/CV_Set.csv", row.names = FALSE)
