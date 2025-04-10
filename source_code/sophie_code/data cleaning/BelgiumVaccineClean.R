library(tidyverse)
library(readxl)
library(MMWRweek)

## home wd
setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Belgium")

## load and rename ####
dataF <- read_csv("municipal vaccine data.csv") %>%
  rename(location = "NIS5") %>%
  rename(vcum = "CUMUL") %>%
  rename(dose = "DOSE")

## change the doses to numbers ####
dataF$dose[dataF$dose == "A"] <- "partial"
dataF$dose[dataF$dose == "B"] <- "full"
dataF$dose[dataF$dose == "C"] <- "full"
dataF$dose[dataF$dose == "E"] <- "3 or booster"
  
## dropping age category and combine full vaccination category ####
## <10 is counted as NA = 0
dataF <- dataF %>%
  mutate(vcum = as.numeric(vcum)) %>%
  group_by(YEAR_WEEK, location, dose) %>%
  summarise(vcum = sum(vcum, na.rm = T)) %>%
  ungroup() 


## adding the extra columns ####
dataF <- dataF %>%
  add_column(country = "Belgium", .before = "YEAR_WEEK") %>%
  add_column(resolution = 4, .after = "country") %>%
  add_column(vnum = NA, .after = "dose") %>%
  add_column(vperc = NA, .after = "vcum")

## fixing year_week into a year, week ####
## pull week and year information from YEAR_WEEK
dataF <- dataF %>%
  mutate(year = substr(YEAR_WEEK, 1, 2)) %>%
  mutate(week = substr(YEAR_WEEK, 4,5)) %>%
  mutate(week = as.numeric(week))

## add 20 to the front of the year
dataF <- dataF %>%
  mutate(year = paste0("20", year))

## reorder and drop yearweek###
dataF <- dataF[c(1,2,4,9,10,5:8)]

## add population data ####
dataPop <- read_excel("municipal population.xlsx")[c(1,21)] %>%
  rename(location = "CD_REFNIS") %>%
  rename(pop = "MS_POPULATION") %>%
  group_by(location) %>%
  summarise(pop = sum(pop)) %>%
  ungroup() %>%
  mutate(location = as.numeric(location))

dataFinal <- left_join(dataF, dataPop, by = "location")[c(1:3,10,4:9)] %>%
  add_column(vaccine = NA, .before= "vnum")

write.csv(dataFinal,"C:/Users/sophi/Box/Vaccine_inequity/updated data/FormattedVaccine/Be_Set_1.csv", row.names = FALSE)


