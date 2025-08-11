library(tidyverse)
library(viridis)
library(lubridate)

## load data ####

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/FormattedVaccine")

dataUS1 <- read_csv("US_Set_1.csv")

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/FormattedSES")

dataUS2 <- read_csv("US_Set_2.csv")

## getting rid of dips ####
dataUS1 <- dataUS1 %>%
  group_by(location, dose) %>%
  arrange(date) %>%
  mutate(dateP = if_else(vperc > lead(vperc), date, NA_Date_)) %>%
  mutate(datePF = if_else(all(is.na(dateP)), max(date, na.rm = T), min(dateP, na.rm = T))) %>%
  filter(date <= datePF) %>%
  ungroup() 
## na.rm warning in so many groups?


## aggregate by week ####

dataUS1 <- dataUS1 %>%
  mutate(week = isoweek(date)) %>%
  mutate(year = isoyear(date)) %>%
  group_by(year, week, location, dose, state) %>%
  ## if everything is NA for this week/location group, return NA, otherwise return the max
  summarise(vperc = ifelse(all(is.na(vperc)), NA, max(vperc, na.rm= T))) %>%
  ungroup()

dataUS1 <- dataUS1 %>%
  filter(state == "CO")
  


## test graph ####
dataUSedu2 <- dataUS2[c(1,4,5)] %>%
  filter(measure == "edu2") %>%
  distinct()
plotUSgr2edu <- left_join(dataT, dataUSedu2, by = "location") %>%
  filter(dose == "partial") %>%
  filter(!is.na(value)) %>%
  filter(year == 2021) %>%
  mutate(bin = cut(value, breaks = 3, na.rm = T)) %>%
  ggplot(aes(x = week, y = vperc, group = bin)) + geom_smooth(aes(color = bin)) + scale_colour_viridis_d(option = "viridis", name = "edu2") + facet_wrap(~state)+ ylim(0, 100) + xlim(0,53)
plotUSgr2edu

## Percent of adults with a bachelor's degree or higher, 2015-19




