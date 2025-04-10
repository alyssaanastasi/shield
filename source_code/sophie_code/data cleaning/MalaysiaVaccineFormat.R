library(tidyverse)
library(readxl)
library(MMWRweek)

## home wd
setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Malaysia")

dataF <- read_csv("vax_state.csv")[c("date", "state", "cumul_partial", "cumul_full", "cumul_booster")] %>%
  rename(location = "state") %>%
  rename(partial = "cumul_partial") %>%
  rename(full = "cumul_full") %>%
  rename("3 or booster" = "cumul_booster") %>%
  pivot_longer(3:5, names_to = "dose", values_to = "vcum") %>%
  add_column(country = "Malaysia", .before = "location") %>%
  add_column(resolution = 1, .after = "country")
dataF <- dataF[c(2,3,4,1,5,6)]

dataPop <- read_csv("population.csv") %>%
  rename(location = 2) %>%
  rename(pop = 5) %>%
  filter(Year == 2020) %>%
  mutate(pop = pop*1000) %>%
  group_by(location) %>%
  summarise(pop = sum(pop, na.rm = T)) %>%
  ungroup()

dataFinal <- left_join(dataF, dataPop, by = "location")
dataFinal <- dataFinal[c(1,2,3,7,4,5,6)]

write.csv(dataFinal,"C:/Users/sophi/Box/Vaccine_inequity/updated data/My_Set_1.csv", row.names = FALSE)

