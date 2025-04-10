library(tidyverse)
library(readxl)
library(MMWRweek)

## home wd
setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Malaysia")

dataF <- read_csv("poverty.csv") %>%
  filter(Year == "2012") %>%
  mutate(Year = NULL) %>%
  rename(location = State) %>%
  rename(income6 = "Incidence of hardcore poverty") %>%
  pivot_longer(2, names_to = "measure", values_to = "value") %>%
  mutate(value = as.numeric(value))

write.csv(dataF,"C:/Users/sophi/Box/Vaccine_inequity/updated data/My_Set_2.csv", row.names = FALSE)
