library(tidyverse)
library(readxl)

pop <- read_csv("bfa_adm1_pop.csv")[c(4,8)] %>%
  rename(location = 1, pop = 2) 

Set1 <- read_csv("CleanedVaccine.csv")[c(2,6,9)] %>%
  slice(-1) %>%
  mutate(date = as.Date(DATE, "%d/%m/%Y"), DATE = NULL) %>%
  rename(location = 1, partial = 2) %>%
  pivot_longer(2, names_to = "dose", values_to = "vcum") %>%
  mutate(vcum = gsub("\U202F", "", vcum))
Set1 <- inner_join(Set1, pop, by = "location") %>%
  mutate(vcum = as.numeric(vcum)) %>%
  mutate(vperc = 100*vcum/pop) %>%
  add_column(country = "Burkina Faso", .before = "location")

Set2 <- read_excel("CleanedSES.xlsx") %>%
  rename(location = 1, poverty = 2) %>%
  pivot_longer(2, names_to = "measure", values_to = "value")

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/BF_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/BF_Set_2.csv", row.names = FALSE)
