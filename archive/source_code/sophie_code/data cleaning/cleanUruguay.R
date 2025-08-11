library(tidyverse)
library(readxl)


pop <- read_excel("pop.xlsx")
Set1 <- read_csv("vaccination.csv")[c("region", "date", "people_vaccinated")] %>%
  rename(location = 1, vcum = 3) %>%
  left_join(pop) %>%
  mutate(vperc = 100*vcum/pop) %>%
  add_column(dose = "partial")

Set2 <- read_excel("SES.xlsx") %>%
  rename(value = 2) %>%
  mutate(measure = "MHI")

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/UR_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/UR_Set_2.csv", row.names = FALSE)
