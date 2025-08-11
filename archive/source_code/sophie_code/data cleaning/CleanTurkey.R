library(tidyverse)
library(readxl)

pop <- read_csv("TurkeyPop.csv")[c(1,3)] %>%
  distinct()

Set1 <- read_csv("TurkeyVaccineData.csv")[c(1,2,3,5,6,7,8)] %>%
  left_join(pop, by = "region_iso") %>%
  mutate(region_iso = NULL) %>%
  rename(deliveries = "total_vaccinations", 
         partial = "people_vaccinated", 
         full = "people_fully_vaccinated", 
         country = "location", 
         location = "region") %>%
  pivot_longer(4:6, names_to = "dose", values_to = "vcum") %>%
  mutate(vperc = 100*vcum/population)

Set2 <- read_excel("GDPpercapita.xlsx")[c(3,4)] %>%
  rename(GDPpc = "gdp per capita", location = "cleaned province") %>%
  pivot_longer(2, names_to = "measure", values_to = "value") %>%
  filter(!is.na(location))

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/TR_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/TR_Set_2.csv", row.names = FALSE)


