library(tidyverse)
library(readxl)

Set1 <- read_csv("opendata_covid19_vaccination_location_county.csv")[c("StatisticsDate", "LocationCounty", "VaccinationSeries", "MeasurementType", "PopulationCoverage")] %>%
  filter(VaccinationSeries == 1 & MeasurementType == "Vaccinated") %>%
  mutate(MeasurementType = NULL) %>%
  rename(dose = VaccinationSeries) %>%
  mutate(location = gsub(" maakond", "", LocationCounty), LocationCounty = NULL) %>%
  rename(vperc = PopulationCoverage, date = StatisticsDate) %>%
  add_column(country = "Estonia")
  
  

Set2 <- read_excel("income.xlsx") %>%
  rename(location = 1, value = 2) %>%
  add_column(measure = "2020income") %>%
  mutate(location = gsub(" county", "", location))

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/ES_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/ES_Set_2.csv", row.names = FALSE)
