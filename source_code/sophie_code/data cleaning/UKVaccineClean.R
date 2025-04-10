library(tidyverse)

## loading and formatting data ####
dataF <- read_csv("localauthorityvaccines.csv")[c(1,4,5,6,7)] %>%
  rename(location = areaCode) %>%
  rename(partial = "cumVaccinationFirstDoseUptakeByVaccinationDatePercentage") %>%
  rename(full = "cumVaccinationSecondDoseUptakeByVaccinationDatePercentage") %>%
  rename("3 or booster" = "cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage") %>%
  pivot_longer(3:5, names_to = "dose", values_to = "vperc") %>%
  add_column(resolution = "local authority", .before = "location") %>%
  add_column(country = "UK", .before = "resolution") %>%
  add_column(pop = NA, .after = "location") %>%
  add_column(vaccine = NA, .after = "dose") %>%
  add_column(vnum = NA, .after = "vaccine") %>%
  add_column(vcum = NA, .after = "vnum") 

write.csv(dataF,"C:/Users/sophi/Box/Vaccine_inequity/updated data/FormattedVaccine/UK_Set_1.csv", row.names = FALSE)


