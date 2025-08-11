library(tidyverse)
library(readxl)

Set1 <- read_csv("vaccinations.csv")[c("date", "geoRegion", "type", "sumTotal","pop")] %>%
  filter(type == "COVID19AtLeastOneDosePersons") %>%
  rename(location = "geoRegion") %>%
  mutate(type = NULL, dose = "partial") %>%
  group_by(date, location, dose,pop) %>% 
  summarise(vcum = sum(sumTotal, na.rm = T)) %>%
  ungroup() %>%
  mutate(vperc = 100*vcum/pop) %>% 
  mutate(pop = NULL)


pop <- read_csv("vaccinations.csv")[c("geoRegion", "pop")] %>%
  rename(location = geoRegion)

Set2 <- read_csv("SES.csv")[c("PERIOD_REF", "REGION", "INDICATORS", "VALUE")] %>%
  filter(INDICATORS == "Ind_06_01" | INDICATORS == "Ind_11_01") %>%
  rename(location = 2, value = VALUE) %>%
  mutate(PERIOD_REF = NULL) %>%
  rename(measure = INDICATORS) %>%
  left_join(pop) %>%
  mutate(value = if_else(measure == "Ind_06_01", value/pop, value), date = NULL, pop = NULL, dose = NULL, vperc = NULL)

Set2$measure[Set2$measure == "Ind_11_01"] <- "Socasst"
Set2$measure[Set2$measure == "Ind_06_01"] <- "GDPpc"

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/SWI_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/SWI_Set_2.csv", row.names = FALSE)

check <- Set1[c("location")] %>% distinct()