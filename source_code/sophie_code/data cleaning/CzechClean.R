library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Czech Republic")

pop <- read_csv("pop.csv") %>%
  rename(code = 1, pop = 3) %>% mutate(date = NULL) %>% distinct()

Set1 <- read_csv("CzechiaVaccination.csv")[c("region", "date", "region_iso", "people_vaccinated")] %>%
  rename(location = 1, code = 3, vcum = 4) %>%
  left_join(pop) %>%
  mutate(location = tolower(location)) %>%
  mutate(location = gsub(" kraj", "", location))

Set2 <- read_excel("Regional GDP.xlsx") %>%
  mutate(location = tolower(location)) %>%
  rename(value = 2) %>%
  mutate(measure = "GDPpc")

Set2$location[Set2$location == "prague"] <- "praha, hlavni mesto"
Set2$location[Set2$location == "vysocina"] <- "kraj vysocina"

Set <- inner_join(Set1, Set2) %>%
  mutate(code = NULL) 

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/CZ_Set.csv", row.names = FALSE)
