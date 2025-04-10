library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Argentina")

pop <- read_csv("population.csv") %>%
  rename(code = 1, pop = 3) %>% mutate(date = NULL) %>% distinct()

Set1 <- read_csv("ArgentinaVaccination.csv")[c("region", "date", "region_iso", "people_vaccinated")] %>%
  rename(location = 1, code = 3, vcum = 4) %>%
  left_join(pop) %>%
  mutate(location = tolower(location))

## also called capital federal according to wikipedia, and appears this way in the SES data
Set1$location[Set1$location == "ciudad autonoma de buenos aires"] <- "capital federal"

Set <- read_excel("SES clean.xlsx") %>%
  mutate(location = tolower(location)) %>%
  inner_join(Set1) %>%
  rename(value = 2) %>%
  mutate(measure = "psincome") %>%
  mutate(code = NULL)

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/AR_Set.csv", row.names = FALSE)
