library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Lebanon")

pop <- read_csv("pop.csv") %>%
  rename(code = 1, pop = 3) %>% mutate(date = NULL) %>% distinct()

Set1 <- read_csv("LebanonVaccine.csv")[c("region", "date", "region_iso", "total_vaccinations")] %>%
  rename(location = 1, code = 3, vcum = 4) %>%
  left_join(pop) 
## not at 50% vaccinated
