library(tidyverse)

dataFr1 <- read_csv("FR_Set_1.csv") %>%
  filter(vaccine == "total", dose == "partial") %>%
  group_by(country, resolution, pop, location, date, dose) %>%
  summarise(vsum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  arrange(date) %>%
  group_by(location) %>%
  mutate(vcum = cumsum(vsum)) %>%
  ungroup() %>%
  mutate(vperc = vcum/pop)

dataFr2 <- read_csv("Fr_Set_2.csv")



## this code counts how many locations do we have vaccine data and SES measures

distinctFr <- dataFr2 %>%
  inner_join(dataFr1) %>%
  filter(!is.na(vperc), !is.na(value)) %>%
  filter(dose == "partial") %>%
  select(country, location, resolution, measure) %>%
  distinct() %>%
  count(country, resolution, measure)

## France has n = 98 for both measures. 
