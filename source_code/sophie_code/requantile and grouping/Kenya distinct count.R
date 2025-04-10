library(tidyverse)

dataKE1 <- read_csv("KE_Set_1.csv")
dataKE2 <- read_csv("KE_Set_2.csv")

distinctKE <- dataKE2 %>%
  inner_join(dataKE1) %>%
  filter(!is.na(vperc), !is.na(value)) %>%
  filter(dose == "partial") %>%
  select(country, location, resolution, measure) %>%
  distinct() %>%
  count(country, resolution, measure)