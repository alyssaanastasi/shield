library(corpus)
library(tidyverse)
library(readxl)

pop <- read_excel("cleanpopulation.xlsx")[c(2,5)] %>%
  rename(location = 1, pop = 2)

Set1 <- read_ndjson("prefecture.ndjson", mmap = FALSE, simplify = TRUE, text = NULL) %>%
  group_by(prefecture, date, status) %>%
  summarise(vnum = sum(count, na.rm = T)) %>%
  ungroup() %>%
  arrange(date) %>%
  group_by(prefecture, status) %>%
  mutate(vcum = cumsum(vnum)) %>%
  ungroup() %>%
  rename(location = prefecture, dose = status) %>% 
  add_column(country = "Japan") %>%
  mutate(location = as.numeric(location))
Set1 <- left_join(Set1, pop, by = "location") %>%
  mutate(vperc = 100*vcum/pop)

Set2 <- read_excel("cleanincome.xlsx") %>%
  pivot_longer(2:48, names_to = "location", values_to = "value")

Set2$measure[Set2$measure == "yearly income"] <- "income"

Set2 <- Set2[c(2,1,3)]

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/JP_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/JP_Set_2.csv", row.names = FALSE)
