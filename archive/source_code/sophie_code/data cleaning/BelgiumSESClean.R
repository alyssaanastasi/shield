library(tidyverse)
library(readxl)


## data is loaded, income normalized by number of residents
Income <- read_excel("municipal income data.xlsx")[c(1,2,5,30)] %>%
  filter(CD_YEAR == "2019") %>%
  mutate(CD_YEAR = NULL) %>%
  rename(location = "CD_MUNTY_REFNIS") %>%
  rename("income1" = "MS_TOT_NET_TAXABLE_INC") %>%
  rename(pop = "MS_TOT_RESIDENTS") %>%
  mutate(income1 = income1/pop) %>%
  mutate(pop = NULL) %>%
  add_column(country = "Belgium", .before = "location") %>%
  pivot_longer(cols = income1, names_to = "measure", values_to = "value")

write.csv(Income,"~/Library/CloudStorage/Box-Box/Vaccine_inequity/updated data/Be_Set_2.csv", row.names = FALSE)
