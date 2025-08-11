library(tidyverse)
library(readxl)

Set1 <- read_excel("TaiwanVaccineSeries.xlsx")[c(1,2,4)] %>%
  rename(date = 1, partial = 3) %>%
  pivot_longer(3, names_to = "dose", values_to = "vperc") %>%
  add_column(country = "Taiwan", .before = "location")

Set1$location[Set1$location == "City of Keelung"] <- "Keelung City"
  

Set2 <- read_excel("SES.xlsx") %>% 
  rename(income = 2) %>%
  pivot_longer(2, names_to = "measure", values_to = "value")

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/TW_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/TW_Set_2.csv", row.names = FALSE)
