library(tidyverse)
library(readxl)

Set1 <- read_csv("cantones.csv")[c(5,6,8,14)] %>%
  rename(location = canton, date = 4, pop = 2, vcum = 3) %>%
  mutate(location = gsub("í", "i", location)) %>%
  mutate(location = gsub("ó", "o", location)) %>%
  mutate(location = gsub("á", "a", location)) %>%
  mutate(location = gsub("ñ", "n", location)) %>%
  mutate(location = gsub("ú", "u", location)) %>%
  mutate(location = gsub("é", "e", location))

Set1$location[Set1$location == "Banos"] <- "Banos de Augua Santa"
Set1$location[Set1$location == "San Jacinto De Yaguachi"] <- "San Jacinto de Yaguachi"
Set1$location[Set1$location == "Pablo Sexto"] <- "Pablo VI"
Set1$location[Set1$location == "Veinticuatro de Mayo"] <- "24 de Mayo"


Set2 <- read_excel("povertybycanton.xlsx") %>%
  rename(value = poverty) %>%
  mutate(measure = "poverty")

Set <- left_join(Set1, Set2)

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/EC_canton.csv", row.names = FALSE)

