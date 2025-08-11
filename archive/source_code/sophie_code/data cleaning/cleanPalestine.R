library(tidyverse)
library(readxl)

##Jerusalem suburbs was changed to Jerusalem since the link goes to Jerusalem governorate
Set1 <- read_excel("vaccinationClean.xlsx") %>%
  rename(vperc = 2, location = 1) %>%
  ## reads as a proportion rather than percent integer 
  mutate(vperc = 100*as.numeric(vperc), dose = "partial")


Set2 <- read_excel("wages2020.xlsx") %>%
  rename(location = 2, value = 1) %>%
  mutate(measure = "income")
Set2$location[Set2$location == "Jericho and Al Aghwar"] <- "Jericho"
Set2$location[Set2$location == "Ramallah & Al-Bireh"] <- "Ramallah And Al-Bireh"
Set2$location[Set2$location == "Tubas & Northern Valleys"] <- "Tubas"

Set1$location[Set1$location == "Qalqilya"] <- "Qalqiliya"


test <- inner_join(Set1,Set2) %>% select(location) %>% distinct()
test <- Set1 %>% ggplot(aes(x = date, y = vperc, group = location)) + geom_line()
test

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/Pal_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/Pal_Set_2.csv", row.names = FALSE)
