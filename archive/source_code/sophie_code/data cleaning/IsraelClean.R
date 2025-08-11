library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Israel")

Set1 <- read_csv("vaccine.csv")[c("town_code", "date", "accumulated_vaccination_first_dose")] %>%
  rename(location = town_code, vcum = 3) %>%
  group_by(location, date) %>%
  mutate(vcum = as.numeric(vcum)) %>%
  summarise(vcum = sum(vcum, na.rm = T)) %>% 
  ungroup()

Set2 <- read_excel("wages.xlsx")[c(1,3)] %>%
  rename(location = 1, value = 2) %>% mutate(measure = "income")

## error is from hebrew that means "living outside localities"
pop <- read_excel("pop.xlsx") %>%
  rename(location = Code)

Set <- inner_join(Set1, Set2, by = "location")
Set <- inner_join(Set, pop, by = "location")
## all 77 municipalities

########## debugging ############
test <- Set %>% mutate(vperc = 100*vcum/pop) %>% group_by(location) %>% 
  filter(max(vperc,na.rm=T)<10)%>% ungroup() %>%
  select(location) %>% distinct()

##names <- Set %>%
 ## mutate(vperc = 100*vcum/pop) %>%
##  inner_join(test) %>%
##  filter(location == "70") %>%
##  filter(date == as.Date("2021-01-03"))
##  group_by(date) %>% 
##  filter(vperc != max(vperc)) %>%
##  ungroup() %>%
##  ggplot() + geom_line(aes(x = date, y = vperc, group = location))  
##names

plot <- Set %>%
  mutate(vperc = 100*vcum/pop) %>%
  ggplot() + 
  geom_line(aes(x = date, y = vperc, group = location))  
plot

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/IS_Set_1.csv", row.names = FALSE)
