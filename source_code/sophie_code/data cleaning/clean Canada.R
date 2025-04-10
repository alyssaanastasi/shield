library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Canada/Province level")
Set1 <- read_csv("vaccination-coverage-byAgeAndSex-overTimeDownload.csv")
pop <- read_excel("cleanpop.xlsx")%>%
  mutate(location = gsub(" 5", "", location))

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Canada/Province level/income data")
Set2 <- read_excel("cleanSES.xlsx") %>%
  rename(value = totalincome) %>%
  add_column(measure = "totalincome")
  

Set1 <- Set1[c("prename", "week_end", "sex", "age", "numtotal_atleast1dose")] %>%
  filter(sex == "All sexes") %>% 
  rename(date = "week_end", location = "prename", partial = 5) %>%
  filter(location != "Canada") %>%
  group_by(location, date) %>% 
  summarise(vcum = sum(as.numeric(partial), na.rm = T)) %>%
  ungroup() %>%
  arrange(date) %>% 
  add_column(dose = "partial") %>%
  left_join(pop)

## test 
Set1 %>% ggplot() + geom_line(aes(x = date, y = 100*vcum/pop, group = location))

locations <- Set1[c(1)] %>% distinct()

write.csv(Set1,"C:/Users/sophi/Box/Vaccine_inequity/updated data/CA_Set_1.csv", row.names = FALSE)
write.csv(Set2,"C:/Users/sophi/Box/Vaccine_inequity/updated data/CA_Set_2.csv", row.names = FALSE)



test<- read_csv("test.csv")
test<- read_csv("CA_Set_1.csv") 

plot <- test %>%
  ggplot() + geom_line(x = date, y = vcum, group = location) 
plot
