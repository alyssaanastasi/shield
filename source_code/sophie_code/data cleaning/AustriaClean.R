library(tidyverse)
library(readxl)

Set1 <- read.csv("COVID19_vaccination_doses_timeline (1).csv", sep = ";")[c(1,3,5,6)] %>%
  rename(dose = dose_number, date = 1, location = 2, vcum = 4) %>%
  mutate(date = as.Date(substr(date, 1,10))) %>%
  ## summarise, since there are multiple entries for each day depending on vaccine given
  group_by(date, dose, location) %>%
  summarise(vcum = sum(vcum, na.rm = T)) %>%
  ungroup()

## mutate to english names with no special characters
Set1$location[Set1$location == "Kärnten"] <- "Carinthia"
Set1$location[Set1$location == "Niederösterreich"] <- "Lower Austria"
Set1$location[Set1$location == "Oberösterreich"] <- "Upper Austria"
Set1$location[Set1$location == "Steiermark"] <- "Styria"
Set1$location[Set1$location == "Tirol"] <- "Tyrol"
Set1$location[Set1$location == "Wien"] <- "Vienna"

Set2 <- read_csv("OECD_SES.csv")[c("Regions", "Value")] %>%
  rename(location = 1, value = 2) %>%
  mutate(measure = "income")

Set2$location[Set2$location == "Burgenland (AT)"] <- "Burgenland"


Set <- inner_join(Set1, Set2)

pop <- read.csv("Population (annual average) since 2016 by federal provinces (table).csv", sep= ";")[c(1,7)] %>%
  rename(location = 1, pop = 2)

Set <- inner_join(Set, pop)

test <- Set %>% select(location) %>% distinct()

test <- Set %>% filter(dose == 1) %>% ggplot() + geom_line(aes(x = date, y = vcum/pop, group = location))
test

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/Austria_Set.csv", row.names = FALSE)




