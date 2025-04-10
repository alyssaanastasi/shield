library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Kazakhstan")

Set1 <- read_excel("Vaccine time series.xlsx")[c(1,2,4)] %>%
  rename(vcum = 2)

Set1$location[Set1$location == "West-Kazakhstan region"] <- "West Kazakhstan region"
Set1$location[Set1$location == "Alma-Ata's region"] <- "Almaty region"
Set1$location[Set1$location == "The East Kazakhstan region"] <- "East Kazakhstan region"
Set1$location[Set1$location == "North-Kazakhstan region"] <- "North Kazakhstan region"
Set1$location[Set1$location == "Jambyl Region"] <- "Jambyl region"
Set1$location[Set1$location == "Kyzylorda Region"] <- "Kyzylorda region"
Set1$location[Set1$location == "Mangistau region"] <- "Mangystau region"

## these are the same region
Set1$location[Set1$location == "Turkestan region"] <- "South Kazakhstan region"


Set2 <- read_csv("SES.csv")[c("year", "poor", "region", "population")] %>%
  filter(year == 2014) %>% rename(location = region) %>% mutate(year = NULL) %>%
  rename(value = poor) %>% mutate(measure = "poverty")

Set <- inner_join(Set1, Set2, by = "location")

test <- Set %>% mutate(vperc = 100*vcum/population) %>% arrange(date)
plot <- test %>% ggplot() + geom_line(aes(x = date, y = vperc, group = location)) 
plot

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/Kaz_Set.csv", row.names = FALSE)
  

##test <- left_join(Set1, Set2, by = "location") %>% filter(is.na(poor)) %>% select(location) %>% distinct()



