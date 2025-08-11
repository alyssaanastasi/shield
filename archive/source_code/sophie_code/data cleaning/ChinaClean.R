library(tidyverse)
library(readxl)
library(padr)


setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/China")

SES <- read_excel("SES.xlsx")[c(1,3,4)] %>%
  rename(location = 1, value = 3) %>%
  mutate(measure = "income")

Set1 <- read_excel("no. of vaccinated doses (1).xlsx", sheet =4)[c("province", "People_vaccinated_1 dose","Date")] %>%
  rename(location = province, vcum = 2, date = 3)
  
Set1 <- Set1 %>% 
  pad(interval = "day", group = "location") %>%
  group_by(location) %>%
  fill(vcum) %>%
  ungroup()

Set <- inner_join(Set1, SES)

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/China.csv", row.names = FALSE)


################## testing ############

plot <- Set %>% mutate(vperc = 100*vcum/pop) %>% 
  ggplot() + geom_line(aes(x = date, y = vperc, group = location))
plot


## we have >20 locations
Set <- Set %>%
  ## income
  mutate(nbin = ntile(x= value, n = 5)) %>%
  mutate(cbin = cut_interval(value, n = 5, labels = c("L",2,3,4,"H"), na.rm = T))

Set$nbin[Set$nbin == 5] <- "H"
Set$nbin[Set$nbin == 1] <- "L"

Set <- Set %>% 
  mutate(vperc = 100*vcum/pop) %>% 
  group_by(date, nbin) %>% 
  summarise(avg = mean(vperc)) %>%
  ungroup()

plot <- Set %>% 
  filter(nbin == "H" | nbin == "L") %>%
  ggplot() + geom_line(aes(x = date, y = avg, group = nbin, color = nbin))
plot




