library(tidyverse)
library(lubridate)

## load data ####
dataF <- read_csv("COVID-19_Vaccinations_in_the_United_States_County.csv")[c(1,2,5,11,20, 44)]

## format data ####
dataF <- dataF %>%
  rename(location = FIPS) %>%
  mutate(date = as.Date(Date, "%m/%d/%Y")) %>%
  mutate(Date = NULL)
dataF <- dataF[c(1,2,6,3,4,5)] %>%
  rename(full = "Series_Complete_18PlusPop_Pct") %>%
  rename(partial = "Administered_Dose1_Recip_18PlusPop_Pct") %>%
  rename("3 or booster" = "Booster_Doses_18Plus_Vax_Pct")
dataF <- dataF %>%
  pivot_longer(4:6, values_to = "vperc", names_to = "dose")
dataF <- dataF %>%
  add_column(country = "United States", .before = "location") %>%
  add_column(resolution = 2, .after = "country") %>%
  add_column(pop = NA, .after = "location") %>%
  add_column(vaccine = NA, .after = "dose") %>%
  add_column(vnum = NA, .after = "vaccine") %>%
  add_column(vcum = NA, .after = "vnum")
dataF <- dataF %>%
  rename(state = Recip_State)
dataF <- dataF[c(1,2,3,5,4,6:11)]

write.csv(dataF,"C:/Users/sophi/Box/Vaccine_inequity/updated data/FormattedVaccine/US_Set_1.csv", row.names = FALSE)

###### test code ##########

test <- dataF %>% 
  filter(state == "GA") %>% 
  filter(dose == "partial") %>%
  ggplot() + geom_line(aes(x = date, y = vperc, group = location))
test

## this data is wrong. Chattahoochee is the FIPS code with vaccination over 75%, but in reality they have around 28% 
## vaccinated in the county https://covidactnow.org/us/georgia-ga/county/chattahoochee_county/?s=35472334

test <- dataF %>% 
  filter(state == "GA") %>% 
  filter(dose == "partial") %>%
  group_by(location) %>%
  filter(max(vperc)>75) %>%
  ungroup()

## and this is flat out not possible for Texas

test <- dataF %>% 
  filter(state == "TX") %>% 
  filter(dose == "partial") %>%
  ggplot() + geom_line(aes(x = date, y = vperc, group = location))
test

## in WV, the data look like a step function

test <- dataF %>% 
  filter(state == "WV") %>% 
  filter(dose == "partial") %>%
  ggplot() + geom_line(aes(x = date, y = vperc, group = location))
test

## Alabama also has these weird steps
test <- dataF %>% 
filter(state == "AL") %>% 
  filter(dose == "partial") %>%
  ggplot() + geom_line(aes(x = date, y = vperc, group = location))
test

#### test code ####

dataF <- read_csv("COVID-19_Vaccinations_in_the_United_States_County.csv")[c(1,2,5,10,19,40)] %>%
  
dataT <- dataF %>%
  filter(Recip_State == "HI") %>%
  filter(FIPS == 15001) %>%
  mutate(date = as.Date(Date, "%m/%d/%Y")) %>%
  arrange(date)
plot1 <- dataT %>%
  ggplot(aes(x = date, y = "Administered_Dose1_Recip_18Plus")) + geom_line()
plot1
dataF <- dataF %>%
  rename(location = FIPS) %>%
  mutate(date = as.Date(Date, "%m/%d/%Y")) %>%
  mutate(Date = NULL) %>%
  rename(state = Recip_State)
dataF <- dataF[c(1,2,6,3,4,5)] %>%
  rename(full = "Series_Complete_18Plus") %>%
  rename(partial = "Administered_Dose1_Recip_18Plus") %>%
  rename("3 or booster" = "Booster_Doses_18Plus")
dataF <- dataF %>%
  pivot_longer(4:6, values_to = "vcum", names_to = "dose")
dataF <- dataF %>%
  add_column(country = "United States", .before = "location") %>%
  add_column(resolution = 2, .after = "country") %>%
  add_column(pop = NA, .after = "location") %>%
  add_column(vaccine = NA, .after = "dose") %>%
  add_column(vnum = NA, .after = "vaccine") %>%
  add_column(vperc = NA, .after = "vcum") %>%
  mutate(week = epiweek(date)) %>%
  mutate(year = isoyear(date)) %>%
  group_by(week, year, state, location, dose) %>%
  summarise(vcum = sum(vcum, na.rm = T))

dataH <- dataF 
dataT <- dataF %>%
  filter(state == "IL") %>%
  filter(location == "17129") %>%
  filter(dose == "full") %>%
  arrange(week)

plotUSstate <- dataT %>%
  filter(year == 2021) %>%
  ggplot(aes(x = week, y = vcum, group = location)) + geom_line(aes(color = location)) + scale_colour_viridis_d(option = "viridis", name = "state") 
plotUSstate


plotUSstate <- dataF %>%
  filter(!is.na(vcum)) %>%
  filter(state == "ID")%>%
  filter(dose == "partial") %>%
  filter(year == 2021) %>%
  ggplot(aes(x = week, y = vcum, group = location)) + geom_line(aes(color = location)) + scale_colour_viridis_d(option = "viridis", name = "state") 
plotUSstate


dataT <- dataF %>%
  filter(Recip_State == "AK")
