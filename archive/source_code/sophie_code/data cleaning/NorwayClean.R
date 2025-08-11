library(tidyverse)
library(readxl)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Norway/county level data")

## loading data ####

## due to European formatting issues, 
## data was first saved as a csv from the website. 
## Then, semicolon was replaced with comma in a txt file,
## then resaved to csv. 
## Then it was saved into an excel, and first 3 rows
## (header of the document) were removed. 

dataF <- read_excel("county vaccination.xlsx") %>%
  pivot_longer(!date, names_to = "A", values_to = "vnum") %>%
  separate(A, c("virus", "dose", "location", "vaccine"), sep = ",") %>%
  filter(location != " Ikke oppgitt") %>%
  mutate(virus = NULL) %>%
  mutate(location = gsub(" ", "", location)) %>% 
  mutate(date = as.Date(date, "%d.%m.%Y")) %>% 
  filter(dose == " Dose 1") %>% 
  group_by(date, location) %>% 
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  arrange(date) %>%
  group_by(location) %>% 
  mutate(vcum = cumsum(vnum)) %>%
  ungroup()

dataF$location[dataF$location == "Oslo(f)"] <- "Oslo"


### SES data  ###

dataSES <- read_excel("countyIncome.xlsx")[c(1,2)] %>%
  rename(location = 1, value = 2) %>%
  mutate(measure = "income")

dataSES$location[dataSES$location == "30 Viken"] <- "Viken"
dataSES$location[dataSES$location == "03 Oslo"] <- "Oslo"
dataSES$location[dataSES$location == "34 Innlandet"] <- "Innlandet"
dataSES$location[dataSES$location == "38 Vestfold og Telemark"] <- "Vestfold og Telemark"
dataSES$location[dataSES$location == "42 Agder"] <- "Agder"
dataSES$location[dataSES$location == "11 Rogaland"] <- "Rogaland"
dataSES$location[dataSES$location == "46 Vestland"] <- "Vestland"
dataSES$location[dataSES$location == "15 Møre og Romsdal"] <- "Møre og Romsdal"
dataSES$location[dataSES$location == "50 Trøndelag - Trööndelage"] <- "Trøndelag"
dataSES$location[dataSES$location == "54 Troms og Finnmark - Romsa ja Finnmárku"] <- "Troms og Finnmark"
dataSES$location[dataSES$location == "18Nordland-Nordlánnda"] <- "Nordland"


dataSES <- dataSES %>%
  mutate(location = gsub(" ", "", location))

### population ###

SvalPop <- read_excel("SvalPop.xlsx")
pop <- read_excel("county population.xlsx", sheet = 2) %>%
  rename(location = 1, pop = 2) %>%
  rbind(SvalPop)

pop$location[pop$location == "Troms og Finnmark - Romsa ja Finnmárku"] <- "Troms og Finnmark"
pop$location[pop$location == "Trøndelag-Trööndelage"] <- "Trøndelag"


pop <- pop %>%
  mutate(location = gsub(" ", "", location))

### merge ###

Set <- inner_join(dataF, pop) %>%
  inner_join(dataSES)

Set <- Set %>% mutate(value = gsub(" ", "", value)) %>%
  mutate(value = as.numeric(value))

write.csv(Set,"C:/Users/sophi/Box/Vaccine_inequity/updated data/NO_Set.csv", row.names = FALSE)


