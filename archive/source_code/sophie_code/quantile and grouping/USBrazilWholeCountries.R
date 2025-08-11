library(tidyverse)
library(viridis)
library(lubridate)
library(dineq)
library(cowplot)
library(readxl)
library(MetBrewer)
library(oce)

#### data US ####
setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data")

dataUS1 <- read_csv("US_Set_1.csv") %>%
  filter(dose == "partial") %>%
  mutate(location = as.numeric(location))

dataUS2 <- read_csv("US_Set_2.csv") %>%
  mutate(location = as.numeric(location))


#### United States ####

## aggregate by week 
dataUS1 <- dataUS1 %>%
  mutate(week = difftime(date, min(date, na.rm = T), units = "weeks")) %>%
  mutate( week = gsub(" weeks", "", week)) %>%
  mutate(week = as.integer(week)) %>%
  group_by(location, dose, week) %>%
  ## here we pick the max date in a week for a location, and filter out to just get the data for that date (i.e., we are pulling the most recent cumulative count for the week).
  filter(date == max(date, na.rm = T)) %>%
  ungroup() %>%
  mutate(date = NULL) %>%
  ## for each location and dose type, find the maximum week recorded and tag it
  group_by(location, dose) %>%
  mutate(weekm = max(week, na.rm = T)) %>%
  ungroup() %>%
  ## for each state, look at the minimum maxweek tagged for each state and filter anything before that. This way, we stop smoothing the state data once locations start dropping off, so we don't get dips 
  group_by(state, dose) %>%
  filter(week <= min(weekm, na.rm = T)) %>%
  ungroup()

## remove the states that we showed in our analysis had weird data
dataUS1 <- dataUS1 %>%
       filter(state != "CO"
             & state != "GA"
             & state != "MI" & state != "NE"
             & state != "NE" & state != "NM"
             & state != "OH" & state != "SD"
             & state != "TX" & state != "UT"
             & state != "VA" & state != "WV")

## inner join, make sure we only keep where we have both vaccine and SES data
dataUS <- inner_join(dataUS1, dataUS2, by = c("location")) %>%
  filter(!is.na(vperc), !is.na(value))

## US income data - split quantiles

## grab the US income data
dataUSinc <- dataUS[c("location", "measure", "value")] %>%
  distinct() %>%
  filter(measure == "income5") %>%
  mutate(nbin = ntile(x= value, n = 10)) %>%
  mutate(cbin = cut_interval(value, n = 10, labels = c("L",2,3,4,5,6,7,8,9,"H"), na.rm = T))

dataUSinc$nbin[dataUSinc$nbin == 1] <- "L"
dataUSinc$nbin[dataUSinc$nbin == 10] <- "H"

## left join 
dataUS <- inner_join(dataUS, dataUSinc,by = c("location", "measure", "value"))

## pivot 
dataUS <- dataUS[c("country", "location","week", "vperc", "measure", "value", "cbin", "nbin")]%>%
  pivot_longer(cols = c("cbin", "nbin"), names_to = "bins", values_to = "binvalue") 

## grid summary
gridUS <- dataUS %>%
  group_by(country, week, measure, bins, binvalue) %>%
  summarise(avg = mean(vperc)) %>%
  ungroup()

## counts 
distinctUS <- dataUS %>%
  filter(!is.na(vperc), !is.na(value)) %>%
  select(country, location, measure) %>%
  distinct() %>%
  count(country, measure) %>%
  add_column(continent = "North America") %>%
  filter(measure == "income5")

#### full set US ####

makeCSVUS <- inner_join(gridUS, distinctUS, by = c("country", "measure")) %>%
  filter(bins == "nbin") %>%
  rename(bin = binvalue, value = avg) %>%
  mutate(bins = NULL) %>%
  filter(bin == "L" | bin == "H")

makeCSVUS <- makeCSVUS %>%
  rename(numberLoc = n) %>%
  mutate(resolution = NULL)

## despike
makeCSVUS <- makeCSVUS %>%
  group_by(country, measure, bin) %>%
  mutate(value = despike(
    value,
    reference = "median",
    n = 1,
    k = 15,
    min = NA,
    max = NA,
    replace = "NA")
  )

#### data Brazil ####

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Brazil/Finished sets")

AC <- read_csv("BR_Set_1_AC.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup()
AL <- read_csv("BR_Set_1_AL.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(AC)
AM <- read_csv("BR_Set_1_AM.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(AL)
AP <- read_csv("BR_Set_1_AP_codetest.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(AM)
BA <- read_csv("BR_Set_1_BA.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(AP)
CE <- read_csv("BR_Set_1_CE.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(BA)
DF <- read_csv("BR_Set_1_DF.csv") %>%
  filter(date >= as.Date("2021-01-17"))  %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(CE)
ES <- read_csv("BR_Set_1_ES.csv") %>%
  filter(date >= as.Date("2021-01-17"))  %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(DF)
GO <- read_csv("BR_Set_1_GO.csv") %>%
  filter(date >= as.Date("2021-01-17"))  %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(ES)
MA <- read_csv("BR_Set_1_MA.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(GO)
MG <- read_csv("BR_Set_1_MG.csv") %>%
  filter(date >= as.Date("2021-01-17"))  %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(MA)
MS <- read_csv("BR_Set_1_MS.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(MG)
MT <- read_csv("BR_Set_1_MT.csv") %>%
  filter(date >= as.Date("2021-01-17"))  %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(MS)
PA <- read_csv("BR_Set_1_PA.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(MT)
PB <- read_csv("BR_Set_1_PB.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(PA)
PE <- read_csv("BR_Set_1_PE.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(PB)
PI <- read_csv("BR_Set_1_PI.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(PE)
PR <- read_csv("BR_Set_1_PR.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(PI)
RN <- read_csv("BR_Set_1_RN.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(PR)
RJ <- read_csv("BR_Set_1_RJ.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(RN)
RO <- read_csv("BR_Set_1_RO.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(RJ)
RS <- read_csv("BR_Set_1_RS.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(RO)
SC <- read_csv("BR_Set_1_SC.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(RS)
SE <- read_csv("BR_Set_1_SE.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(SC)
SP <- read_csv("BR_Set_1_SP.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(SE)
TO <- read_csv("BR_Set_1_TO.csv") %>%
  filter(date >= as.Date("2021-01-17")) %>%
  group_by(code, date, dose, state) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  rbind(SP)

Set1 <- TO
### load SES data

Set2 <- read_csv("BR_Set_2.csv")

#### Brazil ####
Set1 <- Set1 %>% filter(dose == "partial")

start <- c("2021-01-17")

Set1 <- Set1 %>%
  mutate(week = difftime(date, as.Date(start, "%Y-%m-%d"), units = "weeks")) %>%
  mutate( week = gsub(" weeks", "", week)) %>%
  mutate(week = as.integer(week)) %>%
  group_by(code, week) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  arrange(week) %>%
  group_by(code) %>%
  mutate(vcum = cumsum(vnum)) %>%
  ungroup()

## need population
pop <- read_excel("popclean.xlsx")

Set2 <- Set2[c(1,3:6)] %>%
  rename(state = stateac) %>%
  filter(measure != "population") %>%
  rename(name = location)

Brazil <- inner_join(Set1,Set2, by = c("code")) %>%
  mutate(code = as.character(code))
Brazil <- inner_join(Brazil,pop, by = c("name", "state"))

Brazil <- Brazil %>% 
  mutate(pop = as.numeric(pop),vperc = 100*vcum/pop) %>% filter(!is.na(vperc), !is.na(value))


## income5 is hardcore poverty, >100 locations
Brazil100 <- Brazil[c("code", "measure", "value")] %>%
  distinct() %>%
  filter(measure == "income5") %>%
  mutate(nbin = ntile(x= value, n = 10)) %>%
  mutate(cbin = cut_interval(value, n = 10, labels = c("H",2,3,4,5,6,7,8,9,"L"), na.rm = T))

Brazil100$nbin[Brazil100$nbin == 1] <- "H"
Brazil100$nbin[Brazil100$nbin == 10] <- "L"

Brazil <- Brazil %>%
  left_join(Brazil100, by = c("code", "measure", "value")) %>%
  group_by(code) %>%
  filter(!is.na(value), !all(is.na(vperc))) %>%
  ungroup() %>% 
  add_column(country = "Brazil")

### pivot 
Brazil <- Brazil[c("country", "code", "week", "vperc", "measure", "value", "cbin", "nbin")]%>%
  pivot_longer(cols = c("cbin", "nbin"), names_to = "bins", values_to = "binvalue") %>%
  filter(measure == "income5")


### grid summary
Brazil <- Brazil %>%
  group_by(country,week, measure, bins, binvalue) %>%
  summarise(avg = mean(vperc)) %>%
  ungroup()

### distinct Br
distinctBr <- Brazil100 %>%
  select(code) %>%
  distinct() %>%
  count() %>%
  add_column(country = "Brazil", .before = "n")

### make csv

makeCSV <- Brazil[c(1:6)] %>%
  filter(binvalue == "L" | binvalue == "H") %>%
  filter(bins == "nbin") %>%
  rename(bin = binvalue, value = avg) %>%
  mutate(bins = NULL) %>%
  add_column(continent = "South America") %>%
  left_join(distinctBr)


### clean the dips in the data
makeCSV <- makeCSV %>%
  group_by(country,measure, bin) %>%
  mutate(value = despike(
    value,
    reference = "median",
    n = 0.2,
    k = 15,
    min = NA,
    max = NA,
    replace = "NA")
  )

full <- rbind(makeCSV, makeCSVUS)

write.csv(full,"C:/Users/sophi/Box/Vaccine_inequity/FINAL FIGURES/BrazilUSGiniAggregate.csv", row.names = FALSE)

