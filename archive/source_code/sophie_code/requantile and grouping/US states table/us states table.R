library(tidyverse)
library(viridis)
library(lubridate)
library(dineq)
library(cowplot)
library(readxl)
library(MetBrewer)


dataUS1 <- read_csv("US_Set_1.csv") %>%
  filter(dose == "partial")

dataUS2 <- read_csv("US_Set_2.csv") %>%
  filter(measure == "income5" | measure == "income6")

## for example, Guam gets removed from the data, 
##because while there is vaccine data, we do not have SES data represented in the data.
test <- dataUS1 %>% filter(state == "GU")
test <- dataUS2 %>% filter(location == "	
66010")

test <- dataUS1 %>% filter(state == "PR")
test <- dataUS2 %>% filter(location == "72075")

## load distinct counts of number of locations for each state
datadistinct <- read_csv("distinct.csv") %>% 
  filter(country == "United States")
datadistinct <- datadistinct[c("measure", "state", "n")]

datadistinct$measure[datadistinct$measure == "income"] <- "income5"
datadistinct$measure[datadistinct$measure == "poverty"] <- "income6"

## fix the dips ##

dataUS1 <- dataUS1 %>%
  group_by(location, dose) %>%
  arrange(date) %>%
  mutate(dateP = if_else(vperc > lead(vperc), date, NA_Date_)) %>%
  mutate(datePF = if_else(all(is.na(dateP)), max(date, na.rm = T), min(dateP, na.rm = T))) %>%
  filter(date <= datePF) %>%
  ungroup() 

## date to week ##

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

## add state info to SES ## 

state <- dataUS1[c(3,4)] %>%
  distinct()
dataUS2 <- left_join(dataUS2, state, by = "location")
## this is to remove repeated rows, I'm not sure how rows ended up repeated. Might be because I pulled the metro and SVI data from the US vaccine data, so it would've duplicated rows because of dates
dataUS2 <- dataUS2 %>% distinct()

## get list of distinct locations ##
distinctUS <- dataUS2 %>%
  filter(measure == "income5" | measure == "income6") %>%
  inner_join(dataUS1) %>%
  filter(!is.na(vperc), !is.na(value)) %>%
  select(location, measure) %>%
  distinct()

## join SES and distinct data ##
## this will filter out the locations where we don't have both types of data
dataUS2 <- inner_join(dataUS2, distinctUS, by = c("location", "measure"))

## join vaccine and SES data
dataUS <- inner_join(dataUS1, dataUS2, by = c("location", "state"))

## join the data counting how many locations per state, with the US data
dataUS <- inner_join(datadistinct, dataUS, by = c("measure", "state"))


## filter out irrelevant columns, and get distinct
dataUS <- dataUS[c("measure", "state", "n", "week")] %>%
  distinct()

grp1less10 <- dataUS %>%
  group_by(state, measure) %>%
  filter(max(week, na.rm = T) <= 10, n < 20) %>%
  ungroup()
grp1less10 <- grp1less10[c("measure", "state")] %>%
  distinct() %>%
  count(measure)

grp1greater10 <- dataUS %>%
  group_by(state, measure) %>%
  filter(max(week, na.rm = T) > 10, n < 20) %>%
  ungroup()
grp1greater10 <- grp1greater10[c("measure", "state")] %>%
  distinct() %>%
  count(measure)


grp2less10 <- dataUS %>%
  group_by(state, measure) %>%
  filter(max(week, na.rm = T) <= 10, n >= 20, n<100) %>%
  ungroup()
grp2less10 <- grp2less10[c("measure", "state")] %>%
  distinct()%>%
  count(measure)

grp2greater10 <- dataUS %>%
  group_by(state, measure) %>%
  filter(max(week, na.rm = T) > 10, n >= 20, n<100) %>%
  ungroup()
grp2greater10 <- grp2greater10[c("measure", "state")] %>%
  distinct()%>%
  count(measure)

grp3less10 <- dataUS %>%
  group_by(state, measure) %>%
  filter(max(week, na.rm = T) <= 10, n>=100) %>%
  ungroup()
grp3less10 <- grp3less10[c("measure", "state")] %>%
  distinct()%>%
  count(measure)

grp3greater10 <- dataUS %>%
  group_by(state, measure) %>%
  filter(max(week, na.rm = T) > 10, n>=100) %>%
  ungroup()
grp3greater10 <- grp3greater10[c("measure", "state")] %>%
  distinct()%>%
  count(measure)


## total number of states that we have
statecount <- datadistinct[c("measure", "state")] %>%
  distinct()%>%
  count(measure)


