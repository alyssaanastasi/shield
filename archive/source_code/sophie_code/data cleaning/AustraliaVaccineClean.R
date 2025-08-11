library(tidyverse)
library(readxl)
library(MMWRweek)

## home working directory
setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Australia/weekly vaccine data")

## load data ####
fileNames <- list.files()
dataDate <- tibble(x = fileNames) %>% 
  rowid_to_column("ID") %>%
  mutate(x = str_remove(x, ".xlsx")) %>%
  separate(x, c("day", "month", "year"), sep = "-") 

dataAus <- tibble(NULL)
for(i in 1:length(fileNames)){
  temp <- read_excel(fileNames[i]) %>% 
    mutate(ID = i) %>%
    left_join(dataDate)
  dataAus <- dataAus %>% bind_rows(temp)
}

dataAus <- dataAus %>% 
  separate(`Measure Name`, c("state", "B", "dose"), sep = "-")

## get months to integers 
dataAus$month[dataAus$month == "september"] <- "09"
dataAus$month[dataAus$month == "october"] <- "10"
dataAus$month[dataAus$month == "november"] <- "11"
dataAus$month[dataAus$month == "december"] <- "12"

## fix the dates into date format
dataAus$date<-as.Date(with(dataAus,paste(year,month,day,sep="-")),"%Y-%m-%d") 
dataAus <- dataAus %>%
  mutate(year = NULL) %>%
  mutate(month = NULL) %>%
  mutate(day = NULL) 

## formatting the vaccine data ####
dataV <- dataAus %>%
  filter(B == " Residence state ") %>%
  mutate(B = NULL) %>%
  filter(dose == " Number of people 16 and over with 1 dose" | dose == " Number of people 16 and over fully vaccinated") %>%
  mutate(ID = NULL)

## change the doses to numbers
dataV$dose[dataV$dose == " Number of people 16 and over with 1 dose"] <- "partial"
dataV$dose[dataV$dose == " Number of people 16 and over fully vaccinated"] <- "full"

## rename as necessary
dataV <- dataV %>%
  rename(vcum = Value)

## formatting the pop data ####
dataP <- dataAus %>%
  filter(B == " Population 16 and over" & state != "National ")
dataP <- dataP[c(1,4,6)] %>%
  rename(pop = "Value")

## merging and renaming, adding columns
dataFinal <- left_join(dataV, dataP, by = c("state", "date")) %>%
  add_column(country = "Australia", .before = "state") %>%
  add_column(resolution = 1, .after = "country") %>%
  rename(location = "state")
dataFinal <- dataFinal[c(1,2,3, 7, 6, 4, 5)] %>%
  add_column(vaccine = NA, .after = "dose") %>%
  add_column(vnum = NA, .after = "vaccine") %>%
  add_column(vperc = NA, .after = "vcum")

write.csv(dataFinal,"~/Library/CloudStorage/Box-Box/Vaccine_inequity/updated data/Australia_Set_1.csv", row.names = FALSE)
