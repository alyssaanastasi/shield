library(tidyverse)
library(readxl)

## load data ####
dataF <- read_csv("SES data.csv")[c(2,3,4,15)] %>%
  rename(location = Regions) %>%
  rename(measure = IND) %>%
  rename(value = Value)

## format data ####
## removing unwanted measures 
dataF <- dataF %>%
  filter(measure != "VOTERS_SH" &
           measure != "SUBJ_LIFE_SAT" & 
           measure != "SUBJ_SOC_SUPP" &
           measure != "HOMIC_RA" &
           measure != "SUBJ_PERC_CORR")
## rename measures to desired names
dataF$measure[dataF$measure == "LIFE_EXP"] <- "wellbeing1"
dataF$measure[dataF$measure == "BB_ACC"] <- "housing2"
dataF$measure[dataF$measure == "AIR_POL"] <- "environ1"
dataF$measure[dataF$measure == "ROOMS_PC"] <- "housing1"
dataF$measure[dataF$measure == "UNEM_RA"] <- "labor2"
dataF$measure[dataF$measure == "EMP_RA"] <- "labor1"
dataF$measure[dataF$measure == "INCOME_DISP"] <- "income2"
dataF$measure[dataF$measure == "EDU38_SH"] <- "edu1"
dataF$measure[dataF$measure == "STD_MORT"] <- "wellbeing2"

## remove indicator column and national data
dataF <- dataF[c(1,2,4)] %>%
  filter(location != "Australia")

## rename regions to match the codes in vaccine data
dataF$location[dataF$location == "Australian Capital Territory (ACT)"] <- "ACT"
dataF$location[dataF$location == "New South Wales"] <- "NSW"
dataF$location[dataF$location == "Northern Territory (NT)"] <- "NT"
dataF$location[dataF$location == "Queensland"] <- "QLD"
dataF$location[dataF$location == "South Australia"] <- "SA"
dataF$location[dataF$location == "Tasmania"] <- "TAS"
dataF$location[dataF$location == "Victoria"] <- "VIC"
dataF$location[dataF$location == "Western Australia"] <- "WA"

## add country column
dataF <- dataF %>%
  add_column(country = "Australia")

## write csv
write.csv(dataF,"C:/Users/sophi/Box/Vaccine_inequity/updated data/Australia_Set_2.csv", row.names = FALSE)