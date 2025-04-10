library(tidyverse)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Puerto Rico/SES data")

fileNames <- list.files()
dataNames <- tibble(x = fileNames) %>% 
  rowid_to_column("ID") %>%
  separate(x, c("location", "b"), sep = "_") %>%
  mutate(location = toupper(location)) %>%
  mutate(b = NULL)

##temp <- read_csv("Yauco_QuickFacts Aug-12-2021 (34).csv")


dataF <- tibble(NULL)
for(i in 1:length(fileNames)){
  temp <- read_csv(fileNames[i])[c(1,3)] %>% 
    rename(measure = Fact) %>%
    rename(value = 2) %>%
    rowid_to_column("tag") %>%
    filter(tag < 63) %>%
    mutate(ID = i) %>% left_join(dataNames)
  dataF <- dataF %>% bind_rows(temp)
}

## get just the columns we want, plus population
dataF <- dataF[c(2,3,5)] %>%
  filter(measure == "Median gross rent, 2015-2019" | 
  measure == "Persons per household, 2015-2019" | 
  measure == "Households with a computer, percent, 2015-2019" | 
  measure == "Households with a broadband Internet subscription, percent, 2015-2019" | 
  measure == "High school graduate or higher, percent of persons age 25 years+, 2015-2019" | 
measure == "Bachelor's degree or higher, percent of persons age 25 years+, 2015-2019" | 
measure == "Persons  without health insurance, under age 65 years, percent" | 
measure == "Median household income (in 2019 dollars), 2015-2019" | 
measure == "Per capita income in past 12 months (in 2019 dollars), 2015-2019" | 
measure == "Persons in poverty, percent" | 
measure == "Total employment, 2019" | 
  measure == "Population estimates, July 1, 2019,  (V2019)")

## renaming
dataF$measure[dataF$measure == "Median gross rent, 2015-2019"] <- "housing3"
dataF$measure[dataF$measure == "Persons per household, 2015-2019"] <- "housing4"
dataF$measure[dataF$measure == "Households with a computer, percent, 2015-2019"] <- "housing5"
dataF$measure[dataF$measure == "Households with a broadband Internet subscription, percent, 2015-2019"] <- "housing2"
dataF$measure[dataF$measure == "High school graduate or higher, percent of persons age 25 years+, 2015-2019"] <- "edu1"
dataF$measure[dataF$measure == "Bachelor's degree or higher, percent of persons age 25 years+, 2015-2019"] <- "edu2"
dataF$measure[dataF$measure == "Persons  without health insurance, under age 65 years, percent"] <- "wellbeing3"
dataF$measure[dataF$measure == "Median household income (in 2019 dollars), 2015-2019"] <- "income5"
dataF$measure[dataF$measure == "Per capita income in past 12 months (in 2019 dollars), 2015-2019"] <- "income1"
dataF$measure[dataF$measure == "Persons in poverty, percent"] <- "income6"
dataF$measure[dataF$measure == "Total employment, 2019"] <- "labor1"
dataF$measure[dataF$measure == "Population estimates, July 1, 2019,  (V2019)"] <- "pop"

## pivot and divide employment to get an unemployment rate
## also getting rid of percentage and dollar signs, to have numeric values instead of characters
## I had to use substring in some places, because gsub and str_remove were not removing the dollar signs
dataF <- dataF %>%
  pivot_wider(names_from = measure, values_from = value) %>%
  mutate(pop = as.numeric(gsub(",", "", pop))) %>%
  mutate(labor1 = as.numeric(gsub(",", "", labor1))) %>%
  ## get unemployment rate
  mutate(labor1 = labor1/pop) %>%
  mutate(pop = NULL) %>%
  mutate(housing3 = substring(housing3, 2)) %>%
  mutate(housing3 = as.numeric(housing3)) %>%
  mutate(housing5 = as.numeric(str_remove(housing5, "%"))) %>%
  mutate(housing2 = as.numeric(str_remove(housing2, "%"))) %>%
  mutate(edu1 = as.numeric(str_remove(edu1, "%"))) %>%
  mutate(edu2 = as.numeric(str_remove(edu2, "%"))) %>%
  mutate(wellbeing3 = as.numeric(str_remove(wellbeing3, "%"))) %>%
  mutate(income5 = substring(income5, 2)) %>%
  mutate(income5 = as.numeric(str_remove(income5, ","))) %>%
  mutate(income1 = substring(income1, 2)) %>%
  mutate(income1 = as.numeric(str_remove(income1, ","))) %>%
  mutate(income6 = as.numeric(str_remove(income6,"%"))) %>%
  mutate(housing4 = as.numeric(housing4)) %>%
  pivot_longer(2:12, names_to = "measure", values_to = "value") %>%
  add_column(country = "Puerto Rico", .before = "location")

write.csv(dataF,"~/Library/CloudStorage/Box-Box/Vaccine_inequity/updated data/FormattedSES/PR_Set_2.csv", row.names = FALSE)

##str_remove("$444", "\\$")
