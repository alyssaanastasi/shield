library(tidyverse)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Puerto Rico")
## load vaccine data ####
dataF <- read_csv("vaccine data.csv")[c(2,4,5,6)] %>%
  rename(location = "CO_MUNICIPIO") %>%
  rename(date = "FE_VACUNA") %>%
  rename(vnum = "NU_DOSIS") %>%
  rename(vaccine = CO_MANUFACTURERO) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date, location, vaccine) %>%
  summarise(vnum = sum(vnum, na.rm = T)) %>%
  ungroup() %>%
  add_column(country = "Puerto Rico", .before = "location") %>%
  add_column(resolution = 2, .after = "country") %>%
  add_column(dose = "D", .after = "date") %>%
  add_column(vcum = NA, .after = "vnum") %>%
  add_column(vperc = NA, .after = "vcum") %>%
  mutate(location = gsub("_","",location))

dataT <- dataF %>%
  filter(location == "GUAYNABO")

## pop data ####

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Puerto Rico/SES data")

fileNames <- list.files()
dataNames <- tibble(x = fileNames) %>% 
  rowid_to_column("ID") %>%
  separate(x, c("location", "b"), sep = "_") %>%
  mutate(location = toupper(location)) %>%
  mutate(b = NULL)

##temp <- read_csv("Yauco_QuickFacts Aug-12-2021 (34).csv")


dataG <- tibble(NULL)
for(i in 1:length(fileNames)){
  temp <- read_csv(fileNames[i])[c(1,3)] %>% 
    rename(measure = Fact) %>%
    rename(value = 2) %>%
    rowid_to_column("tag") %>%
    filter(tag == 1) %>%
    mutate(ID = i) %>% left_join(dataNames)
  dataG <- dataG %>% bind_rows(temp)
}
## in the above loading, I just kept the population estimates from 2019

dataG <- dataG[c(3,5)] %>%
  rename(pop = value)

dataF <- left_join(dataF, dataG, by = "location")

dataF <- dataF[c(1:3,10,4,5,9,6,7,8)]

write.csv(dataF,"C:/Users/sophi/Box/Vaccine_inequity/updated data/FormattedVaccine/PR_Set_1.csv", row.names = FALSE)

  