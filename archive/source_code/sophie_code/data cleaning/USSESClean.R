library(tidyverse)

## education data ####
dataE <- read_csv("Education.csv")[c(1,44:47)] %>%
  rename(location = "FIPS Code") %>%
  rename(edu5 = "Percent of adults with less than a high school diploma, 2015-19") %>%
  rename(edu3 = "Percent of adults with a high school diploma only, 2015-19") %>%
  rename(edu4 = "Percent of adults completing some college or associate's degree, 2015-19") %>%
  rename(edu2 = "Percent of adults with a bachelor's degree or higher, 2015-19")

## poverty data ####
dataP <- read_csv("PovertyEstimates.csv") %>%
  pivot_wider(names_from = Attribute, values_from = Value)
dataP <- dataP[c("FIPStxt", "PCTPOVALL_2019", "MEDHHINC_2019")] %>%
  rename(location = FIPStxt, income6 = PCTPOVALL_2019, income5 = MEDHHINC_2019) %>%
  mutate(location = as.character(location))

### get just the poverty/income data as the SES
dataP <- dataP %>%
  pivot_longer(2:3, names_to = "measure", values_to = "value")
write.csv(dataP,"C:/Users/sophi/Box/Vaccine_inequity/updated data/US_Set_2.csv", row.names = FALSE)



## unemployment data ####
dataU <- read_csv("Unemployment.csv") %>%
  pivot_wider(names_from = Attribute, values_from = Value)
dataU <- dataU[c("FIPS_Code", "Unemployment_rate_2020")] %>%
  rename(location = FIPS_Code, labor2 = Unemployment_rate_2020) %>%
  mutate(location = as.character(location))

## pull CDC metro data and SVI ####
dataF <- read_csv("COVID-19_Vaccinations_in_the_United_States_County.csv")[c(2,23,28)] %>%
  rename(location = FIPS, SVI = SVI_CTGY, metro = Metro_status) %>%
  mutate(location = as.character(location))

## merge everything ####
dataF <- left_join(dataF, dataU, by = "location")
dataF <- left_join(dataF, dataP, by = "location")
dataF <- left_join(dataF, dataE, by = "location")

dataF <- dataF %>%
  pivot_longer(4:10, names_to = "measure", values_to = "value")

test <- dataF %>%
  filter(measure == "labor2")

write.csv(dataF,"C:/Users/sophi/Box/Vaccine_inequity/updated data/FormattedSES/US_Set_2.csv", row.names = FALSE)

## checking US data issues

Set1 <- read_csv("US_Set_1.csv")[c(1,3,4)] %>% distinct()
Set2 <- read_csv("US_Set_2.csv")[c(1,4,5)] %>%
  filter(measure == "income5" | measure == "income6") %>% distinct()

test <- inner_join(Set1, Set2, by = "location") %>% distinct()

