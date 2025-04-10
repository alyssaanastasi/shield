library(tidyverse)
library(readxl)

## loading income deprivation ####
dataDep1 <- read_excel("localincomedeprivationdata.xlsx", sheet = "Local authorities")[c(1,5)] %>%
  rename(location = "Local Authority District code (2019)") %>%
  rename(income3 = "Income - Proportion of LSOAs in most deprived 10% nationally")

dataDep2 <- read_excel("localincomedeprivationdata.xlsx", sheet = "Rankings for all indicators")[c(1,11)] %>%
  rename(location = "Local Authority District code (2019)") %>%
  rename(income4 = "Income deprivation rate")

## change working directory

## add disposable income measures
## load data ####
fileNames <- list.files()
dataNames <- tibble(x = fileNames) %>% 
  rowid_to_column("ID") %>%
  mutate(x = str_remove(x, ".xls")) %>%
  mutate(x = str_remove(x, "regionalgrossdisposablehouseholdincomelocalauthority"))
dataDisp <- tibble(NULL)
for(i in 1:length(fileNames)){
  temp <- read_excel(fileNames[i], skip = 1, sheet = "Table 2") %>% 
    mutate(ID = i) %>% left_join(dataNames)
  dataDisp <- dataDisp %>% bind_rows(temp)
}

dataDisp <- dataDisp[c(2,26)] %>%
  rename(location = "LAD code") %>%
  rename(income2 = 2)

dataF <- left_join(dataDep1, dataDep2, by = "location")
dataF <- left_join(dataF, dataDisp, by = "location") %>%
  pivot_longer(!location, names_to = "measure", values_to = "value")

dataF <- dataF %>%
  add_column(country = "UK", .before = "location")

write.csv(dataF,"C:/Users/sophi/Box/Vaccine_inequity/updated data/FormattedSES/UK_Set_2.csv", row.names = FALSE)