library(tidyverse)

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
  mutate(virus = NULL)

## format data ####

  

## why do we only have female data for Oslo? FHI has been emailed



