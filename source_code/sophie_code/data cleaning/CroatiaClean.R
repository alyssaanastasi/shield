library(tidyverse)

setwd("C:/Users/sophi/Box/Vaccine_inequity/updated data/Croatia")

Set1 <- read_csv("vaccination.csv.csv")

split0 <- Set1[c(3, "PodaciDetaljno/0/Zupanija", )]