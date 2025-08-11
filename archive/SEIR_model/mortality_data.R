library(dplyr)
library(lubridate)

covid_mortality <- read.csv("~/Desktop/shield/data/COVID_mortality.csv") %>%
  mutate(date = mdy(date)) %>%
  filter(date >= as.Date("2022-04-01"))
         
flu_mortality <- read.csv("~/Desktop/shield/data/FLU_mortality.csv") %>%
  mutate(date = mdy(date))
         
rsv_mortality <- read.csv("~/Desktop/shield/data/RSV_mortality.csv") %>%
  mutate(date = mdy(date)) 
         
         