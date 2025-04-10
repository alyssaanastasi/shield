library(tidyverse)

setwd("C:/Users/sophi/Box/Vaccine_inequity/Data/final")

datamake1 <- read_csv("makeCSV2_5.csv", col_types = cols (state = "c")) 
datamake2 <- read_csv("makeCSV3_5.csv", col_types = cols (state = "c"))
datamake3 <- read_csv("makeCSVPeru_5.csv", col_types = cols (state = "c")) %>%
  mutate(bins = NULL)
databr <- read_csv("Brazil_5.csv", col_types = cols (state = "c"))


MasterData <- rbind(datamake1, datamake2)
MasterData <- rbind(MasterData, datamake3)

### test plots
plot <- MasterData %>%
  filter(country == "United States", state == "KY") %>%
  ggplot(aes(x = week, y = value)) + geom_line(aes(color = bin, group = bin), alpha=.25) + theme_minimal()
plot

## the US states that still need removal after despike are:
## AK, CO,GA, MI, NE, NM, OH,
## SD, TX, UT, VA, WV
## this is done in the group analysis codes

## add Brazil

databr <- databr[c(1:6,8,7)] %>%
  rename(numberLoc = n) %>%
  filter(state != "CE")## this filters out the one state that is still funky after despike
databr$measure[databr$measure == "income5"] <- "poverty"

MasterData <- rbind(MasterData, databr)


### test plots
plot <- MasterData %>%
  filter(country == "Peru") %>%
  ggplot(aes(x = week, y = value)) + geom_line(aes(color = bin, group = bin), alpha=.25) + theme_minimal()
plot


MasterData$measure[MasterData$measure == "income6"] <- "poverty"
MasterData$measure[MasterData$measure == "income5"] <- "income"
MasterData$measure[MasterData$measure == "income1"] <- "income"
MasterData$measure[MasterData$measure == "income2"] <- "income"
MasterData$measure[MasterData$measure == "income3"] <- "income deprivation proportion"
MasterData$measure[MasterData$measure == "income4"] <- "income deprivation rate"
MasterData$measure[MasterData$measure == "income7"] <- "income"
MasterData$measure[MasterData$measure == "income8"] <- "standard of living (euros)"

MasterData <- MasterData %>% filter(country != "China" & country != "Canada" & country != "Kenya")

write.csv(MasterData,"C:/Users/sophi/Box/Vaccine_inequity/Data/final/MasterData.csv", row.names = FALSE)



###### location data for supplement plot #######

d1 <- read_csv("loc2_5.csv", col_types = cols (state = "c")) %>%
  group_by(country, state) %>%
  filter(week == max(week)) %>%
  ungroup()

d2 <- read_csv("loc3_5.csv", col_types = cols (state = "c")) %>%
  group_by(country, state) %>%
  filter(week == max(week)) %>%
  ungroup()

d3 <- read_csv("locperu_5.csv") %>%
  add_column(state = NA, .after = 1) %>%
  group_by(country, state) %>%
  filter(week == max(week)) %>%
  ungroup()

d4 <- read_csv("locbr_5.csv", col_types = cols (state = "c")) %>%
  rename(location = code) %>%
  group_by(country, state) %>%
  filter(week == max(week)) %>%
  ungroup() %>%
filter(state != "CE")## this filters out the one state that is still wrong after despike


df <- rbind(d1, d2) %>%
  rbind(d3) %>%
  rbind(d4)

## df filtering to get just one measure
## one measure for US
df1 <- df %>% filter(country == "United States") %>% filter(measure == "income5")
df <- df %>% filter(country != "United States")
df <- rbind(df, df1)

## one measure for PR
df1 <- df %>% filter(country == "Puerto Rico") %>% filter(measure == "income5")
df <- df %>% filter(country != "Puerto Rico")
df <- rbind(df, df1)

## one measure for France
df1 <- df %>% filter(country == "France") %>% filter(measure == "income8" | measure == "income")
df <- df %>% filter(country != "France")
df <- rbind(df, df1)

## one measure for Korea
df1 <- df %>% filter(country == "Republic of Korea") %>%  filter(measure == "income2")
df <- df %>% filter(country != "Republic of Korea")
df <- rbind(df, df1)

## one meausre for UK
df1 <- df %>% filter(country == "UK") %>%  filter(measure == "income4")
df <- df %>% filter(country != "UK")
df <- rbind(df, df1)

## one measure for Brazil
df1 <- df %>% filter(country == "Brazil") %>%  filter(measure == "income5")
df <- df %>% filter(country != "Brazil")
df <- rbind(df, df1)

## one measure for Swiss
df1 <- df %>% filter(country == "Switzerland") %>%  filter(measure == "Socasst")
df <- df %>% filter(country != "Switzerland")
df <- rbind(df, df1)

## filter bins
df <- df %>% filter(bins == "nbin")

df <- df %>% filter(binvalue == "H" | binvalue == "L")

### this location only has 13 places, which is why there are 4 left
### test <- df %>% filter(state == "AM" & measure == "income5" & bins == "nbin")

df <- df %>% filter(country != "China" & country != "Canada" & country != "Kenya")


write.csv(df,"C:/Users/sophi/Box/Vaccine_inequity/Data/data_figures/locdata.csv", row.names = FALSE)




