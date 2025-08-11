library(readxl)
library(data.table)
library("writexl")
library(tibble)
library(dplyr)
library(tidyr)
# county_vaccination_all is the exact dataframe extracted from the file name county vaccination
# county_vaccination is a dataframe with date column extracted
# date is a dataframe which has been extracted from the county_vaccination_all dataframe
# df is a dataframe which will eventually be outputted as an excel file with requested/required format.


county_vaccination_all <- read_excel("Desktop/county vaccination.xlsx")
county_vaccination <- county_vaccination_all[-1]
date <- county_vaccination_all[1]
colnames(county_vaccination)
df <- county_vaccination





for (i in seq_along(df)){
  df[208+i] <- ifelse(grepl("Dose 1,", colnames(df)[i]), "partial",
                      ifelse(grepl("Dose 2,", colnames(df)[i]), "full", "3 or booster"))
  i = i+1
}



i=1
df1 <- county_vaccination

for (i in seq_along(df1)){
  df[416+i] <- ifelse(grepl("Pfizer)", colnames(df1)[i]), "Pfizer",
                      ifelse(grepl("CoronaVac", colnames(df1)[i]), "CoronaVac",
                             ifelse(grepl("Covishield", colnames(df1)[i]), "Covishield",
                                    ifelse(grepl("Janssen", colnames(df1)[i]), "Janssen",
                                           ifelse(grepl("Moderna", colnames(df1)[i]), "Moderna",
                                                  ifelse(grepl("CureVac", colnames(df1)[i]), "CureVac",
                                                         ifelse(grepl("Covaxin", colnames(df1)[i]), "Covaxin",
                                                         "AstraZeneca")))))))
  i = i+1
}


i=1
df2 <- county_vaccination

for (i in seq_along(df2)){
  df[624+i] <- ifelse(grepl("Agder", colnames(df2)[i]), "Agder",
                      ifelse(grepl("oppgitt", colnames(df2)[i]), "Ikke oppgitt",
                             ifelse(grepl("Innlandet", colnames(df2)[i]), "Hedmark and Oppland",
                                    ifelse(grepl("Romsdal", colnames(df2)[i]), "Møre og Romsdal",
                                           ifelse(grepl("Nordland", colnames(df2)[i]), "Nordland",
                                                  ifelse(grepl("Oslo", colnames(df2)[i]), "Oslo (f)",
                                                         ifelse(grepl("Rogaland", colnames(df2)[i]), "Rogaland",
                                                                ifelse(grepl("Svalbard", colnames(df2)[i]), "Svalbard",
                                                                       ifelse(grepl("Finnmark", colnames(df2)[i]), "Troms og Finnmark",
                                                                              ifelse(grepl("Trøndelag", colnames(df2)[i]), "Trøndelag",
                                                                                     ifelse(grepl("Telemark", colnames(df2)[i]), "Vestfold og Telemark",
                                                                                            ifelse(grepl("Vestland", colnames(df2)[i]), "Vestland",
                                                                                                   "Viken"))))))))))))
  i = i+1
}



# test is a dataframe with population
# testdose is a dataframe with dose number
# testdosetype is a dataframe with type of the vaccine
# testcity is a dataframe with cities




test <- df[1:208]
testdose <- df[209:416]
testdosetype <- df[417:624]
testcity <- df[625:832]




test1 <- data.frame('Covid-19, Dose 1, Agder, Comirnaty (BioNTech og Pfizer)'=unlist(test, use.names = FALSE))
testdose1 <- data.frame('209'=unlist(testdose, use.names = FALSE))
testdosetype1 <- data.frame('417'=unlist(testdosetype, use.names = FALSE))
testcity1 <- data.frame('625'=unlist(testcity, use.names = FALSE))




test_date <- matrix(rep(date,each=208),nrow=208)
test_date <- data.frame(date,i=rep(1:208,ea=NROW(date)))
test_date <- test_date[-2]



test_col<- cbind(testcity1, test_date, testdose1, testdosetype1, test1)



colnames(test_col) <- c('location', 'date', 'dose', 'vaccine', 'vnum')



test_col <- test_col %>%
  add_column(resolution = 1, .before="location") %>%
  add_column(country = "Norway", .before="resolution") %>%
  add_column(vcum = NA, .after="vnum") %>%
  add_column(vperc = NA, .after="vcum")


test_col <- test_col %>%
  mutate(pop = case_when(
    endsWith(location, "Viken") ~ "1241165",
    endsWith(location, "(f)") ~ "693494",
    endsWith(location, "Vestland") ~ "636531",
    endsWith(location, "Rogaland") ~ "479892",
    endsWith(location, "Trøndelag") ~ "468702",
    endsWith(location, "Telemark") ~ "419396",
    endsWith(location, "Hedmark and Oppland") ~ "371385",
    endsWith(location, "Agder") ~ "307231",
    endsWith(location, "Romsdal") ~ "265238",
    endsWith(location, "Finnmark") ~ "243311",
    endsWith(location, "Nordland") ~ "241235"
  ))
test_col <- test_col %>% relocate(pop, .before = date)


View(test_col)
#write_xlsx(test_col,"Desktop/for county vaccination filling.xlsx")



Agder <- subset(test_col, location == "Agder")
dfAgder <- data.frame(unclass(table(Agder$vaccine, Agder$dose)))%>%
  add_row( .before= 1)

Ikkeoppgitt<- subset(test_col, location == "Ikke oppgitt")
dfIkkeoppgitt <- data.frame(unclass(table(Ikkeoppgitt$vaccine, Ikkeoppgitt$dose)))%>%
  add_row( .before= 1)

Hedmark_Oppland<- subset(test_col, location == "Hedmark and Oppland")
dfHedmark_Oppland <- data.frame(unclass(table(Hedmark_Oppland$vaccine, Hedmark_Oppland$dose)))%>%
  add_row( .before= 1)

MøreogRomsdal<- subset(test_col, location == "Møre og Romsdal")
dfMøreogRomsdal <- data.frame(unclass(table(MøreogRomsdal$vaccine, MøreogRomsdal$dose)))%>%
  add_row( .before= 1)

Nordland<- subset(test_col, location == "Nordland")
dfNordland <- data.frame(unclass(table(Nordland$vaccine, Nordland$dose)))%>%
  add_row( .before= 1)

Oslof<- subset(test_col, location == "Oslo (f)")
dfOslof <- data.frame(unclass(table(Oslof$vaccine, Oslof$dose)))%>%
  add_row( .before= 1)

Rogaland<- subset(test_col, location == "Rogaland")
dfRogaland <- data.frame(unclass(table(Rogaland$vaccine, Rogaland$dose)))%>%
  add_row( .before= 1)


Svalbard<- subset(test_col, location == "Svalbard")
dfSvalbard <- data.frame(unclass(table(Svalbard$vaccine, Svalbard$dose)))%>%
  add_row( .before= 1)

TromsogFinnmark<- subset(test_col, location == "Troms og Finnmark")
dfTromsogFinnmark <- data.frame(unclass(table(TromsogFinnmark$vaccine, TromsogFinnmark$dose)))%>%
  add_row( .before= 1)

Trøndelag<- subset(test_col, location == "Trøndelag")
dfTrøndelag <- data.frame(unclass(table(Trøndelag$vaccine, Trøndelag$dose)))%>%
  add_row( .before= 1)

VestfoldogTelemark<- subset(test_col, location == "Vestfold og Telemark")
dfVestfoldogTelemark <- data.frame(unclass(table(VestfoldogTelemark$vaccine, VestfoldogTelemark$dose)))%>%
  add_row( .before= 1)

Vestland<- subset(test_col, location == "Vestland")
dfVestland <- data.frame(unclass(table(Vestland$vaccine, Vestland$dose)))%>%
  add_row( .before= 1)

Viken<- subset(test_col, location == "Viken")
dfViken <- data.frame(unclass(table(Viken$vaccine, Viken$dose)))%>%
  add_row( .before= 1)

eh <- rbind(dfAgder, dfIkkeoppgitt, dfHedmark_Oppland, dfMøreogRomsdal, dfNordland, dfOslof, dfRogaland, dfSvalbard, dfTromsogFinnmark, 
            dfTrøndelag, dfVestfoldogTelemark, dfVestland, dfViken)

# View(eh)
# View(dfViken)
# View(Viken)


####Agder&Rogaland

Agder_Rogaland_ <- Agder %>% 
  full_join(Rogaland,by= c("date", "dose", "vaccine"))

  
Agder_Rogaland <- Agder_Rogaland_ %>% 
  mutate(location = "Agder and Rogaland")%>%
  mutate(pop.x = replace_na(pop.x, 307231))%>%
  mutate(pop.y = replace_na(pop.y, 479892))%>%
  mutate(vnum.x = replace_na(vnum.x, 0))%>%
  mutate(vnum.y = replace_na(vnum.y, 0))%>%
  mutate(country.x = replace_na(country.x, "Norway"))%>%
  mutate(resolution.x = replace_na(resolution.x, 1))%>%
  relocate(location, .before = date )%>%
  rename(country = "country.x")%>%
  rename(resolution = "resolution.x")%>%
  mutate(date.y = NULL)%>%
  mutate(location.x = NULL)%>%
  mutate(location.y = NULL)%>%
  mutate(country.y = NULL)%>%
  mutate(resolution.y = NULL)
  

Agder_Rogaland$pop <- as.numeric(Agder_Rogaland$pop.x) + as.numeric(Agder_Rogaland$pop.y)
Agder_Rogaland$vnum <- as.numeric(Agder_Rogaland$vnum.x) + as.numeric(Agder_Rogaland$vnum.y)
Agder_Rogaland <- Agder_Rogaland %>%
  relocate(pop, .after = location)%>%
  mutate(pop.x = NULL)%>%
  mutate(pop.y = NULL)%>%
  mutate(vnum.x = NULL)%>%
  mutate(vnum.y = NULL)%>%
  mutate(vcum.x = NULL)%>%
  mutate(vcum.y = NULL)%>%
  mutate(vperc.x = NULL)%>%
  mutate(vperc.y = NULL)%>%
  add_column(vcum = NA, .after = "vnum") %>%
  add_column(vperc = NA, .after = "vcum")
  


####### Northern Norway ~ Nordland,Troms og Finnmark

Nordland_TromsogFinnmark_ <- Nordland %>% 
  full_join(TromsogFinnmark,by= c("date", "dose", "vaccine"))
#View(Adger_Rogaland_)

N_Norway <- Nordland_TromsogFinnmark_ %>% 
  mutate(location = "Northern Norway")%>%
  mutate(pop.x = replace_na(pop.x, 307231))%>%
  mutate(pop.y = replace_na(pop.y, 479892))%>%
  mutate(vnum.x = replace_na(vnum.x, 0))%>%
  mutate(vnum.y = replace_na(vnum.y, 0))%>%
  mutate(country.x = replace_na(country.x, "Norway"))%>%
  mutate(resolution.x = replace_na(resolution.x, 1))%>%
  relocate(location, .before = date )%>%
  rename(country = "country.x")%>%
  rename(resolution = "resolution.x")%>%
  mutate(date.y = NULL)%>%
  mutate(location.x = NULL)%>%
  mutate(location.y = NULL)%>%
  mutate(country.y = NULL)%>%
  mutate(resolution.y = NULL)


N_Norway$pop <- as.numeric(N_Norway$pop.x) + as.numeric(N_Norway$pop.y)
N_Norway$vnum <- as.numeric(N_Norway$vnum.x) + as.numeric(N_Norway$vnum.y)
N_Norway <- N_Norway %>%
  relocate(pop, .after = location)%>%
  mutate(pop.x = NULL)%>%
  mutate(pop.y = NULL)%>%
  mutate(vnum.x = NULL)%>%
  mutate(vnum.y = NULL)%>%
  mutate(vcum.x = NULL)%>%
  mutate(vcum.y = NULL)%>%
  mutate(vperc.x = NULL)%>%
  mutate(vperc.y = NULL)%>%
  add_column(vcum = NA, .after = "vnum") %>%
  add_column(vperc = NA, .after = "vcum")

#View(N_Norway)


##### South-Eastern Norway ~ Viken,,Vestfold og Telemark


Viken_Vestfold <- Viken %>% 
  full_join(VestfoldogTelemark,by= c("date", "dose", "vaccine"))


SE_Norway <- Viken_Vestfold %>% 
  mutate(location = "South Eastern Norway")%>%
  mutate(pop.x = replace_na(pop.x, 307231))%>%
  mutate(pop.y = replace_na(pop.y, 479892))%>%
  mutate(vnum.x = replace_na(vnum.x, 0))%>%
  mutate(vnum.y = replace_na(vnum.y, 0))%>%
  mutate(country.x = replace_na(country.x, "Norway"))%>%
  mutate(resolution.x = replace_na(resolution.x, 1))%>%
  relocate(location, .before = date )%>%
  rename(country = "country.x")%>%
  rename(resolution = "resolution.x")%>%
  mutate(date.y = NULL)%>%
  mutate(location.x = NULL)%>%
  mutate(location.y = NULL)%>%
  mutate(country.y = NULL)%>%
  mutate(resolution.y = NULL)


SE_Norway$pop <- as.numeric(SE_Norway$pop.x) + as.numeric(SE_Norway$pop.y)
SE_Norway$vnum <- as.numeric(SE_Norway$vnum.x) + as.numeric(SE_Norway$vnum.y)
SE_Norway <- SE_Norway %>%
  relocate(pop, .after = location)%>%
  mutate(pop.x = NULL)%>%
  mutate(pop.y = NULL)%>%
  mutate(vnum.x = NULL)%>%
  mutate(vnum.y = NULL)%>%
  mutate(vcum.x = NULL)%>%
  mutate(vcum.y = NULL)%>%
  mutate(vperc.x = NULL)%>%
  mutate(vperc.y = NULL)%>%
  add_column(vcum = NA, .after = "vnum") %>%
  add_column(vperc = NA, .after = "vcum")

#View(SE_Norway)



####### Western Norway ~ Vestland, Møre og Romsdal



Vestland_MøreogRomsdal <- Vestland %>% 
  full_join(MøreogRomsdal, by= c("date", "dose", "vaccine"))


W_Norway <- Vestland_MøreogRomsdal %>% 
  mutate(location = "Western Norway")%>%
  mutate(pop.x = replace_na(pop.x, 307231))%>%
  mutate(pop.y = replace_na(pop.y, 479892))%>%
  mutate(vnum.x = replace_na(vnum.x, 0))%>%
  mutate(vnum.y = replace_na(vnum.y, 0))%>%
  mutate(country.x = replace_na(country.x, "Norway"))%>%
  mutate(resolution.x = replace_na(resolution.x, 1))%>%
  relocate(location, .before = date )%>%
  rename(country = "country.x")%>%
  rename(resolution = "resolution.x")%>%
  mutate(date.y = NULL)%>%
  mutate(location.x = NULL)%>%
  mutate(location.y = NULL)%>%
  mutate(country.y = NULL)%>%
  mutate(resolution.y = NULL)


W_Norway$pop <- as.numeric(W_Norway$pop.x) + as.numeric(W_Norway$pop.y)
W_Norway$vnum <- as.numeric(W_Norway$vnum.x) + as.numeric(W_Norway$vnum.y)
W_Norway <- W_Norway %>%
  relocate(pop, .after = location)%>%
  mutate(pop.x = NULL)%>%
  mutate(pop.y = NULL)%>%
  mutate(vnum.x = NULL)%>%
  mutate(vnum.y = NULL)%>%
  mutate(vcum.x = NULL)%>%
  mutate(vcum.y = NULL)%>%
  mutate(vperc.x = NULL)%>%
  mutate(vperc.y = NULL)%>%
  add_column(vcum = NA, .after = "vnum") %>%
  add_column(vperc = NA, .after = "vcum")





total <- rbind(Agder_Rogaland, Hedmark_Oppland, N_Norway, SE_Norway, W_Norway, Trøndelag)

View(total)

write_xlsx(total,"Desktop/NO_Set_1.xlsx")



### Checking Purpose ####

# dfWestern_Norway <- data.frame(unclass(table(Western_Norway$vaccine, Western_Norway$dose)))%>%
#   add_row( .before= 1)
# 
# dfSouth_Eastern_Norway <- data.frame(unclass(table(South_Eastern_Norway$vaccine, South_Eastern_Norway$dose)))%>%
#   add_row( .before= 1)
# 
# dfNorthern_Norway <- data.frame(unclass(table(Northern_Norway$vaccine, Northern_Norway$dose)))%>%
#   add_row( .before= 1)
# 
# dfAgder_Rogaland <- data.frame(unclass(table(Agder_Rogaland$vaccine, Agder_Rogaland$dose)))%>%
#   add_row( .before= 1)

# see <- filter(Western_Norway, vaccine == "Covishield")
# see1 <- filter(Vestland, vaccine == "Covishield")
# see2 <- filter(MøreogRomsdal, vaccine == "Covishield")
# View(see)
# View(see1)
# View(see2)



#write_xlsx(hope,"Desktop/hope.xlsx")
#write_xlsx(Viken,"Desktop/Viken.xlsx")


# "Agder and Rogaland") ~ "Agder,Rogaland",
# "Hedmark and Oppland") ~ "Hedmark,Oppland", >>Innlandet
# "Northern Norway") ~ "Nordland,Troms og Finnmark",
# "Oslo and Akershus") ~ "Oslo (f),Akershus",
# "South-Eastern Norway") ~ "Viken,,Vestfold og Telemark",
# "Western Norway") ~ ",Vestland,Møre og Romsdal"
# Trøndelag



