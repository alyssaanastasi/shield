library(tidyverse)

paramsTableAll <- read_csv("../../Data/data_figures/paramsTable.csv")

dataPlacesE <- paramsTableAll %>% 
  mutate(type = case_when(Group == 1 ~ "Sigmoidal", TRUE ~ "Concave")) %>%
  filter(SES == "H", WVH >= 5, WVH <=30) %>% select(type, Wh_H = WVH, Vm_H = MaxVac) %>%
  ## need to round Vm to nearest even step
  mutate(Wh_H = as.character(round(Wh_H)), Vm_H = as.character(round(Vm_H, 2)))

data4E <- read_csv("../../Data/data_figures/outcomes_4E.csv") %>%
  mutate(ScenarioVm = recode(ScenarioVm, `best case` = "Equity in Vm: best case",
                             `worst case` = "Disparity in Vm: worst case"),
         ScenarioWh = recode(ScenarioWh, `best case` = "Equity in Wh: best case",
                             `worst case` = "Disparity in Wh: worst case"),
         type = recode(type, Cocave = "Concave"),
         ScenarioVm = fct_relevel(ScenarioVm, "Equity in Vm: best case"),
         ScenarioWh = fct_relevel(ScenarioWh, "Equity in Wh: best case")) %>%
  filter(ScenarioVm == "Equity in Vm: best case" & ScenarioWh == "Equity in Wh: best case") %>%
  select(type, Vm_H, Wh_H, PercInfAdverted) %>% distinct() %>%
  mutate(Vm_H = as.character(Vm_H), Wh_H = as.character(Wh_H))

dataPlacesAverted <- left_join(dataPlacesE, data4E, by = c("type", "Vm_H", "Wh_H")) %>%
  arrange(Vm_H)

## these are the odd Vm's that don't match anything on the grid
checkNA <- dataPlacesAverted %>% filter(is.na(PercInfAdverted)) %>%
  mutate(PercInfAdverted = NULL) %>%
  left_join(data4E, by = c("type", "Vm_H", "Wh_H"))

dataPlacesAverted <- dataPlacesAverted %>% filter(!is.na(PercInfAdverted)) %>%
  group_by(type) %>%
  mutate(MaxAverted = max(PercInfAdverted), MinAverted = min(PercInfAdverted), MeanAverted = mean(PercInfAdverted))

write_csv(dataPlacesAverted, "../../Data/data_figures/places_individual_averted.csv")

