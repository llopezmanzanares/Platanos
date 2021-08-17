library(tidyverse)
library(lubridate)

ds <- read_delim("data/raw/dataset-ISTAC-C00014A_000011-1.5-observations.tsv",
                 show_col_types = F) %>% 
  rename_all(tolower) %>% 
  filter(
    territorio == "Tenerife",
    medidas == "Precio medio ponderado",
    !is.na(obs_value)
  ) %>% 
  select(1, 6) %>% 
  rename(
    fecha = time_period,
    pmp = obs_value
  ) %>% 
  mutate(
    fecha = str_c("01/", fecha),
    fecha = dmy(fecha)
    ) %>% 
  write_csv("data/processed/pr_tfe_mes.csv")
