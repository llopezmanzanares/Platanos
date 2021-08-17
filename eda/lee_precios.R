library(tidyverse)

ds <- read_delim("data/raw/dataset-ISTAC-C00014A_000012-1.3-observations.tsv",
                 show_col_types = FALSE) %>% 
  rename_all(tolower) %>% 
  filter(
    territorio == "Tenerife",
    medidas == "Precio medio ponderado",
    !is.na(obs_value)
  ) %>% 
  select(1,6) %>% 
  rename(
    fecha = time_period,
    pmp = obs_value
  ) %>%
  mutate(
    año = str_extract(fecha, "\\d+"),
    sem = str_extract(fecha, "\\d+$"),
    .before = 1
  ) %>%
  select(-3) %>% 
  write_csv("data/processed/pr_tfe.csv")
