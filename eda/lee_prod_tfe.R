library(tidyverse)

ds <- read_delim("data/raw/C00013A_0020.txt",
                 skip = 5,
                 col_select = 2,
                 trim_ws = T) %>% 
  mutate(
    anualidad = as.numeric(str_sub(Tenerife, 1, 4)),
    prod = str_sub(Tenerife, 7, length(Tenerife)-1)
  ) %>% 
  filter(
    !is.na(anualidad),
    prod != ""
  ) %>% 
  select(-1) %>% 
  write_csv("data/processed/prod_tfe.csv")