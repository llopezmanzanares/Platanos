library(tidyverse)

ds <- read_delim("data/raw/C00013A_0014.txt",
                 skip = 9,
                 col_names = c("anualidad", "secano", "regadio"),
                 col_types = "ccc",
                 col_select = c(1,3)) %>% 
  filter(
    !is.na(regadio)
  ) %>% 
  write_csv("data/processed/supcult_tfe.csv")
