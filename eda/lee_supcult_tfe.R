library(tidyverse)

ds <- read_csv("data/raw/C00013A_0014.txt",
                 skip = 7,
                 col_names = c("anualidad", "hectareas")
                 ) %>% 
   mutate(
    anualidad = as.numeric(anualidad)
  ) %>% 
 filter(
    !is.na(anualidad)
  ) %>% 
  write_csv("data/processed/supcult_tfe.csv")
