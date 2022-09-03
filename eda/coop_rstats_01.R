# algunas estadísticas básicas

# la mayoría de los datos interesantes vienen en los pdf, sólo hacen falta gráficas

library(tidyverse)
library(here)

dir <- list(
  raw = "data/raw",
  pro = "data/processed"
)

load(here(dir$raw, "datos_finca.RData"))

ds <- datos %>% 
  select(fecha, ends_with("_kg")) %>% 
  mutate(
    mes = str_c(lubridate::year(fecha), lubridate::month(fecha), "01", sep = "/") %>% 
      lubridate::ymd() %>% 
      lubridate::rollforward(),
    .keep = "unused",
    .before = 1
  ) %>% 
  group_by(mes) %>% 
    summarise(
      pre_kg = sum(premium_kg),
      psup_kg = sum(psup_kg),
      seg_kg = sum(segunda_kg),
      total_kg = rowSums()
    )
