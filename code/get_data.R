# Lectura de datos de las diferentes fuentes


# Carga de librerías ------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(readxl)


# Precios -----------------------------------------------------------------

precios_sem <- read_xlsx(
  here("data/raw", "precios_medios_percibidos.xlsx"),
  skip = 9) %>% 
  rename(periodo = "...1") %>% 
  pivot_longer(
    !periodo,
    names_to = "territorio",
    values_to = "precio"
  ) %>% 
  filter(!is.na(precio)) %>% 
  mutate(
    period = str_replace(periodo, pattern = "Semana", replacement = "week"),
    semana = as_date(periodo, format = "%Y Semana %V")
    # semana = ceiling_date(period, unit = "week")
    # anualidad = str_extract(periodo, pattern = "\\d+"),
    # semana = str_extract(periodo, pattern = "\\d+$")
  )

tfe_precios_sem <- 
  filter(precios_sem, territorio == "Tenerife")
