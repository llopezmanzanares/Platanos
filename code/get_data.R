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
  filter(!is.na(precio)) 

tfe_precios_sem <- precios_sem
