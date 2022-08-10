# Lectura de datos de las diferentes fuentes


# Carga de librerías ------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(ISOweek)


# Precios -----------------------------------------------------------------

precios_sem <- read_xlsx(
  here("data/raw", "precios_medios_percibidos.xlsx"),
  skip = 10,
  col_names = c("periodo", "canarias", "gran.canaria", "tenerife", "la.palma")
  ) %>% 
  pivot_longer(
    !periodo,
    names_to = "territorio",
    values_to = "precio"
  ) %>% 
  filter(!is.na(precio)) %>% 
  mutate(
    semana = str_replace(periodo,
                         pattern = "(\\d+)(\\s\\w+\\s)(\\d+)",
                         replacement = "\\1-W\\3-1") %>% 
      ISOweek2date(),
    anualidad = year(semana),
    mes = month(semana, label = T),
    trimestre = quarter(semana, type = "date_last"),
    .after = 1
  )

# Toneladas anuales -------------------------------------------------------

toneladas <- read_xls(
  here("data/raw", "toneladas_anuales_islas.xls"),
  skip = 7,
  n_max = 11
) %>% 
  rename(anualidad = "...1") %>% 
  filter(anualidad != "Plátano") %>% 
  pivot_longer(
    !anualidad,
    names_to = "territorio",
    values_to = "tn"
  ) %>% 
  mutate(
    tn = str_replace(tn, pattern = "\\.", replacement = "") %>% 
      str_replace(pattern = ",", replacement = "\\.") %>% 
      as.numeric()
  )

# Exportaciones mensuales -------------------------------------------------

exportaciones <- read_xlsx(
  here("data/raw", "exportaciones_mensuales.xlsx"),
  skip = 7
) %>% 
  mutate(
    mes = str_c("01/", periodo, sep = "") %>% 
      dmy(quiet = T) %>% 
      rollforward()
  ) %>% 
  filter(!is.na(mes))

# Guardo los datos --------------------------------------------------------

save(exportaciones, precios_sem, toneladas, 
     file = here("data/processed", "platanos.RData"))
