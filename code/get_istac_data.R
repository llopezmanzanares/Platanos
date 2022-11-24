# Lectura de datos de las diferentes fuentes


# Carga de librerías ------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(ISOweek)

# Variables ---------------------------------------------------------------

istac_ds <- list()  # los datos descargados de ISTAC

# Precios -----------------------------------------------------------------

istac_ds$precios_sem <- read_xlsx(
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

istac_ds$toneladas <- read_xls(
  here("data/raw", "toneladas_anuales_islas.xls"),
  skip = 8,
  n_max = 11,
  col_names = c("anualidad", "canarias", "lanzarote", "fuerteventura", "gran.canaria",
                "tenerife", "la.gomera", "la.palma", "el.hierro")
) %>% 
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

istac_ds$exportaciones <- read_xlsx(
  here("data/raw", "exportaciones_mensuales.xlsx"),
  skip = 10,
  col_names = c("isla", "periodo", "total", "españa (excluida canarias)", "extranjero")
  # .name_repair = tolower
) %>% 
  drop_na(total) %>% 
  fill(isla) %>% 
  mutate(
    mes = str_c("01/", periodo, sep = "") %>% 
      dmy(quiet = T) %>% 
      rollforward()
  ) %>% 
  filter(!is.na(mes)) %>% 
  select(periodo, mes, isla, total, peninsula = `españa (excluida canarias)`, extranjero) %>% 
  mutate(
    anualidad = year(mes),
    trimestre = quarter(mes, type = "date_last"),
    .after = 2
  )

# Consumo interno ---------------------------------------------------------

# TODO ver qué ha pasado con estos datos, que la gráfica se ha dado la vuelta

istac_ds$prodvsexps <- 
  left_join(
    istac_ds$toneladas %>% 
      filter(territorio == "canarias") %>% 
      select(-territorio) %>% 
      mutate(anualidad = as.numeric(anualidad)) %>% 
      rename(produccion = tn),
    istac_ds$exportaciones %>% 
      filter(
        isla == "Canarias",
        anualidad > 2011
      ) %>% 
      group_by(anualidad) %>% 
      summarise(
        exports = sum(total),
        .groups = "drop"
      ),
    by = "anualidad"
  ) %>% 
  mutate(
    interno = produccion - exports,
    int_prc = round(interno / produccion * 100, 2)
  )

# Superficie cultivada ----------------------------------------------------

istac_ds$superficie <- read_xls(
  path = here("data/raw/superficie_cultivada_islas.xls"),
  skip = 8
) %>% 
  filter(!is.na(territorio)) %>% 
  mutate(
    anualidad = as.numeric(anualidad),
    `Regadío: Que aún no produce` = str_replace(`Regadío: Que aún no produce`, ",", "\\.") %>% 
      as.numeric(),
    `Regadío: En producción` = str_remove(`Regadío: En producción`, "\\.") %>% 
      str_replace(",", "\\.") %>% as.numeric(),
    `CULTIVO PROTEGIDO` = str_remove(`CULTIVO PROTEGIDO`, "\\.") %>% 
      str_replace(",", "\\.") %>% as.numeric()
  )

# Guardo los datos --------------------------------------------------------

save(istac_ds,
     file = here("data/processed", "istac_platanos.RData"))
