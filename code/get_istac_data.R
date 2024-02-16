# Lectura de datos de las diferentes fuentes

# Versión: 2024-02-15


# Carga de librerías ------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(ISOweek)

# Variables ---------------------------------------------------------------

istac_ds <- list()  # los datos descargados de ISTAC

# Precios -----------------------------------------------------------------

istac_ds$precios_sem <- 
  read_xlsx(
    path = here::here("data/raw/precios_medios_percibidos.xlsx"),
    skip = 10,
    col_names = c("semana", "Tenerife", "La Palma", "Gran Canaria", "Canarias")
  ) |> 
  mutate(
    aa  = str_extract(semana, pattern = "[:digit:]+") |> as.numeric(),
    sem = str_extract(semana, pattern = "[:digit:]+$") |> as.numeric(),
    .keep = "unused"
  ) |> 
  filter(!is.na(aa)) |> 
  pivot_longer(cols = Tenerife:Canarias, names_to = "territorio", values_to = "precio")

# Toneladas anuales -------------------------------------------------------

istac_ds$toneladas <- 
  read_xlsx(
    here("data/raw", "toneladas_anuales_islas.xlsx"),
    skip = 10,
    col_names = c("tipo","aa", "Canarias", "Lanzarote", "Fuerteventura", "Gran Canaria",
                  "Tenerife", "La Gomera", "La Palma", "El Hierro")
  )  |>  
  filter(!is.na(Tenerife)) |>
  select(-tipo) |> 
  mutate(Canarias = as.numeric(Canarias)) |> 
  pivot_longer(
    !aa,
    names_to = "territorio",
    values_to = "tn"
  )

# Exportaciones mensuales -------------------------------------------------

istac_ds$exportaciones <- 
  read_xlsx(
    here("data/raw", "exportaciones_mensuales.xlsx"),
    skip = 10,
    col_names = c("isla", "periodo", "total", "españa (excluida canarias)", "extranjero")
  )  |>  
  fill(isla) |>  
  drop_na(total) |>  
  mutate(
    mes = str_c("01/", periodo, sep = "") |>  
      dmy(quiet = T) |>  
      rollforward()
  )|> 
  # quito los agregados anuales
  filter(!is.na(mes))|> 
  select(periodo, mes, isla, total, peninsula = `españa (excluida canarias)`, extranjero) |>  
  mutate(
    aa = year(mes),
    trimestre = quarter(mes, type = "date_last"),
    .after = 2
  )

# Consumo interno ---------------------------------------------------------

# TODO ver qué ha pasado con estos datos, que la gráfica se ha dado la vuelta

istac_ds$prodvsexps <- 
  left_join(
    istac_ds$toneladas |>  
      filter(territorio == "Canarias") |>  
      select(-territorio) |>  
      mutate(aa = as.numeric(aa)) |>  
      rename(produccion = tn),
    istac_ds$exportaciones |>  
      filter(
        isla == "Canarias",
        aa > 2011
      )  |>  
      summarise(
        exports = sum(total),
        .by = aa
      ),
    join_by(aa)
  ) |>  
  mutate(
    interno = produccion - exports,
    int_prc = round(interno / produccion * 100, 2)
  )

# Superficie cultivada ----------------------------------------------------

istac_ds$superficie <- 
  read_xlsx(
    path = here("data/raw/superficie_cultivada_islas.xlsx"),
    skip = 10,
    col_names = c("territorio", "aa", "ha")
  )|> 
  filter(!is.na(ha)) %>%
  fill(territorio) |> 
  mutate(aa = as.numeric(aa))

# Guardo los datos --------------------------------------------------------

save(istac_ds,
     file = here("data/processed", "istac_platanos.RData"))
