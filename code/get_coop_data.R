# Datos de producción proporcionados por la cooperativa
# Extraigo la información de todos los pdf cada vez

# Versión: 2023-04-17

# Packages ------

# library(tidyverse)
library(lubridate)
library(pdftools)
# library(here)

# Constantes --------------------------------------------------------------

# coop_ds <- list()
# 
# dirs <- list(
#   raw = "data/raw",
#   cop = "data/raw/coop",
#   pro = "data/processed"
# )

patrones <- list(
  kg    = "(\\d\\.)?\\d+",         # los miles con un punto
  prc   = "\\d{1,2},\\d{1,2}",     # número de 2 decimales
  eurkg = "\\d,\\d{4}",            # número con 4 decimales
  eur   = "(\\d\\.)?\\d+,\\d{2}$"  # último número, tiene 2 decimales
)

# Funciones ---------------------------------------------------------------
xtr_num <- function(txt, patron){
  numero <- str_extract(txt, patron) %>% 
    str_replace("\\.", "") %>%   # en el caso de que sea > 1000 tiene un punto
    str_replace(",", ".") %>% 
    as.numeric()
  
  return(numero)
}

read_pdfs <- function(datafiles) {
  ds_coop <- 
    map(
      here(dirs$cop, datafiles),
      pdf_text
    ) %>% 
    str_split(pattern = "\\n") %>% 
    unlist() %>% 
    as_tibble() %>% 
    filter(str_detect(value, pattern = "Fecha|Semana|PREMIUM|P\\. SUPER|SEGUNDA|Total|racimos|medio")) %>% 
    mutate(
      value  = str_to_lower(value),
      medida = case_when(
        str_detect(value, "fecha")         ~ "fecha",
        str_detect(value, "semana")        ~ "semana",
        str_detect(value, "premium")       ~ "premium",
        str_detect(value, "p\\. super")    ~ "psup",
        str_detect(value, "segunda")       ~ "segunda",
        str_detect(value, "total \\.")     ~ "totales",
        str_detect(value, "total racimos") ~ "racimos",
        str_detect(value, "peso medio")    ~ "peso",
        str_detect(value, "precio medio")  ~ "precio",
        str_detect(value, "total eu")      ~ "total_euros"
      )
    ) 
  return(ds_coop)
}

xtr_sem_data <- function(datos_semanales){
  # voy extrayendo los subconjuntos de datos
  fechas <- filter(datos_semanales, medida == "fecha") %>% 
    mutate(
      fecha = str_extract(value, pattern = "(\\d{2,4}\\.?){3}") %>% 
        str_replace(pattern = "\\.", replacement = "-") %>% 
        dmy(),
      .keep = "none"
    )
  semanas <- filter(datos_semanales, medida == "semana") %>% 
    mutate(
      semana = xtr_num(value, "\\d{1,2}"),
      .keep = "none"
    )
  totales <- filter(datos_semanales, medida == "totales") %>% 
    mutate(
      total_kg = xtr_num(value, "(\\d\\.)?\\d+"),
      total_eur = xtr_num(value, "(\\d\\.)?\\d{1,3},\\d{2}$"),
      .keep = "none"
    )
  total_factura <- filter(datos_semanales, medida == "total_euros") %>% 
    mutate(
      total_fac = xtr_num(value, "(\\d\\.)?\\d{1,3},\\d{2}"),
      .keep = "none"
    )
  racimos <- filter(datos_semanales, medida == "racimos") %>% 
    mutate(
      racimos = xtr_num(value, "\\d{1,2}"),
      .keep = "none"
    )
  peso_med <- filter(datos_semanales, medida == "peso") %>% 
    mutate(
      peso_med = xtr_num(value, "\\d+,\\d{2}"),
      .keep = "none"
    )
  prec_med <- filter(datos_semanales, medida == "precio") %>% 
    mutate(
      prec_med = xtr_num(value, "\\d,\\d{1,4}"),
      .keep = "none"
    )
  premium <- filter(datos_semanales, medida == "premium") %>% 
    mutate(
      premium_kg =    xtr_num(value, patrones$kg),
      premium_pc =    xtr_num(value, patrones$prc),
      premium_eurkg = xtr_num(value, patrones$eurkg),
      premium_eur =   xtr_num(value, patrones$eur),
      .keep = "none"
    )
  psup <- filter(datos_semanales, medida == "psup") %>% 
    mutate(
      psup_kg =    xtr_num(value, patrones$kg),
      psup_pc =    xtr_num(value, patrones$prc),
      psup_eurkg = xtr_num(value, patrones$eurkg),
      psup_eur =   xtr_num(value, patrones$eur),
      .keep = "none"
    )
  segunda <- filter(datos_semanales, medida == "segunda") %>% 
    mutate(
      segunda_kg =    xtr_num(value, patrones$kg),
      segunda_pc =    xtr_num(value, patrones$prc),
      segunda_eurkg = xtr_num(value, patrones$eurkg),
      segunda_eur =   xtr_num(value, patrones$eur),
      .keep = "none"
    )
  
  semanales <- 
    bind_cols(fechas, semanas, racimos, totales, total_factura, peso_med, prec_med, premium, psup, segunda)
  
#  rm(ds, fechas, semanas, racimos, totales, total_factura, peso_med, prec_med, premium, psup, segunda)
  
  return(semanales)
}

# Leo y extraigo ----------------------------------------------------------

data_files <- list.files(
  path = here(dirs$cop),
  pattern = "^L"
  )

coop_ds$sem <- read_pdfs(data_files) %>% xtr_sem_data()

# Agregados mensuales -----------------------------------------------------

# Es más útil trabajar con los datos mensuales
# solo tengo un dato de 2020, así que lo elimino
coop_ds$mes <- 
  filter(coop_ds$sem, year(fecha) > 2020) %>% 
  select(
    !c(semana,             # el dato de la semana no aporta información
       total_fac,          # el total facturado es muy similar al importe, lo quito
       ends_with(c("_med", # tampoco las medias y porcentajes
                   "_pc",
                   "_eurkg")
       )
    )
  ) %>% 
  mutate(
    fecha = rollforward(fecha)
  ) %>% 
  group_by(fecha) %>% 
  summarise(
    across(everything(), sum),
    .groups = "drop"
  ) %>% 
  mutate(
    kg_rac = total_kg / racimos
  )

# Kg por meses ------------------------------------------------------------

coop_ds$mes_kg <- coop_ds$mes %>% 
  mutate(
    aa = year(fecha),
    mm = month(fecha, label = TRUE),
    .after = 1
  ) %>% 
  select(fecha:mm, ends_with("kg")) %>% 
  group_by(aa) %>% 
  mutate(
    across(ends_with("_kg"), cumsum, .names = "{.col}_acum")
  ) %>% 
  ungroup()

# Guardo el conjunto de datos ---------------------------------------------

save(coop_ds, file = here(dirs$pro, "datos_finca_test.RData"))

# write_csv2(coop_ds$sem, file = here(dirs$pro, "datos_finca.csv"))
