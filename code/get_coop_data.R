# datos de producción proporcionados por la cooperativa

library(tidyverse)
library(lubridate)
library(pdftools)
library(here)

# Constantes --------------------------------------------------------------

dir <- list(
  raw = "data/raw",
  pro = "data/processed"
)

patrones <- list(
  kg    = "\\d+",              # es un número entero de varias cifras
  prc   = "\\d{1,2},\\d{1,2}", # número de 2 decimales
  eurkg = "\\d,\\d{4}",        # número con 4 decimales
  eur   = "\\d+,\\d{2}$"       # último número, tiene 2 decimales
)

# Funciones ---------------------------------------------------------------
xtr_num <- function(txt, patron){
  numero <- str_extract(txt, patron) %>% 
    str_replace(",", ".") %>% 
    as.numeric()
  
  return(numero)
}

# Leo y transformo --------------------------------------------------------

data_files <- list.files(
  path = here(dir%raw, "coop"),
  pattern = "^L"
  )

ds <- 
  map(
    here(dir$raw, data_files),
    pdf_text
  ) %>% 
  str_split(pattern = "\\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  filter(str_detect(value, pattern = "Fecha|Semana|PREMIUM|P\\. SUPER|SEGUNDA|racimos|medio")) %>% 
  mutate(
    value  = str_to_lower(value),
    medida = str_extract(value, pattern = "[:graph:]+")
  ) 



ds <- 
  map(here(dir$raw, "coop", data_files), pdf_text) %>% 
  str_split(pattern = "\\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  filter(str_detect(value, pattern = "Fecha|Semana|PREMIUM|P\\. SUPER|SEGUNDA|racimos|medio")) %>% 
  mutate(
    value  = str_to_lower(value),
    medida = str_extract(value, pattern = "[:graph:]+")
  ) %>% 
  pivot_wider(
    names_from  = medida,
    values_from = value
  ) %>% 
  rename(
    psup        = p.,
    racimos     = total,
    pesomedio   = peso,
    preciomedio = precio
  ) %>% 
  mutate(
    # info gral
    fecha = str_extract(fecha, pattern = "(\\d{2}\\.?){3}") %>% 
      str_replace(pattern = "\\.", replacement = "-") %>% 
      dmy(),
    semana = xtr_num(semana, "\\d{1,2}"),
    racimos = xtr_num(racimos, "\\d{1,2}"),
    pesomedio   = xtr_num(pesomedio, "\\d+,\\d{2}"),
    preciomedio = xtr_num(preciomedio, "\\d,\\d{1,4}"),
    # categoría premium
    premium_kg =    xtr_num(premium, patrones$kg),
    premium_pc =    xtr_num(premium, patrones$prc),
    premium_eurkg = xtr_num(premium, patrones$eurkg),ds <- 
  map(
    here(dir$raw, data_files),
    pdf_text
  ) %>% 
  str_split(pattern = "\\n") %>% 
  unlist() %>% 
  as_tibble() %>% 
  filter(str_detect(value, pattern = "Fecha|Semana|PREMIUM|P\\. SUPER|SEGUNDA|racimos|medio")) %>% 
  mutate(
    value  = str_to_lower(value),
    medida = str_extract(value, pattern = "[:graph:]+")
  ) 

# voy extrayendo los subconjuntos de datos
fechas <- filter(ds, medida == "fecha") %>% 
  mutate(
    fecha = str_extract(value, pattern = "(\\d{2}\\.?){3}") %>% 
      str_replace(pattern = "\\.", replacement = "-") %>% 
      dmy(),
    .keep = "none"
  )
semanas <- filter(ds, medida == "semana") %>% 
  mutate(
    semana = xtr_num(value, "\\d{1,2}"),
    .keep = "none"
  )
racimos <- filter(ds, medida == "total") %>% 
  mutate(
    racimos = xtr_num(value, "\\d{1,2}"),
    .keep = "none"
  )
peso_med <- filter(ds, medida == "peso") %>% 
  mutate(
    peso_med = xtr_num(value, "\\d+,\\d{2}"),
    .keep = "none"
  )
prec_med <- filter(ds, medida == "precio") %>% 
  mutate(
    prec_med = xtr_num(value, "\\d,\\d{1,4}"),
    .keep = "none"
  )
premium <- filter(ds, medida == "premium") %>% 
  mutate(
    premium_kg =    xtr_num(value, patrones$kg),
    premium_pc =    xtr_num(value, patrones$prc),
    premium_eurkg = xtr_num(value, patrones$eurkg),
    premium_eur =   xtr_num(value, patrones$eur),
    .keep = "none"
  )
psup <- filter(ds, medida == "p.") %>% 
  mutate(
    psup_kg =    xtr_num(value, patrones$kg),
    psup_pc =    xtr_num(value, patrones$prc),
    psup_eurkg = xtr_num(value, patrones$eurkg),
    psup_eur =   xtr_num(value, patrones$eur),
    .keep = "none"
  )
segunda <- filter(ds, medida == "segunda") %>% 
  mutate(
    segunda_kg =    xtr_num(value, patrones$kg),
    segunda_pc =    xtr_num(value, patrones$prc),
    segunda_eurkg = xtr_num(value, patrones$eurkg),
    segunda_eur =   xtr_num(value, patrones$eur),
    .keep = "none"
  )


datos <- bind_cols(fechas, semanas, racimos, peso_med, prec_med, premium, psup, segunda)

rm(ds, fechas, semanas, racimos, peso_med, prec_med, premium, psup, segunda)


# Guardo el conjunto de datos ---------------------------------------------

save(datos, file = here(dir$pro, "datos_finca.RData"))

write_csv2(datos, file = here(dir$pro, "datos_finca.csv"))
