# datos de producción proporcionados por la cooperativa

library(tidyverse)
library(lubridate)
library(pdftools)
library(here)

# Constantes --------------------------------------------------------------

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
  path = here("data/raw", "coop"),
  pattern = "^L"
  )

ds <- 
  map(here("data/raw", "coop", data_files), pdf_text) %>% 
  unlist() %>% 
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
    premium_eurkg = xtr_num(premium, patrones$eurkg),
    premium_eur =   xtr_num(premium, patrones$eur),
    # categoría superior
    psup_kg =    xtr_num(psup, patrones$kg),
    psup_pc =    xtr_num(psup, patrones$prc),
    psup_eurkg = xtr_num(psup, patrones$eurkg),
    psup_eur =   xtr_num(psup, patrones$eur),
    # categoría segunda
    segunda_kg =    xtr_num(segunda, patrones$kg),
    segunda_pc =    xtr_num(segunda, patrones$prc),
    segunda_eurkg = xtr_num(segunda, patrones$eurkg),
    segunda_eur =   xtr_num(segunda, patrones$eur),
    .keep = "unused"
  )

cosa <- ds %>% 
  filter(
    str_detect(value, "Semana|Fecha|PREMIUM|P. SU|SEGUNDA")
    ) %>% 
  mutate(
    medida1 = str_extract(value, pattern = "^[:graph:]+"),
    .before = 1
  )
