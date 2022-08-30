# datos de producción proporcionados por la cooperativa

library(tidyverse)
library(lubridate)
library(pdftools)
library(here)

# funciones de ayuda
xtr_num <- function(txt, patron){
  numero <- str_extract(txt, patron) %>% 
    str_replace(",", ".") %>% 
    as.numeric()
  
  return(numero)
}


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
    premium_kg = xtr_num(premium, "\\d+"),
    premium_pc = xtr_num(premium, "\\d{1,2},\\d{1,2}"),
    premium_eurkg = xtr_num(premium, "\\d,\\d{4}"),
    premium_eur = xtr_num(premium, "\\d+,\\d{2}$"),
    # categoría superior
    psup_kg = xtr_num(psup, "\\d+"),
    psup_pc = xtr_num(psup, "\\d{1,2},\\d{1,2}"),
    psup_eurkg = xtr_num(psup, "\\d,\\d{4}"),
    psup_eur = xtr_num(psup, "\\d+,\\d{2}$"),
    # categoría segunda
    segunda_kg = xtr_num(segunda, "\\d+"),
    segunda_pc = xtr_num(segunda, "\\d{1,2},\\d{1,2}"),
    segunda_eurkg = xtr_num(segunda, "\\d,\\d{4}"),
    segunda_eur = xtr_num(segunda, "\\d+,\\d{2}$"),
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
