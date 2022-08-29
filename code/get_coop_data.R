# datos de producción proporcionados por la cooperativa

library(tidyverse)
library(lubridate)
library(pdftools)
library(here)

data_files <- list.files(
  path = here("data/raw", "coop"),
  pattern = "^L"
  )

ds <- 
  map(here("data/raw", "coop", data_files), pdf_text) %>% 
  unlist() %>% 
  str_split(pattern = "\\n") %>% 
  unlist() %>% 
  as_tibble()

cosa <- ds %>% 
  filter(
    str_detect(value, "Semana|Fecha|PREMIUM|P. SU|SEGUNDA"))
