# Análisis de los datos de producción de la finca, en base a los 
# documentos de la Cooperativa

# Versión: 2023-08-31

# Packages ------

library(tidyverse)
# library(lubridate)
# library(pdftools)
library(here)

# Constantes --------------------------------------------------------------

coop_ds <- list()

dirs <- list(
  raw = "data/raw",
  cop = "data/raw/coop",
  pro = "data/processed",
  cod = "code"
)


# Functions ---------------------------------------------------------------

source(here(dirs$cod, "get_coop_data.R"))
