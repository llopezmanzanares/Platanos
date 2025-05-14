# Extracción y pre-procesado de los datos de la finca

# Fuentes de los datos:
# - Coooperativa
# - BALTEN
# - Facturas de Proveedores
# - ISTAC

# Versión: 2025-05-09


# Set up ------------------------------------------------------------------

library(tidyverse)
library(here)
library(readxl)

## Constantes -------------------------------------------------------------

dirs <- list(
  raw = "data/raw",
  cop = "data/raw/coop",
  bal = "data/raw/balten",
  pro = "data/processed",
  cod = "code",
  eda = "eda"
)


# datos_balten ------------------------------------------------------------

source(here(dirs$cod, "get_balten_data.R"))


# datos_coop_liq ----------------------------------------------------------

# extraigo también otros gastos e ingresos
source(here(dirs$cod, "get_coop_data.R"))


# istac_platanos ----------------------------------------------------------

# TODO revisar el script
source(here(dirs$cod, "get_istac_data.R"))


# Otras acciones ----------------------------------------------------------

# source(here(dirs$eda, "coop_graficas.R"))
# source(here(dirs$eda, "istac_graficas.R"))


# Limpieza ----------------------------------------------------------------

rm(file_otros)
