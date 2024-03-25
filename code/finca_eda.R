# Análisis de los datos de producción de la finca, en base a los 
# documentos de la Cooperativa
# Cargo los datos de los PDF y genero una serie de gráficas para
# los informes de evolución

# Versión: 2024-03-25

# Packages ------

library(tidyverse)
# library(lubridate)
# library(pdftools)
library(here)

# Constantes --------------------------------------------------------------

coop_ds    <- list()
coop_grafs <- list()
balten_ds  <- list()

dirs <- list(
  raw = "data/raw",
  cop = "data/raw/coop",
  bal = "data/raw/balten",
  pro = "data/processed",
  cod = "code",
  eda = "eda"
)


# Functions ---------------------------------------------------------------


# Acciones ----------------------------------------------------------------

# Genero el conjunto de datos con las lecturas semanales y acumulados mensuales.

# Se crea el archido de datos procesados "datos_balten.RData" con los consumos de agua
source(here(dirs$cod, "get_balten_data.R"))
# Se crea el archivo de datos procesados "datos_finca.RData" con los datos de la cooperativa
source(here(dirs$cod, "get_coop_data.R"))

# Genero el conjunto de datos con las gráficas
# se crea el archivo de datos procesados "graficas_finca.RData"

source(here(dirs$eda, "coop_graficas.R"))

# Genero el conjunto de datos descargados de ISTAC
# Se crea el archivo de datos procesados "istac_platanos.RData"
source(here(dirs$cod, "get_istac_data.R"))

# Genero el conjunto de datos de las gráficas
# Se crea el archivo de datos procesados "istac_graficas.RData"
source(here(dirs$eda, "istac_graficas.R"))
