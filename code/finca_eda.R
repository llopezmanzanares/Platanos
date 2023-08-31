# Análisis de los datos de producción de la finca, en base a los 
# documentos de la Cooperativa
# Cargo los datos de los PDF y genero una serie de gráficas para
# los informes de evolución

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
  cod = "code",
  eda = "eda"
)


# Functions ---------------------------------------------------------------


# Acciones ----------------------------------------------------------------

# Genero el conjunto de datos con las lecturas semanales y acumulados mensuales.
# Se crea el archivo de datos procesados "datos_finca.RData"

source(here(dirs$cod, "get_coop_data.R"))

# Genero el conjunto de datos con las gráficas
# se crea el archivo de datos procesados "graficas_finca.RData"

source(here(dirs$eda, "coop_graficas.R"))
