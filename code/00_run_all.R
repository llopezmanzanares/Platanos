# ******************************************************************************
# SCRIPT: 00_run_all.R
# PROYECTO: Control y análisis financiero de la finca de plátanos
# FECHA DE CREACIÓN:   24/03/2026
# ÚLTIMA MODIFICACIÓN: 30/03/2026
#
# DESCRIPCIÓN:
#   Script maestro que orquesta la ejecución completa del pipeline de análisis.
#
# SALIDAS:
#   - Todos los archivos RDS de los scripts individuales
#
# DEPENDENCIAS:
#   - tidyverse, here, glue
#   - Scripts:
#     + code: 01_ingest_coop.R,  02_ingest_balten.R
#             11_process_coop.R, 12_process_balten.R
#     + eda:  21_eda_coop.R,     22_eda_balten.R
# ******************************************************************************


# 0. CONFIG INICIAL -------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(glue)
})

# directorios

dirs <- list(
  raw = "data/raw",
  cop = "data/raw/coop",
  bal = "data/raw/balten",
  pro = "data/processed",
  cod = "code"
)

# Verificar que existen las carpetas necesarias
walk(dirs, \(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE))

# Cargo funciones auxiliares
source(here("code/helpers/helpers_grales.R"))
source(here("code/helpers/helpers_ingest.R"))

# 1. Ingesta -----------------------------------------------------------------

message("\n════════════════════════════════════════════════════════════════")
message("  INICIO: Importación de datos de la finca")
message("════════════════════════════════════════════════════════════════\n")

# Lee PDFs y extrae datos
source(here(dirs$cod, "01_ingest_coop.R")) # guarda coop_raw.rds
source(here(dirs$cod, "02_ingest_balten.R")) # guarda balten_raw.rds

# Lee Excel de Otros ingresos y gastos
source(here(dirs$cod, "03_ingest_otros.R")) # guarda gastos_otros-ingresos.rds

# source(here("code", "03_ingest_istac.R"))  # TODO: pendiente

# 2. Procesado ---------------------------------------------------------------

# 3. EDA ---------------------------------------------------------------------

source(here("eda", "21_eda_finca.R")) # guarda finca_eda.rds
