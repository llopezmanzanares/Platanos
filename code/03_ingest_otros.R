# ******************************************************************************
# SCRIPT: 03_ingest_otros.R
# PROYECTO: Control y análisis financiero de la finca de plátanos
# FECHA DE CREACIÓN:   30/03/2026
# ÚLTIMA MODIFICACIÓN: 04/04/2026
# DESCRIPCIÓN:
#   Importación de datos de otros ingresos y gastos, archivo EXCEL
#
#   El script se ejecuta desde 00_run_all.R
#
# ENTRADAS:
#   - data/raw/00_Control_gastos-otrosingresos.xlsx

# SALIDAS:
#   - Objetos: gastos_otrosingresos$gastos, gastos_otrosingresos$ingresos
#   - Archivo RDS con los datos data/processed/gastos_otros-ingresos.rds
#   - Mensajes de progreso y validación en consola

# DEPENDENCIAS:
#   - tidyverse, here, glue, readxl

# ******************************************************************************

# 1. LIBRERÍAS ------------------------------------------------------------

suppressPackageStartupMessages(
  library(readxl)
)

# 2. EJECUCION ------------------------------------------------------------

otros_file <- here(dirs$raw, "00_Control_gastos-otrosingresos.xlsx")

if (length(otros_file) == 0) {
  stop("✗ ERROR: No se encontró el archivo EXCEL en: ", here(dirs$raw))
}

message("Cargando Gastos y Otros Ingresos...")

gastos_otrosingresos <- list(
  gastos   = read_xlsx(path = otros_file, sheet = "Gastos", skip = 2),
  ingresos = read_xlsx(path = otros_file, sheet = "Ingresos", skip = 2)
)

# 3. GUARDADO -------------------------------------------------------------

guardar_con_backup(gastos_otrosingresos, "data/processed/gastos_otros-ingresos.rds")

message("✓ Script 03_ingest_otros.R finalizado correctamente.\n")
