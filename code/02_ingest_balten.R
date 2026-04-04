# ******************************************************************************
# SCRIPT: 02_ingest_balten.R
# PROYECTO: Control y análisis financiero de la finca de plátanos
# FECHA DE CREACIÓN:   25/03/2026
# ÚLTIMA MODIFICACIÓN: 30/03/2026
# DESCRIPCIÓN:
#   Importación de datos de facturación de agua (BALTEN), archivos PDFs
#
#   El script se ejecuta desde 00_run_all.R
#
# ENTRADAS:
#   - data/raw/balten/*.pdf

# SALIDAS:
#   - Objetos: balten_ds$todos, balten_ds$bimensuales
#   - Archivo RDS con los datos data/processed/balten_raw.rds
#   - Mensajes de progreso y validación en consola

# DEPENDENCIAS:
#   - tidyverse, here, glue, pdftools

# ******************************************************************************

# 0. LIBRERIAS ------------------------------------------------------------

suppressPackageStartupMessages(
  library(pdftools)
)

# 1. CONSTANTES -----------------------------------------------------------

# Patrones regex para extraer valores de las facturas BALTEN
patrones_balten <- list(
  l_ant   = "(?<=Lectura Anterior: )\\d+", # lectura anterior contador
  f_ant   = "(?<=Fecha: )(\\d{2}/){2}\\d{4}", # fecha lectura anterior
  l_act   = "(?<=Lectura Actual: )\\d+", # lectura actual contador
  f_act   = "(\\d{2}/){2}\\d{4}(?=\\s+Con)", # fecha lectura actual
  consumo = "\\d+,\\d+(?=\\s*m)", # consumo en m³
  precio  = "(?<=precio\\s{1,10})\\d,\\d+", # precio unitario €/m³
  total   = "(?<=Total )(\\d\\.)?\\d{1,3},\\d{2}" # importe total factura
)

# 2. FUNCIONES ------------------------------------------------------------

# 3. EJECUCION ------------------------------------------------------------

balten_ds <- list()

balten_files <- list.files(path = here(dirs$bal), full.names = TRUE)

if (length(balten_files) == 0) {
  stop("✗ ERROR: No se encontraron los archivos PDF en: ", here(dirs$bal))
}

message("Cargando facturas de BALTEN, ", length(balten_files), " archivos PDF...")


balten_raw <-
  map(balten_files, pdf_text) |>
  set_names(basename(balten_files)) |>
  enframe(name = "archivo", value = "contenido") |>
  mutate(lineas = map(contenido, ~ str_split(.x, "\\n")[[1]])) |>
  unnest(lineas) |>
  filter(str_detect(lineas, "Lectura Ant|Agua Re"))

balten_ds$todos <-
  balten_raw |>
  mutate(
    l_ant      = as.numeric(str_extract(lineas, pattern = patrones_balten$l_ant)),
    f_ant      = dmy(str_extract(lineas, pattern = patrones_balten$f_ant)),
    l_act      = as.numeric(str_extract(lineas, pattern = patrones_balten$l_act)),
    f_act      = dmy(str_extract(lineas, pattern = patrones_balten$f_act)),
    consumo_m3 = xtr_num(txt = lineas, patron = patrones_balten$consumo),
    precio     = xtr_num(txt = lineas, patron = patrones_balten$precio),
    total      = xtr_num(txt = lineas, patron = patrones_balten$total)
  ) |>
  fill(precio, total, .direction = "up") |>
  drop_na(f_act) |>
  select(-contenido, -lineas, -archivo) |>
  arrange(f_act)

balten_ds$bimensuales <-
  balten_ds$todos |>
  select(fecha = f_act, consumo_m3, precio, total_eur = total)


message("\n ✓ Extracción completada: ", nrow(balten_raw), " observaciones.\n")

# 4. GUARDAR --------------------------------------------------------------

guardar_con_backup(coop_raw, "data/processed/balten_raw.rds")

message("✓ Script 02_ingest_balten.R finalizado correctamente.\n")
