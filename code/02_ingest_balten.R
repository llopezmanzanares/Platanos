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
  n_factura = "AG\\s\\d+/\\d{4}", # Ejemplo AG 10316/2023
  todas_fechas = "(\\d{2}/){2}\\d{4}", # Todas las fechas para procesarlas luego
  # f_emision = "(?<=FECHA:\\n)\\d{2}/\\d{2}/\\d{4}", # Fecha de emisión de la factura
  periodo = "(?i)\\dº\\sBIMESTRE\\s\\d{4}", # Ejemplo: 4º BIMESTRE 2024
  l_ant = "(?<=Lectura Anterior: )\\d+", # lectura anterior contador
  # f_ant = "(?<=Fecha: )(\\d{2}/){2}\\d{4}", # fecha lectura anterior
  l_act = "(?<=Lectura Actual: )\\d+", # lectura actual contador
  # f_act = "(\\d{2}/){2}\\d{4}(?=\\s+Consumo)", # fecha lectura actual, antes de "Consumo"
  consumo = "(?<=Consumo \\(m³\\): )\\d+", # consumo en m³
  precio = "(?<=precio\\s{1,10})\\d,\\d+", # precio unitario €/m³
  total = "(?<=TOTAL )(\\d\\.)?\\d{1,3},\\d{2}" # importe total factura
)

# 2. FUNCIONES ------------------------------------------------------------

# 3. EJECUCION ------------------------------------------------------------

balten_ds <- list()

balten_files <- list.files(path = here(dirs$bal), full.names = TRUE)

if (length(balten_files) == 0) {
  stop("✗ ERROR: No se encontraron los archivos PDF en: ", here(dirs$bal))
}

message("Cargando facturas de BALTEN, ", length(balten_files), " archivos PDF...")

## 3.1 Lectura de los archivos --------------------------------------------

message("\n[1/2] Cargando datos de las facturas...")

balten_raw <-
  map(balten_files, pdf_text) |>
  set_names(basename(balten_files)) |>
  enframe(name = "archivo", value = "contenido")

## 3.2 Transformación de los datos ----------------------------------------

message("\n[2/2] Transformando los datos...")

balten_ds$todos <-
  balten_raw |>
  mutate(
    # --- Extraigo todas las fechas detectadas en el documento
    # En BALTEN: [[1]] es emisión, [[2]] anterior y [[3]] actual
    fechas_det = map(contenido, ~ str_extract_all(.x, patrones_balten$todas_fechas)[[1]]),

    # --- Metadatos de las Facturas
    factura_n = str_extract(contenido, patrones_balten$n_factura),
    fecha_fact = dmy(map_chr(fechas_det, ~ .x[1])), # PRIMERA fecha que aparece en el documento
    periodo = str_to_upper(str_extract(contenido, patrones_balten$periodo)),

    # --- Datos lectura y Consumo
    lectura_ant = str_extract(contenido, patrones_balten$l_ant),
    fecha_ant = dmy(map_chr(fechas_det, ~ .x[2])), # SEGUNDA fecha detectada
    lectura_act = str_extract(contenido, patrones_balten$l_act),
    fecha_act = dmy(map_chr(fechas_det, ~ .x[3])), # TERCERA fecha detectada
    consumo_m3 = xtr_num(contenido, patrones_balten$consumo),

    # --- Importes
    precio = xtr_num(contenido, patrones_balten$precio),
    total_eur = xtr_num(contenido, patrones_balten$total)
  ) |>
  select(!c(archivo, contenido, fechas_det)) |>
  arrange(fecha_fact)

balten_ds$bimensuales <-
  balten_ds$todos |>
  select(fecha_fact, fecha_act, consumo_m3, precio, total_eur) |>
  mutate(aa_act = year(fecha_act), mm_act = month(fecha_act, label = TRUE), .after = 1)


message("\n ✓ Extracción completada: ", nrow(balten_raw), " observaciones.\n")

# 4. GUARDAR --------------------------------------------------------------

message("Guardando datos limpios...")

guardar_con_backup(balten_ds, "data/processed/balten_raw.rds")

message("✓ Script 02_ingest_balten.R finalizado correctamente.\n")
