# ******************************************************************************
# SCRIPT: 01_ingest_coop.R
# PROYECTO: Control y análisis financiero de la finca de plátanos
# FECHA DE CREACIÓN:   25/03/2026
# ÚLTIMA MODIFICACIÓN: 04/04/2026
# DESCRIPCIÓN:
#   Importación de datos las liquidaciones de la Cooperativa, archivos PDFs
#
#   El script se ejecuta desde 00_run_all.R
#
# ENTRADAS:
#   - data/raw/coop/
#     * Archivos PDF → "L*.pdf"

# SALIDAS:
#   - Objetos: coop_raw
#   - Archivo RDS con los datos data/processed/coop_raw.rds
#   - Mensajes de progreso y validación en consola

# DEPENDENCIAS:
#   - tidyverse, here, glue, pdftools

# ******************************************************************************

# 0. LIBRERIAS ------------------------------------------------------------

suppressPackageStartupMessages(
  library(pdftools)
)


# 1. CONSTANTES -----------------------------------------------------------

# Patrones regex para extraer valores numéricos del texto de los PDFs
patrones_coop <- list(
  fecha = "(\\d{2,4}\\.?){3}", # dd.mm.aa ó dd.mm.aaaa
  kg    = "(\\d\\.)?\\d+", # kg, miles con punto separador
  prc   = "\\d{1,2},\\d{1,2}", # porcentaje con 2 decimales
  eurkg = "\\d,\\d{4}", # precio €/kg con 4 decimales
  eur   = "(\\d\\.)?\\d{1,3},\\d{2}$", # importe final (2 decimales)
  racms = "\\d{1,2}" # número de racimos
)

# 2 FUNCIONES -------------------------------------------------------------

#' Lee los PDFs de liquidaciones y filtra las líneas con datos relevantes
#' @param  datafiles Vector de nombres de archivo (sin ruta)
#' @return tibble con columnas: archivo, value
read_pdfs <- function(datafiles) {
  map(here(dirs$cop, datafiles), pdf_text) |>
    set_names(datafiles) |>
    map(\(x) unlist(str_split(x, "\\n"))) |>
    enframe(name = "archivo", value = "lineas") |>
    unnest(lineas) |>
    rename(value = lineas) |>
    filter(str_detect(
      value,
      "Fecha|PREMIUM|P\\. SUPER|SEGUNDA|Total|racimos"
    ))
}

#' Extrae y estructura los datos de las liquidaciones semanales
#' @param liquidaciones tibble con columnas: archivo, value
#' @return tibble tidy con columnas: fecha, tipo, valor
xtr_datos_liquidaciones <- function(liquidaciones) {
  # Helper: extrae número solo si la línea contiene el patrón de detección
  xtr_si <- function(txt, patron_det, patron_xtr) {
    if_else(str_detect(txt, patron_det), xtr_num(txt, patron_xtr), NA_real_)
  }

  liquidaciones |>
    mutate(
      # --- Valores únicos por línea ---
      fecha = if_else(
        str_detect(value, "Fecha"),
        str_extract(value, patrones_coop$fecha),
        NA_character_
      ),
      racimos = xtr_si(value, "racimos", patrones_coop$racms)
    ) |>
    fill(fecha) |>
    mutate(
      # --- Múltiples valores por línea ---
      fecha         = dmy(fecha),
      total_kg      = xtr_si(value, "Total \\.", patrones_coop$kg),
      total_eur     = xtr_si(value, "Total \\.", patrones_coop$eur),
      premium_kg    = xtr_si(value, "PREMIUM", patrones_coop$kg),
      premium_eurkg = xtr_si(value, "PREMIUM", patrones_coop$eurkg),
      premium_eur   = xtr_si(value, "PREMIUM", patrones_coop$eur),
      psup_kg       = xtr_si(value, "P\\. SUPER", patrones_coop$kg),
      psup_eurkg    = xtr_si(value, "P\\. SUPER", patrones_coop$eurkg),
      psup_eur      = xtr_si(value, "P\\. SUPER", patrones_coop$eur),
      segunda_kg    = xtr_si(value, "SEGUNDA", patrones_coop$kg),
      segunda_eurkg = xtr_si(value, "SEGUNDA", patrones_coop$eurkg),
      segunda_eur   = xtr_si(value, "SEGUNDA", patrones_coop$eur)
    ) |>
    select(!c(value, archivo)) |>
    pivot_longer(
      cols           = !fecha,
      names_to       = "tipo",
      values_to      = "valor",
      values_drop_na = TRUE
    ) |>
    distinct() |>
    summarise(.by = c(fecha, tipo), valor = sum(valor))
}

# 3. EJECUCION ------------------------------------------------------------

data_files <- list.files(path = here(dirs$cop), pattern = "^L")

if (length(data_files) == 0) {
  stop("✗ ERROR: No se encontraron los archivos PDF en: ", here(dirs$cop))
}

message("Cargando liquidaciones de la Cooperativa, ", length(data_files), " archivos PDF...")

coop_raw <-
  read_pdfs(data_files) |>
  xtr_datos_liquidaciones() |>
  mutate(
    across(fecha, list(
      aa  = \(x) as_factor(year(x)),
      mm  = \(x) month(x, label = TRUE),
      sem = \(x) week(x)
    )),
    .after = 1
  )

message("\n ✓ Extracción completada: ", nrow(coop_raw), " observaciones.\n")

# 4. GUARDAR --------------------------------------------------------------

guardar_con_backup(coop_raw, "data/processed/coop_raw.rds")

message("✓ Script 01_ingest_coop.R finalizado correctamente.\n")
