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

# Categorías de producto y patrones asociados
categorias <- list(
  total   = list(patron = "Total \\.", campos = c("kg", "eur")),
  premium = list(patron = "PREMIUM", campos = c("kg", "eurkg", "eur")),
  psup    = list(patron = "P\\. SUPER", campos = c("kg", "eurkg", "eur")),
  segunda = list(patron = "SEGUNDA", campos = c("kg", "eurkg", "eur"))
)

# 2 FUNCIONES -------------------------------------------------------------

#' Lee los PDFs de liquidaciones y filtra las líneas con datos relevantes
#' @param  datafiles Vector de nombres de archivo (sin ruta)
#' @return tibble con columnas: archivo, value
read_pdfs <- function(datafiles) {
  map_df(
    here(dirs$cop, datafiles),
    \(f) {
      data.frame(
        archivo = basename(f),
        value = unlist(str_split(pdf_text(f), "\\n"))
      )
    }
  ) |>
    filter(str_detect(value, "Fecha|PREMIUM|P\\. SUPER|SEGUNDA|Total|racimos"))
}


#' Extrae y estructura los datos de las liquidaciones semanales
#' @param liquidaciones tibble con columnas: archivo, value
#' @return tibble tidy con columnas: fecha, tipo, valor
xtr_datos_liquidaciones <- function(liquidaciones) {
  # Extraer valores para cada categoría y campo
  valores_categorias <-
    imap(categorias, \(cat, nom_cat){
      map_dfc(cat$campos, \(campo){
        tibble(
          !!paste0(nom_cat, "_", campo) := if_else(
            str_detect(liquidaciones$value, cat$patron),
            xtr_num(liquidaciones$value, patrones_coop[[campo]]),
            NA_real_
          )
        )
      })
    }) |>
    list_cbind()

  liquidaciones |>
    mutate(
      fecha = if_else(
        str_detect(value, "Fecha"),
        xtr_num(value, patrones_coop$fecha),
        NA_character_
      ),
      racimos = if_else(
        str_detect(value, "racimos"),
        xtr_num(value, patrones_coop$racms),
        NA_real_
      )
    ) |>
    fill(fecha) |>
    mutate(fecha = dmy(fecha)) |>
    bind_cols(valores_categorias) |>
    select(!c(value, archivo)) |>
    pivot_longer(
      cols = !fecha,
      names_to = "tipo",
      values_to = "valor",
      values_drop_na = TRUE
    ) |>
    summarise(.by = c(fecha, tipo), valor = sum(valor, na.rm = TRUE))
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
    fecha_aa = as_factor(year(fecha)),
    fecha_mm = month(fecha, label = TRUE),
    fecha_sem = week(fecha),
    .after = fecha
  )

message("\n ✓ Extracción completada: ", nrow(coop_raw), " observaciones.\n")

# 4. GUARDAR --------------------------------------------------------------

guardar_con_backup(coop_raw, "data/processed/coop_raw.rds")

message("✓ Script 01_ingest_coop.R finalizado correctamente.\n")
