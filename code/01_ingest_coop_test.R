# ******************************************************************************
# SCRIPT: 01_ingest_coop.R
# PROYECTO: Control y análisis financiero de la finca de plátanos
# FECHA DE CREACIÓN:   25/03/2026
# ÚLTIMA MODIFICACIÓN: 13/05/2026
# DESCRIPCIÓN: Importación y limpieza de liquidaciones en PDF (COPLACSIL)
# ******************************************************************************

# 0. LIBRERIAS ------------------------------------------------------------

suppressPackageStartupMessages({
  library(pdftools)
})

# 1. FUNCIONES DE PROCESAMIENTO -------------------------------------------
#' Función interna para limpiar formatos numéricos españoles (1.313,89 € -> 1313.89)
limpiar_num_es <- function(x) {
  if (is.na(x) || length(x) == 0) {
    return(NA_real_)
  }
  x |>
    str_remove_all("[\\s€]") |> # Elimina espacios y símbolo de Euro
    str_replace_all("\\.", "") |> # Elimina puntos de millar
    str_replace(",", ".") |> # Cambia coma decimal por punto
    as.numeric()
}

#' Procesa de manera íntegra y segura una única liquidación en PDF
procesar_liquidacion_pdf <- function(ruta_completa) {
  # Intenta leer el PDF; si falla, lanza un warning y continúa con el siguiente
  texto_raw <- tryCatch(
    {
      pdf_text(ruta_completa) |> paste(collapse = "\n")
    },
    error = function(e) {
      warning("⚠️ No se pudo leer el archivo: ", basename(ruta_completa))
      return(NULL)
    }
  )

  if (is.null(texto_raw) || texto_raw == "") {
    return(NULL)
  }
  lineas <- read_lines(texto_raw)

  # 1. Extracción de la Fecha
  fecha_str <- str_extract(texto_raw, "\\d{2}\\.\\d{2}\\.\\d{2,4}")

  if (is.na(fecha_str)) {
    warning("⚠️ No se encontró fecha válida en: ", basename(ruta_completa))
    return(NULL)
  }

  # 2. Helper de captura posicional por categorías (Kilos, %, Precio, Importe)
  extraer_categoria <- function(patron, lineas_texto) {
    linea <- str_subset(lineas_texto, patron)

    if (length(linea) == 0) {
      return(c(NA_real_, NA_real_, NA_real_))
    }

    # Captura estructurada de las columnas de la tabla
    matches <- str_match(linea, "([0-9.,]+)\\s+([0-9.,]+)\\s+([0-9.,]+)\\s+([0-9.,]+)")

    if (is.na(matches[1])) {
      return(c(NA_real_, NA_real_, NA_real_))
    }

    # Devolvemos Kilos (Grupo 2), Precio (Grupo 4), Importe (Grupo 5)
    c(limpiar_num_es(matches[2]), limpiar_num_es(matches[4]), limpiar_num_es(matches[5]))
  }

  # Extracciones matriciales seguras
  prem <- extraer_categoria("PREMIUM", lineas)
  psup <- extraer_categoria("P\\. SUPER", lineas)
  segu <- extraer_categoria("SEGUNDA", lineas)

  # 3. Línea de Totales de la tabla (Kilos e Importe Bruto)
  linea_total <- str_subset(lineas, "Total \\.\\.\\.")
  total_kg_val <- NA_real_
  eur_brutos_val <- NA_real_

  if (length(linea_total) > 0) {
    matches_total <- str_match(linea_total, "Total\\s*\\.+\\s*([0-9.,]+)\\s+([0-9.,]+)\\s+([0-9.,]+)")
    if (!is.na(matches_total[1])) {
      total_kg_val   <- limpiar_num_es(matches_total[2])
      eur_brutos_val <- limpiar_num_es(matches_total[4])
    }
  }

  # 4. Total Racimos
  linea_racimos <- str_subset(lineas, "racimos")
  racimos_val   <- NA_real_
  if (length(linea_racimos) > 0) {
    racimos_val <- str_extract(linea_racimos, "\\d+") |> as.numeric()
  }

  # 5. Total Euros Neto (Liquidación final percibida en el banco)
  linea_neto    <- str_subset(lineas, "Total Euros")
  eur_netos_val <- NA_real_
  if (length(linea_neto) > 0) {
    eur_netos_val <- str_extract(linea_neto, "[0-9.,]+") |> limpiar_num_es()
  }

  # Construcción del dataframe ancho por archivo
  tibble(
    fecha           = dmy(fecha_str),
    premium_kg      = prem[1],
    premium_eurkg   = prem[2],
    premium_eur     = prem[3],
    psup_kg         = psup[1],
    psup_eurkg      = psup[2],
    psup_eur        = psup[3],
    segunda_kg      = segu[1],
    segunda_eurkg   = segu[2],
    segunda_eur     = segu[3],
    total_kg        = total_kg_val,
    total_eur       = eur_brutos_val, # Mantenido por compatibilidad
    eur_brutos      = eur_brutos_val, # Solución TODO: bruto
    eur_netos       = eur_netos_val,  # Solución TODO: neto
    racimos         = racimos_val
  )
}

# 2. EJECUCION ------------------------------------------------------------

data_files <- list.files(path = here(dirs$cop), pattern = "^L.*\\.pdf$")

if (length(data_files) == 0) {
  stop("✗ ERROR: No se encontraron los archivos PDF en: ", here(dirs$cop))
}

message("Cargando y procesando ", length(data_files), " liquidaciones de la Cooperativa...")

# Iteración robusta sobre los 112 archivos mapeando la función extractora
coop_raw <- data_files[1:65] |>

  map_df(~ procesar_liquidacion_pdf(here(dirs$cop, .x))) |>

  # Transformación al formato estructurado largo (Tidy Data) original
  pivot_longer(
    cols           = !fecha,
    names_to       = "tipo",
    values_to      = "valor",
    values_drop_na = TRUE
  ) |>
  # Agregamos agrupaciones temporales estándar para el informe Quarto
  mutate(
    fecha_aa  = as_factor(year(fecha)),
    fecha_mm  = month(fecha, label = TRUE),
    fecha_sem = week(fecha),
    .after    = fecha
  )

message("\n ✓ Extracción completada de forma segura: ", nrow(coop_raw), " métricas consolidadas.\n")

# 3. GUARDAR --------------------------------------------------------------

guardar_con_backup(coop_raw, here(dirs$pro, "coop_raw.rds"))

message("✓ Script 01_ingest_coop.R finalizado correctamente.\n")
