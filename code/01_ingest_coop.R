# ******************************************************************************
# SCRIPT: 01_ingest_coop.R
# PROYECTO: Control y análisis financiero de la finca de plátanos
# FECHA DE CREACIÓN:   25/03/2026
# ÚLTIMA MODIFICACIÓN: 15/05/2026
# DESCRIPCIÓN: Importación y limpieza de liquidaciones en PDF (COPLACSIL)
# ******************************************************************************

# 0. LIBRERIAS ------------------------------------------------------------

suppressPackageStartupMessages({
  library(pdftools)
})

# 1. FUNCIONES DE PROCESAMIENTO -------------------------------------------
#' Función interna para limpiar formatos numéricos españoles (1.313,89 € -> 1313.89)
#' Limpieza vectorizada: Maneja correctamente cuando x tiene más de un elemento
#' por ejemplo 2 filas por categoría, como ocurre en alguna factura
limpiar_num_es <- function(x) {
  if (length(x) == 0) return(NA_real_)
  
  map_dbl(x, \(val){
    if(is.na(val) || val == "" || val == ".") return(NA_real_)
    num <- val |> 
      str_remove_all("[\\s€\\+]") |> # Quita espacios, € y +
      str_remove("-$") |>            # Quita el guion final si existe
      str_replace_all("\\.", "") |>  # Quita puntos de millar
      str_replace(",", ".") |>       # Cambia coma por punto decimal
      as.numeric()
  })
}
  

#' Procesa de manera íntegra y segura por líneas
procesar_liquidacion <- function(ruta_pdf) {
  # Intenta leer el PDF; si falla, lanza un warning y continúa con el siguiente
  texto_raw <- tryCatch(
    {
      pdf_text(ruta_pdf) |> paste(collapse = "\n")
    },
    error = function(e) {
      warning("⚠️ No se pudo leer el archivo: ", basename(ruta_pdf))
      return(NULL)
    }
  )

  if (is.null(texto_raw) || texto_raw == "") {return(NULL)}
  lineas <- read_lines(texto_raw)

  # 1. Extracción de la Fecha y Racimos
  fecha_val   <- dmy(str_extract(texto_raw, "\\d{2}\\.\\d{2}\\.\\d{2,4}"))
  racimos_val <- str_subset(lineas, "Total racimos:") |> 
    str_extract("\\d+") |> as.numeric()

  # 2. Extraer filas de Calidades, aunque sean varias filas por "Calidad"
  # Regex: busca el primer dígito y captura 4 bloques numéricos (Kilos, %, Precio, Importe)
  regex_calidad <- "(\\d[0-9.,]*)\\s+([0-9.,]+)\\s+([0-9.,]+)\\s+([0-9.,]+)"
  
  extraer_filas <- function(patron, nombre_tipo) {
    lineas_encontradas <- str_subset(lineas, patron)
    if (length(lineas_encontradas) == 0) return(NULL)
    
    matches <- str_match(lineas_encontradas, regex_calidad)
    
    tibble(
      fecha = fecha_val,
      tipo  = nombre_tipo,
      kg    = limpiar_num_es(matches[,2]),
      eur   = limpiar_num_es(matches[,5])
    )
  }
  
  # Proceso y consolidado de las filas de cada calidad
  datos_categorias <- bind_rows(
    extraer_filas("PREMIUM", "premium"),
    extraer_filas("P\\. SUPER", "p_super"),
    extraer_filas("SEGUNDA", "segunda")
  ) |> 
    summarise(
      .by   = c(fecha, tipo),
      kg    = sum(kg, na.rm = T),
      eur   = sum(eur, na.rm = T),
      eurkg = eur / kg 
      )

  # 3. Extraer Totales, bruto y neto
  # Bruto, de la tabla de Calidades
  linea_total <- str_subset(lineas, "Total \\. \\. \\.")
  match_tab <- str_match(linea_total, "(\\d[0-9.,]*)\\s+([0-9.,]+)\\s+([0-9.,]+)")
  
  bruto_df <- tibble(
    fecha = fecha_val,
    tipo  = "total_bruto",
    kg    = limpiar_num_es(match_tab[1, 2]),
    eur   = limpiar_num_es(match_tab[1, 4])
  )
  # Neto, final de la liquidación
  linea_neto <- str_subset(lineas, "Total Euros")
  neto_eur <- limpiar_num_es(str_extract(linea_neto, "[0-9.,]+(?=€|$)"))
  
  neto_df <- tibble(fecha = fecha_val, tipo = "total_neto", kg = NA_real_, eur = neto_eur)

  # 4. Unificar todo, formato largo
  df_metricas <- bind_rows(datos_categorias, bruto_df, neto_df) |> 
    pivot_longer(cols = c(kg, eur, eurkg), names_to = "metrica", values_to = "valor") |> 
    mutate(tipo = paste0(tipo, "_", metrica)) |> 
    select(fecha, tipo, valor)
  
  df_racimos <- tibble(fecha = fecha_val, tipo = "total_racimos", valor = racimos_val)
  
  bind_rows(df_metricas, df_racimos) |> filter(!is.na(valor))
}

# 2. EJECUCION ------------------------------------------------------------

data_files <- list.files(path = here(dirs$cop), pattern = "^L.*\\.pdf$", full.names = TRUE)

if (length(data_files) == 0) {
  stop("✗ ERROR: No se encontraron los archivos PDF en: ", here(dirs$cop))
}

message("Cargando y procesando ", length(data_files), " liquidaciones de la Cooperativa...")

# Iteración robusta sobre los 112 archivos mapeando la función extractora
coop_raw <- data_files |>
  map_df(~ procesar_liquidacion(.x)) |>

  # Enriquecer con dimensiones temporales para informes
  mutate(
    fecha_aa  = as_factor(year(fecha)),
    fecha_mm  = month(fecha, label = TRUE),
    fecha_sem = week(fecha),
    .after = fecha
  )

message("\n ✓ Extracción completada de forma segura: ", nrow(coop_raw), " métricas consolidadas.\n")

# 3. GUARDAR --------------------------------------------------------------

guardar_con_backup(coop_raw, here(dirs$pro, "coop_raw.rds"))

message("✓ Script 01_ingest_coop.R finalizado correctamente.\n")
