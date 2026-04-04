# code/helpers/00_utils_load.R

suppressPackageStartupMessages({
  library(here)
  library(glue)
  library(readr)
})


# ── Cargar RDS si no existe en el entorno ──────────────────────────────────────
cargar_rds_si_no_existe <- function(obj_name, ruta_relativa,
                                    solucion = NULL) {
  if (exists(obj_name, envir = .GlobalEnv)) {
    message(glue("✓ {obj_name} ya existe en el entorno"))
    return(invisible(get(obj_name, envir = .GlobalEnv)))
  }
  archivo <- here(ruta_relativa)
  if (!file.exists(archivo)) {
    stop(glue("✗ No se encuentra: {archivo}\n{solucion %||% ''}"))
  }

  obj <- tryCatch(
    read_rds(archivo),
    error = function(e) stop(glue("✗ Error al cargar {basename(archivo)}: {e$message}"))
  )
  assign(obj_name, obj, envir = .GlobalEnv)
  message(glue(" ✓ {obj_name} cargado ({length(obj)} componentes)"))
  invisible(obj)
}

# ── Guardar RDS con backup automático ─────────────────────────────────────────
guardar_con_backup <- function(objeto, ruta_relativa, compress = "gz") {
  ruta <- here(ruta_relativa)
  dir.create(dirname(ruta), recursive = TRUE, showWarnings = FALSE) # ← ver punto 3

  if (file.exists(ruta)) {
    ruta_bkp <- sub("\\.rds$", "_backup.rds", ruta)
    file.copy(ruta, ruta_bkp, overwrite = TRUE)
    message(glue(" → Backup: {basename(ruta_bkp)}"))
  }
  write_rds(objeto, ruta, compress = compress)
  message(glue(" ✓ Guardado: {basename(ruta)} ({round(file.size(ruta)/1024,1)} KB)"))
  invisible(ruta)
}

# Operador null-coalesce para mensajes opcionales
`%||%` <- function(a, b) if (!is.null(a)) a else b
