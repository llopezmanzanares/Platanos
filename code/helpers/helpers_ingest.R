# code/helpers/helpers_ingest

# Funciones de ayuda para la lectura de los pdf

# 1. --- FUNCIONES GRALES -------------------------------------------------

#' Convierte cadena con formato numérico español (1.234,56) a numeric
#' @param  txt  Vector de caracteres con el texto a procesar
#' @param  patron Expresión regular para extraer el número
#' @return Vector numeric
xtr_num <- function(txt, patron) {
  str_extract(txt, patron) |>
    str_remove("\\.") |> # elimina separador de miles
    str_replace(",", ".") |> # normaliza separador decimal
    as.numeric()
}


# 2. --- COOPERATIVA ------------------------------------------------------

# 3. --- BALTEN -----------------------------------------------------------
