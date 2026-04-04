# Lectura de los datos de las facturas del consumo de agua

# Versión: 2025-04-03


# Set-up ------------------------------------------------------------------

library(pdftools)

balten_ds <- list()

# Funciones ---------------------------------------------------------------

# Extraigo un número no entero y lo transformo de cadena de texto a número
xtr_cifra <- function(txt, patron) {
  numero <- str_extract(txt, patron) |>
    str_remove("\\.") |> # en el caso de que sea > 1000 tiene un punto
    str_replace(",", ".") |> # normaliza decimal
    as.numeric()

  return(numero)
}

balten_files <- list.files(path = here(dirs$bal), full.names = TRUE)

# Forma previa
# balten <-
#   map(balten_files, pdf_text) |>
#   unlist() |>
#   str_split(pattern = "\\n") |>
#   unlist() |>
#   as_tibble_col() |>
#   filter(str_detect(value, pattern = "Lectura Ant|Agua Re"))

balten_raw <- map(balten_files, pdf_text) |>
  set_names(basename(balten_files)) |>
  enframe(name = "archivo", value = "contenido") |>
  mutate(lineas = map(contenido, ~ str_split(.x, "\\n")[[1]])) |>
  unnest(lineas) |>
  filter(str_detect(lineas, pattern = "Lectura Ant|Agua Re"))


balten_ds$todos <- balten_raw |>
  mutate(
    # lectura anterior del contador, y fecha
    l_ant = as.numeric(str_extract(lineas, pattern = "(?<=Lectura Anterior: )\\d+")),
    f_ant = dmy(str_extract(lineas, pattern = "(?<=Fecha: )(\\d{2}/){2}\\d{4}")),
    # lectura actual del contador, y fecha
    l_act = as.numeric(str_extract(lineas, pattern = "(?<=Lectura Actual: )\\d+")),
    f_act = dmy(str_extract(lineas, pattern = "(\\d{2}/){2}\\d{4}(?=\\s+Con)")),
    consumo_m3 = xtr_cifra(lineas, patron = "\\d+$"),
    precio = xtr_cifra(lineas, patron = "(?<=precio\\s{1,10})\\d,\\d+"),
    total = xtr_cifra(lineas, patron = "(?<=Total )(\\d\\.)?\\d{3},\\d{2}")
  ) |>
  fill(c(precio, total), .direction = "up") |>
  drop_na() |>
  select(!value) |>
  arrange(f_act)

balten_ds$bimensuales <- balten_ds$todos |>
  select(fecha = f_act, consumo_m3, precio, total_eur = total)

save(balten_ds, file = here(dirs$pro, "datos_balten.RData"))

# Limpio ------------------------------------------------------------------

rm(xtr_cifra, balten_files, balten_raw)
