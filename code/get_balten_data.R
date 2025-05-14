# Lectura de los datos de las facturas del consumo de agua

# Versión: 2025-04-03


# Set-up ------------------------------------------------------------------

library(pdftools)

balten_ds  <- list()

# Funciones ---------------------------------------------------------------

# Extraigo un número no entero y lo transformo de cadena de texto a número
xtr_cifra <- function(txt, patron){
  numero <- str_extract(txt, patron) %>% 
    str_replace("\\.", "") |>    # en el caso de que sea > 1000 tiene un punto
    str_replace(",", ".") |>  
    as.numeric()
  
  return(numero)
}

balten_files <- list.files(path = here::here(dirs$bal))

balten <- 
  map(here::here(dirs$bal, balten_files), pdf_text) |> 
  unlist() |> 
  str_split(pattern = "\\n") |> 
  unlist() |> 
  as_tibble_col() |> 
  filter(str_detect(value, pattern = "Lectura Ant|Agua Re"))

balten_ds$todos <- 
  balten |> 
  mutate(
    # lectura anterior del contador, y fecha
    l_ant = str_extract(value, pattern = "(?<=Lectura Anterior: )\\d+") |> as.numeric(),
    f_ant = str_extract(value, pattern = "(?<=Fecha: )(\\d{2}/){2}\\d{4}") |> dmy(),
    # lectura actual del contador, y fecha
    l_act = str_extract(value, pattern = "(?<=Lectura Actual: )\\d+") |> as.numeric(),
    f_act = str_extract(value, pattern = "(\\d{2}/){2}\\d{4}(?=\\s+Con)") |> dmy(),
    
    consumo_m3 = xtr_cifra(value, patron = "\\d+$"),
    precio = xtr_cifra(value, patron = "(?<=precio\\s{1,10})\\d,\\d+"),
    total  = xtr_cifra(value, patron = "(?<=Total )(\\d\\.)?\\d{3},\\d{2}")
    ) |> 
  fill(c(precio, total), .direction = "up") |> 
  drop_na() |> 
  select(!value) |> 
  arrange(f_act)

balten_ds$bimensuales <-
  select(balten_ds$todos, f_act:total) |> 
  rename(fecha = f_act, total_eur = total)

save(balten_ds, file = here(dirs$pro, "datos_balten.RData"))

# Limpio ------------------------------------------------------------------

rm(xtr_cifra, balten_files, balten)
