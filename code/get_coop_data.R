# Datos de producción proporcionados por la cooperativa
# Extraigo la información de todos los pdf cada vez

# Versión: 2024-03-19

# Packages ------

# library(tidyverse)
library(pdftools)
# library(here)

# Constantes --------------------------------------------------------------

# coop_ds <- list()
# 
# dirs <- list(
#   raw = "data/raw",
#   cop = "data/raw/coop",
#   pro = "data/processed"
# )

patrones <- list(
  fecha = "(\\d{2,4}\\.?){3}",        # dd.mm.aa ó dd.mm.aaaa
  semns = "\\d{1,2}",                 # número de 1 ó 2 dígitos
  kg    = "(\\d\\.)?\\d+",            # los miles con un punto
  prc   = "\\d{1,2},\\d{1,2}",        # número de 2 decimales
  eurkg = "\\d,\\d{4}",               # número con 4 decimales
  eur   = "(\\d\\.)?\\d{1,3},\\d{2}$", # último número, tiene 2 decimales
  racms = "\\d{1,2}"
)

# Funciones ---------------------------------------------------------------
xtr_num <- function(txt, patron){
  numero <- str_extract(txt, patron) %>% 
    str_replace("\\.", "") %>%   # en el caso de que sea > 1000 tiene un punto
    str_replace(",", ".") %>% 
    as.numeric()
  
  return(numero)
}

read_pdfs <- function(datafiles) {
  
  ds_coop <- 
    map(here(dirs$cop, datafiles), pdf_text)|> 
    unlist() |> 
    str_split(pattern = "\\n")|> 
    unlist() |> 
    as_tibble_col() |> 
    # me quedo con las filas con información relevante
    filter(str_detect(value, pattern = "Fecha|Semana|PREMIUM|P\\. SUPER|SEGUNDA|Total|racimos|medio"))
  
  return(ds_coop)
}

# xtr_sem_data <- function(datos_semanales){
#   # voy extrayendo los subconjuntos de datos
#   fechas <- filter(datos_semanales, medida == "fecha") %>% 
#     mutate(
#       fecha = str_extract(value, pattern = "(\\d{2,4}\\.?){3}") |> 
#         dmy(quiet = TRUE),
#       .keep = "none"
#     ) |> 
#     filter(!is.na(fecha))
#   semanas <- filter(datos_semanales, medida == "semana") %>% 
#     mutate(
#       semana = xtr_num(value, "\\d{1,2}"),
#       .keep = "none"
#     )
#   totales <- filter(datos_semanales, medida == "totales") %>% 
#     mutate(
#       total_kg = xtr_num(value, "(\\d\\.)?\\d+"),
#       total_eur = xtr_num(value, "(\\d\\.)?\\d{1,3},\\d{2}$"),
#       .keep = "none"
#     )
#   total_factura <- filter(datos_semanales, medida == "total_euros") %>% 
#     mutate(
#       total_fac = xtr_num(value, "(\\d\\.)?\\d{1,3},\\d{2}"),
#       .keep = "none"
#     )
#   racimos <- filter(datos_semanales, medida == "racimos") %>% 
#     mutate(
#       racimos = xtr_num(value, "\\d{1,2}"),
#       .keep = "none"
#     )
#   peso_med <- filter(datos_semanales, medida == "peso") %>% 
#     mutate(
#       peso_med = xtr_num(value, "\\d+,\\d{2}"),
#       .keep = "none"
#     )
#   prec_med <- filter(datos_semanales, medida == "precio") %>% 
#     mutate(
#       prec_med = xtr_num(value, "\\d,\\d{1,4}"),
#       .keep = "none"
#     )
#   premium <- filter(datos_semanales, medida == "premium") %>% 
#     mutate(
#       premium_kg =    xtr_num(value, patrones$kg),
#       premium_pc =    xtr_num(value, patrones$prc),
#       premium_eurkg = xtr_num(value, patrones$eurkg),
#       premium_eur =   xtr_num(value, patrones$eur),
#       .keep = "none"
#     )
#   psup <- filter(datos_semanales, medida == "psup") %>% 
#     mutate(
#       psup_kg =    xtr_num(value, patrones$kg),
#       psup_pc =    xtr_num(value, patrones$prc),
#       psup_eurkg = xtr_num(value, patrones$eurkg),
#       psup_eur =   xtr_num(value, patrones$eur),
#       .keep = "none"
#     )
#   segunda <- filter(datos_semanales, medida == "segunda") %>% 
#     mutate(
#       segunda_kg =    xtr_num(value, patrones$kg),
#       segunda_pc =    xtr_num(value, patrones$prc),
#       segunda_eurkg = xtr_num(value, patrones$eurkg),
#       segunda_eur =   xtr_num(value, patrones$eur),
#       .keep = "none"
#     )
#   
#   semanales <- 
#     bind_cols(fechas, semanas, racimos, totales, total_factura, peso_med, prec_med, premium, psup, segunda)
#   
# #  rm(ds, fechas, semanas, racimos, totales, total_factura, peso_med, prec_med, premium, psup, segunda)
#   
#   return(semanales)
# }

### Hay un problema con la liquidación del 2024/02/03
# varias filas por cada calidad del corte, por lo que la función anterior genera un error
# Nueva función
#TODO ver las diferencias con este dataset
xtr_datos_liquidaciones <- function(liquidaciones){
  ds <- liquidaciones |>
    # me quedo con la información relevante
    # las que tienen un solo valor por fila
    mutate(
      fecha   = ifelse(str_detect(value, "Fecha"),   str_extract(value, patrones$fecha),NA),
      racimos = ifelse(str_detect(value, "racimos"), xtr_num(value, patrones$racms),    NA)
    ) |> 
    fill(fecha) |> 

    # varios valores por fila
    mutate(
      fecha = dmy(fecha),
      
      total_kg      = ifelse(str_detect(value, "Total \\."), xtr_num(value, patrones$kg),   NA),
      total_eur     = ifelse(str_detect(value, "Total \\."), xtr_num(value, patrones$eur),  NA),
      premium_kg    = ifelse(str_detect(value, "PREMIUM"),   xtr_num(value, patrones$kg),   NA),
      premium_eurkg = ifelse(str_detect(value, "PREMIUM"),   xtr_num(value, patrones$eurkg),NA),
      premium_eur   = ifelse(str_detect(value, "PREMIUM"),   xtr_num(value, patrones$eur),  NA),
      psup_kg       = ifelse(str_detect(value, "P\\. SUPER"),xtr_num(value, patrones$kg),   NA),
      psup_eurkg    = ifelse(str_detect(value, "P\\. SUPER"),xtr_num(value, patrones$eurkg),NA),
      psup_eur      = ifelse(str_detect(value, "P\\. SUPER"),xtr_num(value, patrones$eur),  NA),
      segunda_kg    = ifelse(str_detect(value, "SEGUNDA"),   xtr_num(value, patrones$kg),   NA),
      segunda_eurkg = ifelse(str_detect(value, "SEGUNDA"),   xtr_num(value, patrones$eurkg),NA),
      segunda_eur   = ifelse(str_detect(value, "SEGUNDA"),   xtr_num(value, patrones$eur),  NA)
    ) |> 
    
    # reestructuro el dataset
    select(!value) |> 
    pivot_longer(cols = !fecha, names_to = "tipo", values_to = "valor", values_drop_na = TRUE) |> 
    distinct() |> 
    summarise(.by = c(fecha, tipo), valor = sum(valor))
  
  return(ds)
}

# Leo y extraigo ----------------------------------------------------------

data_files <- list.files(
  path = here(dirs$cop),
  pattern = "^L"
  )

# metodo anterior
# coop_ds$sem <- read_pdfs(data_files) %>% xtr_sem_data()

coop_ds$semanas <- 
  read_pdfs(data_files) |> 
  xtr_datos_liquidaciones() |> 
  mutate(
    across(fecha, list(
      aa = \(x) as_factor(year(x)),
      mm = \(x) month(x, label = TRUE),
      sem= \(x) week(x)
    )),
    .after = 1
  )

# Agregados mensuales -----------------------------------------------------

# Es más útil trabajar con los datos mensuales
# solo tengo un dato de 2020, así que lo elimino
coop_ds$mes <- 
  filter(coop_ds$semanas, fecha_aa != 2020) |>
  select(!fecha_sem) |> 
  pivot_wider(names_from = "tipo", values_from = "valor") |> 
  mutate(
    fecha = rollforward(fecha)
  ) |> 
  summarise(
    .by = starts_with("fecha"),
    across(!ends_with("eurkg"), sum),
    across(ends_with("eurkg"), mean),
    kg_rac = total_kg / racimos
  )


# Agregados semanales -----------------------------------------------------

coop_ds$semanas <- 
  filter(coop_ds$full, year(fecha) > 2020) |> 
  pivot_wider(names_from = "tipo", values_from = "valor") |> 
  

# Kg por meses ------------------------------------------------------------

# coop_ds$mes_kg <- coop_ds$mes %>% 
#   mutate(
#     aa = year(fecha),
#     mm = month(fecha, label = TRUE),
#     .after = 1
#   ) %>% 
#   select(fecha:mm, ends_with("kg")) %>% 
#   group_by(aa) %>% 
#   mutate(
#     across(ends_with("_kg"), cumsum, .names = "{.col}_acum")
#   ) %>% 
#   ungroup()

# Guardo el conjunto de datos ---------------------------------------------

save(coop_ds, file = here(dirs$pro, "datos_finca.RData"))

# write_csv2(coop_ds$sem, file = here(dirs$pro, "datos_finca.csv"))


# Limpio la casa ----------------------------------------------------------

rm(xtr_num, read_pdfs, patrones, data_files)
