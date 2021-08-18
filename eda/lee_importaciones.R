library(tidyverse)

# Los datos del último año son provisionales y vienen marcadas las fechas
# con un (p) que es necesario eliminar (líneas 16 y 17)

ds <- read_delim("data/raw/C00012A_0001.txt",
                 skip = 7,
                 col_names = c("fecha", "pen", "extr"),
                 show_col_types = F,
                 trim_ws = T) %>% 
  filter(
    fecha != "Tenerife",
    pen > 0
  ) %>% 
  mutate(
    año = str_extract(fecha, "\\d+"),
    mes = str_extract(fecha, "[A-Z][a-z]+"),
    mes = case_when(
      mes == "Enero" ~ "01", mes == "Febrero" ~ "02",
      mes == "Marzo" ~ "03", mes == "Abril" ~ "04",
      mes == "Mayo" ~ "05", mes == "Junio" ~ "06",
      mes == "Julio" ~ "07", mes == "Agosto" ~ "08",
      mes == "Septiembre" ~ "09", mes == "Octubre" ~ "10",
      mes == "Noviembre" ~ "11", mes == "Diciembre" ~ "12"
    ),
    fecha = str_c("01", mes, año, sep = "-"),
    .before = 1
  ) %>%
  select(-1,-2) %>% 
  mutate(
    extr = as.numeric(str_replace(extr, ",", "."))
  ) %>% 
  write_csv("data/processed/imp_tfe.csv")

