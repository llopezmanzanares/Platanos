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
    fecha = str_c(str_extract(fecha, "\\d+"),  # saco el año
                  str_extract(fecha, "[A-Z][a-z]+"), #saco el mes, sin (p)
                  sep = " "),
    .before = 1
  ) %>% 
  write_csv("data/processed/imp_tfe.csv")

