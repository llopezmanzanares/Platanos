# creación de las gráficas

library(tidyverse)
library(here)

if (file.exists(here("data/processed", "platanos.RData")))
  load(here("data/processed", "platanos.RData"))

# Gráficas de precios -----------------------------------------------------


source(here("eda", "precios_graficas.R"))