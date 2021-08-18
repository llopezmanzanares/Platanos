library(tidyverse)
library(lubridate)

imp <- read_csv("data/processed/imp_tfe.csv",
                show_col_types = F) %>% 
  mutate(
    fecha = as.Date(fecha, tryformats = "%d-%m-%Y")
  )
pr_m <- read_csv("data/processed/pr_tfe_mes.csv",
                 show_col_types = F)
