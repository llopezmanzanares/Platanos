# vamos a intentar hacer una gráfica compleja
# evolución semanal de los valores

library(tidyverse)

eur <- scales::label_currency(suffix = "€", prefix = NULL,
                              big.mark = ".", decimal.mark = ",")

# mi paleta de colores
col_pal <- c("blanco" = "white", "cream" = "#f4f7be", "mindaro" = "#e5f77d", "ecru" = "#deba6f",
             "wine"  = "#823038", "licoire" = "#1e000e")


load(here("data/processed/datos_finca.RData"))

# todas las semanas de todos los años (considero 53)
aa_sem <- 
  tibble(
    aa  = rep(year(coop_rng[1]):year(coop_rng[2]), each = 53),
    semana = rep(1:53, year(coop_rng[2])-year(coop_rng[1]) + 1)
  )

coop_semanales <- 
  coop_ds$sem |> 
  filter(year(fecha) > 2020) |> 
  mutate(aa = year(fecha), .before = 1, .keep = "unused") |> 
  select(!c(racimos, total_fac))
