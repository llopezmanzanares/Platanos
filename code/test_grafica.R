# vamos a intentar hacer una gráfica compleja
# evolución semanal de los valores
# probar gghoriplot o ggridges
# No todos los datos tienen el mismo "sentido" ni dan la misma info

library(tidyverse)
library(here)

# set up ------------------------------------------------------------------


eur <- scales::label_currency(suffix = "€", prefix = NULL,
                              big.mark = ".", decimal.mark = ",")

# mi paleta de colores
col_pal <- c("blanco" = "white", "cream" = "#f4f7be", "mindaro" = "#e5f77d", "ecru" = "#deba6f",
             "wine"  = "#823038", "licoire" = "#1e000e")


load(here("data/processed/datos_finca.RData"))

coop_rng <- 
  range(coop_ds$sem$fecha)


# todas las semanas de todos los años (considero 53)
aa_sem <- 
  tibble(
    aa  = rep(year(coop_rng[1]+1):year(coop_rng[2]), each = 53),
    semana = rep(1:53, year(coop_rng[2])-year(coop_rng[1]))
  )

coop_semanales <- 
  coop_ds$sem |> 
  filter(year(fecha) > 2020) |> 
  mutate(aa = year(fecha), .before = 1, .keep = "unused") |> 
  select(!c(racimos, total_fac))


semana_ds <-
  left_join(aa_sem, coop_semanales, join_by(aa, semana)) |> 
  mutate(across(everything(), ~replace_na(.x, replace = 0))) |> 
  pivot_longer(cols = !aa:semana)


# kg totales --------------------------------------------------------------

kg_tot <- filter(semana_ds, name == "total_kg")

kg_tot |> 
  mutate(aa = factor(aa, levels = sort(unique(aa), decreasing = TRUE))) |> 
  ggplot(aes(x = semana, y = aa, fill = value)) +
  geom_tile()


test_ds <-
  filter(semana_ds, str_detect(name, "total|_med"))


test_ds |>
  mutate(aa = factor(aa, levels = sort(unique(aa), decreasing = T))) |> 
  ggplot(aes(x = semana, y = aa)) +
  geom_raster(aes(fill = value), show.legend = TRUE) +
  facet_wrap(~name, ncol = 1, shrink = F)
