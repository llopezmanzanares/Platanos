# Gráficas usadas en el informe Finca.Rmd


# Carga de datos ----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

theme_set(
  theme_minimal()
  # theme_light()
)

# cargo datos_mes y obtengo datos_mes_kg
source(file = here("eda/", "coop_graficas_datos.R"))

grafs <- list()

# Relación euros vs Kg ----------------------------------------------------

grafs$eur_kg <-
datos_mes %>% 
  select(fecha, starts_with("total")) %>% 
  mutate(
    eur_kg = total_eur / total_kg,
  ) %>% 
  ggplot(aes(x = fecha, y = eur_kg)) +
  geom_point(alpha = .8) +
  geom_smooth(
    method = "loess",
    se = FALSE
    ) +
  labs(
    title = "Evolución de la relación € / Kg",
    x = NULL, y = "€ / Kg"
  )

# ggsave(
#   filename = here("report/graphs", "eur_kg.png")
# )

# Kg por meses ------------------------------------------------------------

# evolución del total de kg
grafs$rac_kg_mm <-
datos_mes %>% 
  select(fecha:total_kg) %>% 
  pivot_longer(
    cols = -fecha,
    names_to = "tipo",
    values_to = "valor"
  ) %>% 
  mutate(
    tipo = case_when(
      tipo == "racimos" ~  "Número de racimos",
      tipo == "total_kg" ~ "Kg totales"
    ) 
    ) %>% 
  ggplot(aes(x = fecha, y = valor)) +
  geom_col(fill = "steelblue3") +
  facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  labs(
    title = "Evolución mensual del número de racimos y peso",
    x = NULL, y = NULL
  )
# ggsave(
#   filename = here("report/graphs", "mes_racskg.png")
# )
# los totales mensuales comparados
grafs$kg_mm <-
datos_mes_kg %>% 
  select(fecha:mm, total_kg) %>% 
  ggplot(aes(x = mm, y = total_kg, fill = as_factor(aa))) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparativa de la producción mensual (Kg)",
    subtitle = "Producción de todas las categorías",
    x = NULL, y = NULL, fill = "Anualidades"
  ) +
  scale_fill_brewer(palette = "Paired")

# ggsave(
#   filename = here("report/graphs", "mes_aa_total_kg.png")
# )

# el acumulado de los totales
grafs$kg_mm_acum <-
datos_mes_kg %>% 
  select(fecha:mm, total_kg_acum) %>% 
  ggplot(aes(x= mm, y = total_kg_acum, color = as_factor(aa))) +
  geom_point() +
  geom_line(aes(group = aa)) +
  # valores del último mes y comparativa con años anteriores
  geom_point(
    data = datos_mes_kg %>% filter(month(fecha) == month(max(fecha))),
    aes(x = mm, y = total_kg_acum), color = "black", shape = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = datos_mes_kg %>% filter(month(fecha) == month(max(fecha))),
    aes(
      label = format(total_kg_acum, big.mark=".", decimal.mar = ",") %>%
        str_c(.,"kg", sep=" ")
    ),
    nudge_x = .8, size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Acumulados mensuales de la producción total (Kg)",
    x = NULL, y = NULL, color = "Anualidades"
  )
# ggsave(
#   filename = here("report/graphs", "mes_aa_total_kg_acum.png")
#   )

# los kg por categorías, comparados
grafs$kg_cat <-
datos_mes_kg %>% 
  select(!c(total_kg, ends_with("acum"))) %>% 
  pivot_longer(
    cols = !c(fecha, aa, mm),
    names_to  = "cat",
    values_to = "peso"
  ) %>% 
  mutate(
    cat = case_when(
      str_detect(cat, "psup") ~ "Psup",
      str_detect(cat, "segu") ~ "Segunda",
      TRUE                    ~ "Premium"
    )
  ) %>% 
  ggplot(aes(x = mm, y = peso, fill = as_factor(aa))) +
  geom_col(position = "dodge") +
  facet_wrap(~cat, ncol = 1) +
  labs(
    title = "Comparativa de la producción mensual (Kg)",
    subtitle = "Producción de cada una de las categorías",
    x = NULL, y = NULL, fill = "Anualidades"
  ) +
  scale_fill_brewer(palette = "Paired")

# ggsave(
#   filename = here("report/graphs", "mes_aa_total_kg_cat.png")
# )

# acumulados de los kg por categorías
grafs$kg_cat_acum <-
datos_mes_kg %>% 
  select(!c(ends_with("kg"), total_kg_acum)) %>% 
  pivot_longer(
    cols = !c(fecha, aa, mm),
    names_to = "cat",
    values_to = "peso"
  ) %>% 
  mutate(
    cat = case_when(
      str_detect(cat, "psup") ~ "Psup",
      str_detect(cat, "segu") ~ "Segunda",
      TRUE                    ~ "Premium"
    )
  ) %>% 
  ggplot(aes(x = mm, y = peso, color = as_factor(aa))) +
  geom_point(alpha = .5) +
  geom_line(aes(group = aa)) +
  facet_wrap(~cat, ncol = 1) +
  labs(
    title = "Acumulados de la producción mensual (Kg)",
    subtitle = "Producción de cada una de las categorías",
    x = NULL, y = NULL, color = "Anualidades"
  ) 
  # scale_color_brewer(palette = "Paired")
# ggsave(
#   filename = here("report/graphs", "mes_aa_total_kg_cat_acum.png")
# )

# relación entre racimos y kg
datos_mes %>% 
  select(fecha, kg_rac) %>% 
  ggplot(aes(x = fecha, y = kg_rac)) +
  geom_point() +
  geom_smooth(se = FALSE, method = "loess") +
  labs(
    title = "Evolución del Kg por racimo",
    x = NULL, y = NULL
  )
# ggsave(
#   filename = here("report/graphs", "kg_racimo.png")
# )  

# puedo hacer la relación entre racimos y kg en base semanal
grafs$rac_kg_sem <-
datos_sem %>% 
  select(fecha, semana, racimos, peso_med) %>% 
  filter(year(fecha) > 2020) %>% 
  pivot_longer(
    !c(fecha, semana),
    names_to = "medida",
    values_to = "valor"
  ) %>% 
  mutate(
    aa = year(fecha),
    medida = case_when(
      medida == "racimos"  ~ "Racimos",
      medida == "peso_med" ~ "Kg (media)"
    )
  ) %>% 
  ggplot(aes(x = semana, y = valor, color = as_factor(aa))) +
  geom_point(alpha = .5) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = 34, color = "grey75") +
  facet_wrap(~medida, ncol = 1, scales = "free_y") +
  labs(
    title    = "Comparativa de la media de Kg y número de racimos",
    subtitle = "Semanas de cada anualidad",
    caption  = "Marcada la semana 34",
    x = "Semanas", y = NULL, color = "Anualidades"
  )
# ggsave(
#   filename = here("report/graphs", "kg_racimo_sem.png")
# )

# Guardo las gráficas -----------------------------------------------------

save(grafs, file = here("data/processed", "finca_graficas.RData"))
