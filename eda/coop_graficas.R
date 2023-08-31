# Generación de las gráficas usadas en el informe Finca.Rmd y en
# Produccion_Finca.qmd

# Versión: 2023-08-31

# Modificaciones a realizar:
# concordancia de colores en todas las gráficas


# Carga de datos ----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

# theme_set(
#   theme_minimal()
#   # theme_light()
# )

# cargo datos_mes y obtengo datos_mes_kg
source(file = here("eda/", "coop_graficas_datos.R"))

coop_grafs <- list()

# función para modificar la posición de la leyenda y el eje y en las gráficas
my_plot <- function(...){
  ggplot(...) + 
    theme(
      legend.position = "bottom"
    ) +
    scale_y_continuous(position = "right")
}
  

# Relación euros vs Kg ----------------------------------------------------

coop_grafs$eur_kg <-
coop_ds$mes %>% 
  select(fecha, starts_with("total")) %>% 
  mutate(
    eur_kg = total_eur / total_kg,
  ) %>% 
  my_plot(aes(x = fecha, y = eur_kg)) +
  geom_point(alpha = .8) +
  geom_smooth(
    method = "loess",
    se = FALSE
  ) +
  labs(
    title = "Evolución de la relación € / Kg",
    x = NULL, y = NULL
  )
  
# Kg por meses ------------------------------------------------------------

# evolución del total de kg
coop_grafs$rac_kg_mm <-
  coop_ds$mes %>% 
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
  my_plot(aes(x = fecha, y = valor)) +
  geom_col(fill = "steelblue3") +
  facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  labs(
    title = "Evolución mensual del número de racimos y peso",
    x = NULL, y = NULL
  )


# los totales mensuales comparados
coop_grafs$kg_mm <-
  coop_ds$mes_kg %>% 
  select(fecha:mm, total_kg) %>%
  my_plot(aes(x = mm, y = total_kg, fill = as_factor(aa))) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparativa de la producción mensual (Kg)",
    subtitle = "Producción de todas las categorías",
    x = NULL, y = NULL, fill = "Anualidades"
  ) +
  scale_fill_brewer(palette = "Paired")


# el acumulado de los totales
coop_grafs$kg_mm_acum <-
  coop_ds$mes_kg %>% 
  select(fecha:mm, total_kg_acum) %>% 
  my_plot(aes(x= mm, y = total_kg_acum, color = as_factor(aa))) +
  geom_point() +
  geom_line(aes(group = aa)) +
  # valores del último mes y comparativa con años anteriores
  geom_point(
    data = coop_ds$mes_kg %>% filter(month(fecha) == month(max(fecha))),
    aes(x = mm, y = total_kg_acum), color = "black", shape = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = coop_ds$mes_kg %>% filter(month(fecha) == month(max(fecha))),
    aes(
      label = format(total_kg_acum, big.mark=".", decimal.mark = ",") %>%
        str_c(.,"kg", sep=" ")
    ),
    nudge_x = .8, size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Acumulados mensuales de la producción total (Kg)",
    x = NULL, y = NULL, color = "Anualidades"
  )

# los kg por categorías, comparados
coop_grafs$kg_cat <-
  coop_ds$mes_kg %>% 
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
  my_plot(aes(x = mm, y = peso, fill = as_factor(aa))) +
  geom_col(position = "dodge") +
  facet_wrap(~cat, ncol = 1) +
  labs(
    title = "Comparativa de la producción mensual (Kg)",
    subtitle = "Producción de cada una de las categorías",
    x = NULL, y = NULL, fill = "Anualidades"
  ) +
  scale_fill_brewer(palette = "Paired")


# acumulados de los kg por categorías
coop_grafs$kg_cat_acum <-
  coop_ds$mes_kg %>% 
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
  my_plot(aes(x = mm, y = peso, color = as_factor(aa))) +
  geom_point(alpha = .5) +
  geom_line(aes(group = aa)) +
  facet_wrap(~cat, ncol = 1) +
  labs(
    title = "Acumulados de la producción mensual (Kg)",
    subtitle = "Producción de cada una de las categorías",
    x = NULL, y = NULL, color = "Anualidades"
  ) 
  # scale_color_brewer(palette = "Paired")

# relación entre racimos y kg
# coop_ds$mes %>% 
#   select(fecha, kg_rac) %>% 
#   my_plot(aes(x = fecha, y = kg_rac)) +
#   geom_point() +
#   geom_smooth(se = FALSE, method = "loess") +
#   labs(
#     title = "Evolución del Kg por racimo",
#     x = NULL, y = NULL
#   )

# puedo hacer la relación entre racimos y kg en base semanal
coop_grafs$rac_kg_sem <-
  coop_ds$sem %>% 
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
  my_plot(aes(x = semana, y = valor, color = as_factor(aa))) +
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


# Importes ----------------------------------------------------------------

coop_grafs$eur_mm_acum <- coop_ds$mes %>% 
  select(fecha, total_eur) %>% 
  group_by(anualidad = year(fecha)) %>% 
  mutate(
    eur_acum = cumsum(total_eur)
  ) %>% 
  ungroup() %>% 
  mutate(
    mes = month(fecha, label = TRUE)
  ) %>% 
  my_plot(aes(x = mes, y = eur_acum, color = as_factor(anualidad))) +
  geom_point(alpha = .5) +
  geom_line(aes(group = anualidad)) +
  # último valor del último año, comparado con los anteriores
  geom_point(
    data = . %>% filter(month(fecha) == month(max(fecha))),
    aes(x = mes, y = eur_acum), color = "black", shape = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = . %>% filter(month(fecha) == month(max(fecha))),
    aes(
      label = format(eur_acum, big.mark=".", decimal.mark = ",") %>%
        str_c(.,"eur", sep=" ")
    ),
    nudge_x = .9, size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Acumulados mensuales de los importes (€)",
    x = NULL, y = NULL, color = "Anualidades"
  )

# comparo los importes por categorías

coop_grafs$eur_cat <- coop_ds$mes %>% 
  select(fecha, ends_with("eur"), -total_eur) %>% 
  pivot_longer(
    cols = -fecha,
    names_to = "cat",
    values_to = "eur"
  ) %>% 
  mutate(
    mes = month(fecha, label = TRUE),
    anualidad = as_factor(year(fecha)),
    cat = case_when(
      str_detect(cat, "prem") ~ "Premium",
      str_detect(cat, "psup") ~ "Psup",
      TRUE                    ~ "Segunda"
    )
  ) %>% 
  my_plot(aes(x = mes, y = eur, color = anualidad)) +
  geom_point(alpha = .5) +
  geom_line(aes(group = anualidad)) +
  facet_wrap(~cat, ncol = 1) +
  labs(
    title = "Comparativa de la evolución de los importes por categoría",
    subtitle = "Valores en €",
    x = NULL, y = NULL, color = NULL
  )

# Guardo las gráficas -----------------------------------------------------

save(coop_grafs, file = here("data/processed", "finca_graficas.RData"))
