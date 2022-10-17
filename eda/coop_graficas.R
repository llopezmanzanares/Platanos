# Gráficas usadas en el informe Finca.Rmd


# Carga de datos ----------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

theme_set(
  theme_minimal()
  # theme_light()
)

# objeto datos_sem
load(file = here("data/processed", "datos_finca.RData"))

# Datos mensuales ---------------------------------------------------------

# Es más útil trabajar con los datos mensuales
# solo tengo un dato de 2020, así que lo elimino
datos_mes <- 
  filter(datos_sem, year(fecha) > 2020) %>% 
  select(
    !c(semana,             # el dato de la semana no aporta información
       total_fac,          # el total facturado es muy similar al importe, lo quito
       ends_with(c("_med", # tampoco las medias y porcentajes
                   "_pc",
                   "_eurkg")
       )
    )
  ) %>% 
  mutate(
    fecha = rollforward(fecha)
    # esto da problemas si quiero recomponer la fecha posteriormente
    # aa = year(fecha),
    # mm = month(fecha, label = TRUE),
  ) %>% 
  group_by(fecha) %>% 
  summarise(
    across(everything(), sum),
    .groups = "drop"
  )

# Relación euros vs Kg ----------------------------------------------------

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

ggsave(
  filename = here("report/graphs", "eur_kg.png")
)

# Kg por meses ------------------------------------------------------------

datos_mes_kg <- datos_mes %>% 
  select(fecha, ends_with("kg")) %>% 
  group_by(aa = year(fecha)) %>% 
  mutate(
    across(ends_with("_kg"), cumsum, .names = "{.col}_acum")
    ) %>% 
  ungroup()

  
# los totales mensuales comparados
datos_mes_kg %>% 
  select(fecha, total_kg) %>% 
  mutate(
    aa = year(fecha),
    mm = month(fecha, label = TRUE)
    ) %>% 
  ggplot(aes(x = mm, y = total_kg, fill = as_factor(aa))) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparativa de la producción mensual (Kg)",
    subtitle = "Producción de todas las categorías",
    x = NULL, y = NULL, fill = "Anualidades"
  ) +
  scale_fill_brewer(palette = "Paired")

ggsave(
  filename = here("report/graphs", "mes_aa_total_kg.png")
)

# el acumulado de los totales
datos_mes_kg %>% 
  select(fecha, total_kg_acum) %>% 
  mutate(
    aa = year(fecha),
    mm = month(fecha, label = TRUE)
  ) %>% 
  ggplot(aes(x= mm, y = total_kg_acum, color = as_factor(aa))) +
  geom_point() +
  geom_line(aes(group = aa)) +
  labs(
    title = "Acumulados mensuales de la producción total (Kg)",
    x = NULL, y = NULL, color = "Anualidades"
  )
ggsave(filename = here("report/graphs", "mes_aa_total_kg_acum.png")
       )

# los kg por categorías, comparados
datos_mes_kg %>% 
  select(!c(total_kg, ends_with("acum"))) %>% 
  mutate(
    aa = year(fecha),
    mm = month(fecha, label = TRUE),
    .keep = "unused"
  ) %>% 
  pivot_longer(
    cols = !c(aa, mm),
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

ggsave(
  filename = here("report/graphs", "mes_aa_total_kg_cat.png")
)

# acumulados de los kg por categorías
datos_mes_kg %>% 
  select(!c(ends_with("kg"), total_kg_acum, aa)) %>% 
  pivot_longer(
    cols = -fecha,
    names_to = "cat",
    values_to = "peso"
  ) %>% 
  mutate(
    aa = year(fecha),
    mm = month(fecha, label = TRUE),
    cat = case_when(
      str_detect(cat, "psup") ~ "Psup",
      str_detect(cat, "segu") ~ "Segunda",
      TRUE                    ~ "Premium"
    )
  ) %>% 
  ggplot(aes(x = mm, y = peso, color = as_factor(aa))) +
  geom_line(aes(group = aa)) +
  facet_wrap(~cat, ncol = 1) +
  labs(
    title = "Acumulados de la producción mensual (Kg)",
    subtitle = "Producción de cada una de las categorías",
    x = NULL, y = NULL, color = "Anualidades"
  ) 
  # scale_color_brewer(palette = "Paired")
ggsave(
  filename = here("report/graphs", "mes_aa_total_kg_cat.png")
)
