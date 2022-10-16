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
  pivot_longer(
    cols = !fecha,
    names_to = "cat",
    values_to = "kg"
  ) %>% 
  mutate(
    aa = year(fecha),
    mm = month(fecha, label = TRUE),
    # tengo que encontrar una mejor manera de hacer esto
    kg_2021 = ifelse(aa == 2021, kg, NA),
    kg_2022 = ifelse(aa == 2022, kg, NA)
  ) %>% 
  # me estoy liando
  select(fecha, cat, starts_with("kg_"))

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
  geom_point()
  geom_line()

# los kg por categorías, comparados
datos_mes_kg %>% 
  select(-total_kg) %>% 
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
