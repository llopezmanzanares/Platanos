library(tidyverse)
library(here)

# TODO actualizar con el nuevo conjunto de datos

dir <- list(
  pro = "data/processed"
)

load(here(dir$pro, "datos_finca.RData")) # objeto datos_sem


# Gráficas generales ------------------------------------------------------

# evolución temporal de la media de precios, pesos y racimos
gral <- select(datos, fecha, racimos, ends_with("med")) %>% 
  pivot_longer(
    -fecha,
    names_to = "tipo",
    values_to = "valor"
  )

ggplot(
  data = gral,
  aes(x = fecha, y = valor)
) +
  geom_point() +
  geom_line() +
  facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  theme_light()

ggsave(
  here("finca_general.pdf"),
  width = 5,
  height = 15)

# evolución temporal del peso de los cortes, por categoría
pesos <- select(datos, fecha, ends_with("_kg")) %>% 
  pivot_longer(
    -fecha,
    names_to = "tipo",
    values_to = "valor"
  )

ggplot(
  data = pesos,
  aes(x = fecha, y = valor)
) +
  geom_line() +
  facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  theme_light()

ggsave(
  here("cats_pesos.pdf")
  # width = 5,     # los tamaños de las gráficas son un poco raros, ¿individuales?
  # height = 15
)

# evolución temporal del precio del kg, por categoría
precios <- select(datos, fecha, ends_with("_eurkg")) %>% 
  pivot_longer(
    -fecha,
    names_to = "tipo",
    values_to = "valor"
  )

ggplot(
  data = precios,
  aes(x = fecha, y = valor, color = tipo)
) +
  geom_line() +
#  facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  theme_light()

ggsave(
  here("cats_precios.pdf")
  # width = 5,     # los tamaños de las gráficas son un poco raros, ¿individuales?
  # height = 15
)

# evolución temporal de los pagos realizados, por categoría
ingresos <- select(datos, fecha, ends_with("_eur")) %>% 
  pivot_longer(
    -fecha,
    names_to = "tipo",
    values_to = "valor"
  )

ggplot(
  data = ingresos,
  aes(x = fecha, y = valor, color = tipo)
) +
  geom_line() +
#  facet_wrap(~tipo, ncol = 1, scales = "free_y") +
  theme_light()

ggsave(
  here("ingresos.pdf")
  # width = 5,     # los tamaños de las gráficas son un poco raros, ¿individuales?
  # height = 15
)

# Comparativas mensuales --------------------------------------------------

datos_mes <- datos_sem %>% 
  filter(year(fecha) > 2020) %>% 
  # mutate(
  #   anualidad = year(fecha)
  # ) %>% 
  group_by(anualidad = year(fecha), mes = month(fecha)) %>% 
  summarise(
    across(c(racimos, ends_with(c("_kg", "_eur"))), sum)
    )

kg_mes_cat <- datos_mes %>% 
  select(anualidad, mes,  premium_kg, psup_kg, segunda_kg) %>% 
  pivot_longer(
    -c(anualidad, mes),
    names_to = "cat",
    values_to = "valor"
  )

ggplot(
  data = kg_mes_cat,
  aes(x = as_factor(mes), y = valor, fill = as_factor(anualidad))
) +
  geom_col(position = "dodge") +
  facet_wrap(~cat, ncol = 1, scales = "free_y") +
  labs(
    # title = "Todas las categorías",
    subtitle = "Comparativa mensual del peso",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")

ggsave(here("report/graphs", "kg_mes_cat.pdf"))

datos_mes %>% 
  select(anualidad, mes, total_kg) %>% 
  ggplot(
    aes(x = as_factor(mes), y = total_kg, fill = as_factor(anualidad))
  ) +
  geom_col(position = "dodge") +
  labs(
    subtitle = "Comparativa mensual del peso total",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Dark2")

ggsave(here("report/graphs", "kg_mes_total.pdf"))
