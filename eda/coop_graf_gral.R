library(tidyverse)
library(here)

dir <- list(
  pro = "data/processed"
)

load(here(dir$pro, "datos_finca.RData"))

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
