# Análisis de la temporalidad de los precios

# modifico el dataset -----------------------------------------------------

precios_sem <- precios_sem %>% 
  mutate(
    sem = str_extract(periodo, pattern = "\\d{2}$") %>% 
      tolower() %>% 
      as.numeric(),
    .after = 1
  )

# precios todas las islas -------------------------------------------------

precios_sem %>% 
  filter(territorio != "canarias") %>% 
  mutate(
    territorio = case_when(
      territorio == "gran.canaria" ~ "Gran Canaria",
      territorio == "la.palma" ~ "La Palma",
      territorio == "tenerife" ~ "Tenerife"
    )
  ) %>% 
  ggplot(aes(semana, precio, color = territorio)) +
  geom_line() +
  labs(
    title = "Precios medios percibidos, €/kg",
    color = "Isla",
    x = "Semana del año",
    y = NULL
  ) +
  theme_light()

ggsave(
  here("report/graphs", "precio_islas.pdf")
    )

# precios Tenerife --------------------------------------------------------

precios_sem %>% 
  filter(territorio == "tenerife") %>% 
  ggplot(aes(sem, precio, color = as_factor(anualidad))) +
  geom_line(size = 1) +
  labs(
    title = "Precios medios percibidos, €/kg",
    subtitle = "Isla de Tenerife",
    x = "Semana",
    y = NULL,
    color = "Año"
  ) +
  scale_y_continuous(position = "right") +
  theme_light()

ggsave(
  here("report/graphs", "precio_tfe.pdf")
)

