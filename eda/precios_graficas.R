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
    territorio = str_replace(territorio, pattern = "\\.", replacement = " ") %>% 
      str_to_title()
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
  here("report/graphs", "precio_islas.png"),
  device = "png"
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
  here("report/graphs", "precio_tfe.png"),
  device = "png"
)

# heatmap test
precios_sem %>% 
  filter(territorio == "tenerife") %>% 
  ggplot(aes(x = sem, y = anualidad, fill = precio)) +
  geom_tile() +
  geom_vline(xintercept = 34) +
  scale_fill_gradient(low = "white", high = "steelblue4") +
  theme_light()

ggsave(
  here("report/graphs", "precio_tfe_hm.png"),
  device = "png"
)
