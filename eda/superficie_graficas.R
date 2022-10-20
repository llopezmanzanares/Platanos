# Gráficas de la superficie cultivada


# Total superficie --------------------------------------------------------

superficie %>% 
  filter(territorio != "Canarias") %>% 
  mutate(
    hectareas = `Regadío: Que aún no produce` + `Regadío: En producción` + `CULTIVO PROTEGIDO`,
    .keep = "unused"
  ) %>% 
  ggplot(aes(x = anualidad, y = hectareas, color = territorio)) +
  geom_line() +
  labs(
    title = "Evolución anual de la superficie cultivada",
    subtitle = "Hectáreas",
    x = NULL, y = NULL, color = "Territorio"
  ) +
  scale_y_continuous(position = "right") +
  theme_light()

ggsave(
  filename = here("report/graphs/hectareas_islas.png")
)

superficie %>% 
  filter(territorio == "La Palma") %>% 
  pivot_longer(
    cols = !c("territorio", "anualidad"),
    names_to = "medida",
    values_to = "hectareas"
  ) %>% 
  ggplot(aes(x = anualidad, y = hectareas, color = medida)) +
  geom_line() +
  labs(
    title = "Evolución anual de la superficie cultivada",
    subtitle = "La Palma",
    caption = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL, color = NULL
  ) +
  scale_y_continuous(position = "right") +
  theme_light() +
  theme(
    legend.position = "bottom"
  )
