# Análisis de la temporalidad de las exportaciones

# Exportaciones por islas -------------------------------------------------


exportaciones %>% 
  filter(isla != "Canarias") %>% 
  ggplot(aes(mes, total, color = isla)) +
  geom_line(size = 1) +
  labs(
    title = "Exportaciones de plátanos, Tn",
    y = NULL,
    x = NULL
  ) +
  scale_y_continuous(position = "right") +
  theme_light()

ggsave(
  here("reports/graph", "expor_islas.pdf"),
  width = 11,
  height = 9,
  units = "in"
)


# Exportaciones Tenerife --------------------------------------------------

exportaciones %>% 
  filter(isla == "Tenerife") %>% 
  select(mes, total, peninsula, extranjero) %>% 
  pivot_longer(
    !mes,
    names_to = "destino",
    values_to = "tn"
  ) %>% 
  ggplot(aes(mes, tn, color = destino)) +
  geom_line(size = 1) +
  labs(
    title = "Exportaciones de plátanos, Tn",
    subtitle = "Isla de Tenerife",
    x = NULL,
    y = NULL
  ) +
  theme_light()

ggsave(
  here("reports/graph", "expor_tfe.pdf"),
  width = 11,
  height = 9,
  units = "in"
)

