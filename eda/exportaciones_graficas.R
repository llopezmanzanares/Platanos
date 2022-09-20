# Análisis de la temporalidad de las exportaciones

# Exportaciones por islas -------------------------------------------------


exportaciones %>% 
  filter(isla != "Canarias") %>% 
  ggplot(aes(mes, total, color = isla)) +
  geom_line() +
  labs(
    title = "Exportaciones de plátanos, Tn",
    y = NULL,
    x = NULL
  ) +
  scale_y_continuous(position = "right") +
  theme_light()

ggsave(
  here("report/graphs", "expor_islas.png"),
  device = "png"
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
  geom_line() +
  labs(
    title = "Exportaciones de plátanos, Tn",
    subtitle = "Isla de Tenerife",
    x = NULL,
    y = NULL
  ) +
  theme_light()

ggsave(
  here("report/graphs", "expor_tfe.png"),
  device = "png"
)

