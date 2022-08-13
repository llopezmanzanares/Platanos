# Análisis de los cortes (pesos)


# Modifico el dataset -----------------------------------------------------

Tn <- toneladas %>% 
  filter(territorio != "canarias") %>% 
  mutate(
    anualidad = as.numeric(anualidad),
    territorio = str_replace(territorio, pattern = "\\.", replacement = " ") %>% 
      str_to_title()
  )

# Tn todas las islas ------------------------------------------------------

Tn %>% 
  ggplot(aes(anualidad, tn, color = territorio)) +
  geom_line(size = 1) +
  labs(
    title = "Producción agrícola por islas, Tn",
    x = NULL,
    y = NULL,
    color = "Isla"
  ) +
  scale_y_continuous(position = "right") +
  theme_light()

ggsave(
  here("report/graphs", "prod_islas.pdf")
)
