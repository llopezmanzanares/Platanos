# Análisis de los cortes (pesos)


# Modifico el dataset -----------------------------------------------------

Tn <- toneladas %>% 
  filter(territorio != "canarias") %>% 
  mutate(
    territorio = str_replace(territorio, pattern = "\\.", replacement = " ") %>% 
      str_to_title()
  )
