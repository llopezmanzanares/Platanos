# Generación de las gráficas a partir de los datos de ISTAC

library(tidyverse)


istac_grafs <- list()

# función para modificar la posición de la leyenda y el eje y en las gráficas
my_plot <- function(...){
  ggplot(...) + 
    theme(
      legend.position = "bottom"
    ) +
    scale_y_continuous(position = "right")
}

# Evolución de la producción ----------------------------------------------

istac_grafs$tn <- istac$toneladas %>% 
  filter(territorio != "canarias") %>% 
  mutate(
    territorio = str_replace(territorio, "\\.", " ") %>% 
      str_to_title()
  ) %>% 
  my_plot(aes(x = anualidad, y = tn/1000, color = territorio)) +
  geom_line(aes(group = territorio)) +
  labs(
    title = "Toneladas producidas, por isla",
    caption = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL, color = NULL
  )

istac_grafs$exp_tot <- istac$exportaciones %>% 
  filter(
    isla %in% c("Gran Canaria", "Tenerife", "La Palma"),
    anualidad >= 2018
    ) %>% 
  my_plot(aes(x = lubridate::month(mes, label = TRUE), y = total/1000, color = as_factor(anualidad))) +
  geom_point(alpha = .5) +
  geom_smooth(aes(group = anualidad), se = FALSE) +
  facet_wrap(~isla, ncol = 1) +
  labs(
    title = "Evolución mensual de las exportaciones",
    subtitle = "Miles de Tn",
    caption = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL, color = NULL
  )


# Gráficas de precios -----------------------------------------------------


# Gráficas de superficie --------------------------------------------------

istac_grafs$sup <- istac$superficie %>% 
  filter(
    territorio %in% c("Gran Canaria", "Tenerife", "La Palma")
  ) %>% 
  pivot_longer(
    cols = !c("territorio", "anualidad"),
    names_to = "medida",
    values_to = "valor"
  ) %>% 
  my_plot(aes(x = as_factor(anualidad), y = valor, color = territorio)) +
  geom_line(aes(group=territorio)) +
  facet_wrap(~medida, ncol = 1) +
  labs(
    title = "Evolución de la superficie cultivada (ha)",
    x = NULL, y = NULL, color = NULL
  )

# Guardo las gráficas -----------------------------------------------------

save(istac_grafs, file = here("data/processed", "istac_graficas.RData"))  
