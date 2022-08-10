# Análisis de la temporalidad de los precios

library(tidyverse)

precios_sem %>% 
  filter(territorio == "Tenerife") %>% 
  ggplot(aes(semana, precio, color = lubridate::year(semana))) +
  geom_smooth(se = F, span = 0.8) +
  geom_line() +
  theme_light()
