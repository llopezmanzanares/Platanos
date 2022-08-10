# Análisis de la temporalidad de los precios

library(tidyverse)
library(here)

if (file.exists(here("data/processed", "platanos.RData")))
  load(here("data/processed", "platanos.RData"))


precios_sem %>% 
  mutate(
    sem = str_extract(periodo, pattern = "\\d{2}$") %>% 
      tolower() %>% 
      as.numeric(),
    .after = 1
  ) %>% 
  filter(territorio == "tenerife") %>% 
  ggplot(aes(sem, precio, color = as.factor(anualidad))) +
  # geom_smooth(se = F, span = 0.8) +
  geom_line() +
  theme_light()
