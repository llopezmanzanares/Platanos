library(tidyverse)

imp <- read_csv("data/processed/imp_tfe.csv",
                show_col_types = F)

pr_m <- read_csv("data/processed/pr_tfe_mes.csv",
                 show_col_types = F)

mes <- imp %>% 
  left_join(pr_m, by = "fecha") %>% 
  mutate(
    fecha = as.Date(fecha),
    pd_tot = pen + extr
  ) 

meses <- mes %>% 
  pivot_longer(
    !fecha,
    names_to = "tipo",
    values_to = "valor"
  )

ggplot(mes, aes(fecha, pmp)) +
  geom_line()

meses %>% 
  filter(
    tipo %in% c("pmp", "pd_tot")
  ) %>% 
ggplot(aes(fecha, valor, color = year(fecha))) +
  geom_line(size = 1.2) +
  facet_wrap(~tipo, 
             ncol = 1,
             scales = "free_y")
