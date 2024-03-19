# Generación de las gráficas usadas en el informe Finca.Rmd y en
# Produccion_Finca.qmd

# Versión: 2024-03-19

# Modificaciones a realizar:
# concordancia de colores en todas las gráficas


# Carga de datos ----------------------------------------------------------

# library(tidyverse)
# library(here)
library(ggdist)

# mi paleta de colores
col_pal <- c("cream" = "#f4f7be", "mindaro" = "#e5f77d", "ecru" = "#deba6f",
             "wine"  = "#823038", "licoire" = "#1e000e")

eur <- scales::label_currency(suffix = "€", prefix = NULL,
                              big.mark = ".", decimal.mark = ",")

# datos de la cooperativa (semana, mes y kg mensuales)
load(here("data/processed/datos_finca.RData"))

# función para modificar la posición de la leyenda y el eje y en las gráficas
my_plot <- function(...){

  ggplot(...) + 
    theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    ) +
    scale_y_continuous(position = "right") +
    scale_color_brewer(
      palette = "YlGn", 
      aesthetics = c("colour", "fill") # ambas al mismo tiempo
    )
}
  

# Resumen gral ------------------------------------------------------------

# Distribución de los racimos, ingresos y pesos mensuales por anualidades
coop_grafs$mensuales <-
  coop_ds$mes  |>  
  select(fecha_aa, Racimos = racimos, Ingresos = total_eur, Pesos = total_kg)  |>  
  pivot_longer(!fecha_aa) |>  
  ggplot(aes(x = fecha_aa, y = value)) +
  geom_boxplot(colour = "springgreen4", outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  labs(
    title = "Evolución de la distribución anual de los valores mensuales de\nIngresos, Pesos y número de Racimos",
    x = NULL, y = NULL
  )

# Distribución de los pesos, al detalle
coop_grafs$dist_kg <- 
  coop_ds$semanas |> 
  filter(tipo == "total_kg") |> 
  ggplot(aes(x = valor)) +
  geom_boxplot(width = 0.1, fill = col_pal["wine"]) +
  geom_dots(
    dotsize = 0.1, height = 0.55, fill = col_pal["wine"],
    side = "bottom", position = position_nudge(y = -0.075)
    ) +
  stat_slab(
    fill = col_pal["wine"],
    position = position_nudge(y = 0.075), height = 0.75
    ) +
  labs(
    title = "Distribución de los pesos semanales para todo el conjunto de datos",
    x = "Kg", y = element_blank()
  ) +
  theme(
    plot.title.position = "plot",
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  scale_x_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))

# Distribución Kg por anualidades
coop_grafs$dist_kg_aa <- 
  coop_ds$semanas |> 
  filter(tipo == "total_kg") |>
  filter(fecha_aa != "2020") |> 
  ggplot(aes(y = valor, fill = fecha_aa)) +
  geom_boxplot(width = 0.1, outliers = FALSE) +
  geom_dots(
    dotsize = 0.1, height = 0.55,
    side = "bottom", position = position_nudge(x = -0.075)
  ) +
  stat_slab(
    position = position_nudge(x = 0.075), height = 0.75
  ) +
  facet_wrap(~fecha_aa, nrow = 1) +
  labs(
    # title = "Distribución de los pesos semanales (Kg)",
    x = element_blank(), y = element_blank()
  ) +
  theme(
    plot.title.position = "plot",
    legend.position = "none",
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))

# Relación euros vs Kg ----------------------------------------------------

coop_grafs$eur_kg <-
  coop_ds$mes |> 
  select(fecha, starts_with("total")) |>  
  mutate(eur_kg = total_eur / total_kg, .keep = "unused") |>  
  my_plot(aes(x = fecha, y = eur_kg)) +
  geom_point(alpha = .8, color = "yellow4") +
  geom_smooth(method = "loess", se = FALSE, color = "springgreen4") +
  labs(
    title = "Evolución de la relación € / Kg",
    x = NULL, y = NULL
  )
  

# Evolucion mensual -------------------------------------------------------

# evolución del total de kg
coop_grafs$kg_meses <-
  coop_ds$mes  |>  
  my_plot(aes(x = fecha, y = total_kg)) +
  geom_point(shape = 21, fill = col_pal["cream"], color = col_pal["wine"]) +
  geom_smooth(
    method = "loess", formula = y ~ x,
    color = col_pal["wine"], fill = col_pal["cream"]
  ) +
  labs(
    title = "Evolución de la producción mensual (Kg)",
    x = NULL, y = NULL
  ) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ","))

# evolución del total de EUROS
coop_grafs$eur_meses <-
  coop_ds$mes  |>  
  my_plot(aes(x = fecha, y = total_eur)) +
  geom_point(shape = 21, fill = col_pal["cream"], color = col_pal["wine"]) +
  geom_smooth(
    method = "loess", formula = y ~ x,
    color = col_pal["wine"], fill = col_pal["cream"]
  ) +
  labs(
    title = "Evolución de la producción mensual (€)",
    x = NULL, y = NULL
  ) +
  theme(plot.title.position = "plot") +
  scale_y_continuous(labels = eur)

# evol de la relación de categoría PREMIUM respecto del total
coop_grafs$prop_premium |> 
  coop_ds$mes |> 
  mutate(
    prc_kg = premium_kg / total_kg,
    prc_eur= premium_eur / total_eur
  ) |> 
  select(fecha, starts_with("prc")) |> 
  pivot_longer(
    cols = !fecha,
    names_prefix = "prc_",
    names_transform = list(name = \(x) ifelse(x == "kg", "Peso", "Precio"))
    ) |> 
  ggplot(aes(x = fecha, y = value)) +
  # probar a meter dos líneas diferentes, así puedo jugar con el alpha
  geom_line(aes(color = name), linewidth = 1) +
  labs(
    title = "Porcentaje de categoría Premium",
    x = element_blank(), y = element_blank(), color = element_blank()
  ) +
  theme(plot.title.position = "plot", legend.position = "bottom") +
  scale_y_continuous(labels = scales::label_percent(decimal.mark = ","))

# los totales mensuales comparados
coop_grafs$kg_mm <-
  coop_ds$mes_kg %>% 
  select(fecha:mm, total_kg) %>%
  my_plot(aes(x = mm, y = total_kg, fill = as_factor(aa))) +
  geom_col(position = "dodge") +
  labs(
    title = "Producción mensual (Kg)",
    subtitle = "Todas las categorías",
    x = NULL, y = NULL, fill = "Anualidades"
  )




# el acumulado de los totales
coop_grafs$kg_mm_acum <-
  coop_ds$mes_kg %>% 
  select(fecha:mm, total_kg_acum) %>% 
  my_plot(aes(x= mm, y = total_kg_acum, color = as_factor(aa))) +
  geom_line(aes(group = aa), linewidth = 1.1) +
  geom_point() +
  # valores del último mes y comparativa con años anteriores
  geom_point(
    data = coop_ds$mes_kg %>% filter(month(fecha) == month(max(fecha))),
    aes(x = mm, y = total_kg_acum), color = "black", shape = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = coop_ds$mes_kg %>% filter(month(fecha) == month(max(fecha))),
    aes(
      label = format(total_kg_acum, big.mark=".", decimal.mark = ",") %>%
        str_c(.,"kg", sep=" ")
    ),
    nudge_x = .8, size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Acumulados mensuales de la producción total (Kg)",
    x = NULL, y = NULL, color = "Anualidades"
  ) + 
  scale_y_continuous(labels = eur, position = "right")

# los kg por categorías, comparados
coop_grafs$kg_cat <-
  coop_ds$mes_kg %>% 
  select(!c(total_kg, ends_with("acum"))) %>% 
  pivot_longer(
    cols = !c(fecha, aa, mm),
    names_to  = "cat",
    values_to = "peso"
  ) %>% 
  mutate(
    cat = case_when(
      str_detect(cat, "psup") ~ "Psup",
      str_detect(cat, "segu") ~ "Segunda",
      TRUE                    ~ "Premium"
    )
  ) %>% 
  my_plot(aes(x = mm, y = peso, fill = as_factor(aa))) +
  geom_col(position = "dodge", color = "#333333") +
  facet_wrap(~cat, ncol = 1) +
  labs(
    title = "Comparativa de la producción mensual (Kg)",
    subtitle = "Producción de cada una de las categorías",
    x = NULL, y = NULL, fill = "Anualidades"
  )


# acumulados de los kg por categorías
coop_grafs$kg_cat_acum <-
  coop_ds$mes_kg %>% 
  select(!c(ends_with("kg"), total_kg_acum)) %>% 
  pivot_longer(
    cols = !c(fecha, aa, mm),
    names_to = "cat",
    values_to = "peso"
  ) %>% 
  mutate(
    cat = case_when(
      str_detect(cat, "psup") ~ "Psup",
      str_detect(cat, "segu") ~ "Segunda",
      TRUE                    ~ "Premium"
    )
  ) %>% 
  my_plot(aes(x = mm, y = peso, color = as_factor(aa))) +
  geom_point() +
  geom_line(aes(group = aa)) +
  facet_wrap(~cat, ncol = 1) +
  labs(
    title = "Acumulados de la producción mensual (Kg)",
    subtitle = "Producción de cada una de las categorías",
    x = NULL, y = NULL, color = "Anualidades"
  ) 

# relación entre racimos y kg
# coop_ds$mes %>% 
#   select(fecha, kg_rac) %>% 
#   my_plot(aes(x = fecha, y = kg_rac)) +
#   geom_point() +
#   geom_smooth(se = FALSE, method = "loess") +
#   labs(
#     title = "Evolución del Kg por racimo",
#     x = NULL, y = NULL
#   )

# puedo hacer la relación entre racimos y kg en base semanal
coop_grafs$rac_kg_sem <-
  coop_ds$sem %>% 
  select(fecha, semana, racimos, peso_med) %>% 
  filter(year(fecha) > 2020) %>% 
  pivot_longer(
    !c(fecha, semana),
    names_to = "medida",
    values_to = "valor"
  ) %>% 
  mutate(
    aa = year(fecha),
    medida = case_when(
      medida == "racimos"  ~ "Racimos",
      medida == "peso_med" ~ "Kg (media)"
    )
  ) %>% 
  my_plot(aes(x = semana, y = valor, color = as_factor(aa))) +
  geom_point(alpha = .5) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept = 34, color = "grey75") +
  facet_wrap(~medida, ncol = 1, scales = "free_y") +
  labs(
    title    = "Comparativa de la media de Kg y número de racimos",
    subtitle = "Semanas de cada anualidad",
    caption  = "Marcada la semana 34",
    x = "Semanas", y = NULL, color = "Anualidades"
  )


# Importes ----------------------------------------------------------------

coop_grafs$eur_mm_acum <- 
  coop_ds$mes %>% 
  select(fecha, total_eur) %>% 
  group_by(anualidad = year(fecha)) %>% 
  mutate(
    eur_acum = cumsum(total_eur)
  ) %>% 
  ungroup() %>% 
  mutate(
    mes = month(fecha, label = TRUE)
  ) %>% 
  my_plot(aes(x = mes, y = eur_acum, color = as_factor(anualidad))) +
  geom_point(alpha = .5) +
  geom_line(aes(group = anualidad), linewidth = 1.2) +
  # último valor del último año, comparado con los anteriores
  geom_point(
    data = . %>% filter(month(fecha) == month(max(fecha))),
    aes(x = mes, y = eur_acum), color = "black", shape = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = . %>% filter(month(fecha) == month(max(fecha))),
    aes(
      label = format(eur_acum, big.mark=".", decimal.mark = ",", digits = 1) %>%
        str_c(.,"eur", sep=" ")
    ),
    nudge_x = .9, size = 3,
    show.legend = FALSE
  ) +
  labs(
    title = "Acumulados mensuales de los importes (€)",
    x = NULL, y = NULL, color = "Anualidades"
  ) +
  scale_y_continuous(labels = eur, position = "right")

# comparo los importes por categorías

coop_grafs$eur_cat <- 
  coop_ds$mes %>% 
  select(fecha, ends_with("eur"), -total_eur) %>% 
  pivot_longer(
    cols = -fecha,
    names_to = "cat",
    values_to = "eur"
  ) %>% 
  mutate(
    mes = month(fecha, label = TRUE),
    anualidad = as_factor(year(fecha)),
    cat = case_when(
      str_detect(cat, "prem") ~ "Premium",
      str_detect(cat, "psup") ~ "Psup",
      TRUE                    ~ "Segunda"
    )
  ) %>% 
  my_plot(aes(x = mes, y = eur, color = anualidad)) +
  geom_point(alpha = .5) +
  geom_line(aes(group = anualidad), linewidth = 1) +
  facet_wrap(~cat, ncol = 1, scales = "free_y") +
  labs(
    title = "Comparativa de la evolución de los importes por categoría",
    subtitle = "Valores en €",
    x = NULL, y = NULL, color = NULL
  )

# Guardo las gráficas -----------------------------------------------------

save(coop_grafs, file = here("data/processed", "graficas_finca.RData"))
