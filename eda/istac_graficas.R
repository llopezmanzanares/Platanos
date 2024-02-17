# Generación de las gráficas a partir de los datos de ISTAC

# Versión: 2024-02-16

# TODO: modificar gráficas de líneas. Incluir nombre isla en la gráfica

library(tidyverse)

# Preparacion -------------------------------------------------------------

eur <- scales::label_currency(suffix = "€", prefix = NULL,
                              big.mark = ".", decimal.mark = ",")

col_pal <- c("cream" = "#f4f7be", "mindaro" = "#e5f77d", "ecru" = "#deba6f",
             "wine"  = "#823038", "licoire" = "#1e000e")


istac_grafs <- list()

# función para modificar la posición de la leyenda y el eje y en las gráficas
my_plot <- function(...){
  ggplot(...) + 
    theme(
      plot.title.position = "plot",
      legend.position = "bottom"
    ) +
    scale_y_continuous(position = "right")
}

# Evolución de la producción ----------------------------------------------

istac_grafs$tn_canarias <- 
  istac_ds$toneladas|> 
  filter(territorio == "Canarias") |>  
  my_plot(aes(x = aa, y = tn/1000)) +
  geom_segment(aes(x = aa, xend = aa, y = 0, yend = tn/1000), linewidth = 1) +
  geom_point(shape = 21, fill = col_pal["wine"], color = col_pal["mindaro"], size = 10) +
  labs(
    title = "Producción total anual (miles Tn)",
    caption = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL
  )

txt_tn_pos <-
  istac_ds$toneladas|> 
  filter(
    territorio %in% c("Tenerife", "Gran Canaria", "La Palma"),
    aa == min(aa)
    )

istac_grafs$tn <- 
  istac_ds$toneladas|> 
  filter(territorio %in% c("Tenerife", "Gran Canaria", "La Palma"))|> 
  my_plot(aes(x = aa, y = tn/1000, color = territorio)) +
  geom_line(aes(group = territorio)) +
  geom_text(
    data = txt_tn_pos,
    aes(label = territorio, x = aa, y = tn/1000),
    vjust = 0, hjust = 0
  ) +
  labs(
    title    = "Producción de plátanos (mil Tn)",
    subtitle = "Principales islas productoras",
    caption  = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL, color = NULL
  ) +
  theme(legend.position = "none")

 exps_eda <- 
  istac_ds$exportaciones |> 
  filter(
    isla %in% c("Gran Canaria", "Tenerife", "La Palma"),
    aa >= 2018
    )|> 
  select(mes, isla, total:extranjero) |> 
  pivot_longer(cols = total:extranjero, names_to = "tipo", values_to = "tn") |> 
  mutate(mm = month(mes, label = TRUE), aa = year(mes)) |> 
  mutate(
    mx_tn = max(tn), mn_tn = min(tn), md_tn = median(tn),
    .by = c(isla, tipo, mm)
  )
 
 exps_last_aa <- filter(exps_eda, aa == max(aa), tipo == "total")
 
istac_grafs$exp_tot <-  
  exps_eda |> 
  filter(tipo == "total") |>
  ggplot(aes(x = mm, group = aa)) +
  geom_ribbon(aes(ymin = mn_tn, ymax = mx_tn), fill = col_pal["mindaro"]) +
  geom_line(aes(x = mm, y = md_tn), color = col_pal["ecru"], linewidth = 1) +
  geom_line(data = exps_last_aa, aes(y = tn), color = col_pal["wine"]) +
  facet_wrap(~isla, ncol = 1, scales = "free_y") +
  labs(
    title    = "Evolución mensual de las exportaciones (mil Tn)",
    subtitle = "Valores máximos y mínimos, media y último año",
    caption  = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL, color = NULL
  )

col_pe <- c("#e69f00", "#009e73")
name(col_pe) <- c("Producción", "Exportación")

# toneladas producidas vs exportaciones
istac_grafs$prodvsexps <- 
  istac_ds$prodvsexps |>  
  select(aa:exports) |>  
  pivot_longer( cols = -aa, names_to = "medida", values_to = "tn") |>  
  mutate(medida = if_else(medida == "produccion", "Producción", "Exportación"))|> 
  my_plot(aes(x = as_factor(aa), y = tn, color = medida)) +
  geom_point(alpha = .4) +
  geom_smooth(aes(group = medida), se = FALSE) +
  labs(
    title    = "Comparativa de la Producción y Exportación anual",
    subtitle = "Toneladas anuales",
    caption  = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL, color = NULL
  ) +
  scale_color_manual(values = col_pe)

# producción dedicada a consumo interno
istac_grafs$cons_propio <- 
  istac_ds$prodvsexps |>  
  my_plot(aes(x = aa, y = interno)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, color = "#1e000e") +
  labs(
    title = "Evolución anual del consumo interno de la producción (Tn)",
    caption = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL
  ) +
  scale_x_continuous(breaks = istac_ds$prodvsexps$aa)

# Gráficas de precios -----------------------------------------------------

#precios última anualidad
p_sem_ult_aa <- filter(istac_ds$precios_sem, aa == max(aa))

istac_grafs$pre_sem <- 
  istac_ds$precios_sem |> 
  mutate(
    mx_precio = max(precio, na.rm = TRUE),
    mn_precio = min(precio, na.rm = TRUE),
    md_precio = median(precio, na.rm = TRUE),
    .by = c(territorio, sem)
  ) |> 
  ggplot(aes(x = sem)) +
  geom_ribbon(aes(ymin = mn_precio, ymax = mx_precio), fill = "#e5f77d", color = "#DEBA6F") +
  geom_line(aes(y = md_precio), color = "#DEBA6F", linewidth = 0.9) +
  geom_line(data = p_sem_ult_aa, aes(y = precio), color = "#823038", linewidth = 0.9) +
  geom_vline(xintercept = 34) +
  facet_wrap(~territorio, ncol = 1) +
  labs(
    title    = "Evolución del rango del precio percibido (€/Kg)",
    subtitle = "Valores máximos, mínimos, medios, última anualidad y semana 34.",
    caption  = "Fuente: ISTAC, Gobierno de Canarias",
    x = "Semana", y = NULL
  ) +
  theme(
    plot.title.position = "plot"
  )


# Gráficas de superficie --------------------------------------------------

txt_position <-
  istac_ds$superficie |>  
  filter(
    territorio %in% c("Gran Canaria", "Tenerife", "La Palma"),
    aa == min(aa)
  )

istac_grafs$sup <- 
  istac_ds$superficie |>  
  filter(
    territorio %in% c("Gran Canaria", "Tenerife", "La Palma")
  ) |> 
  my_plot(aes(x = as_factor(aa), y = ha, color = territorio)) +
  geom_line(aes(group = territorio)) +
  geom_text(
    data = txt_position, 
    aes(label = territorio, x = as.character(aa), y = ha),
    vjust = 0, hjust = 0
    ) +
  labs(
    title    = "Evolución de la superficie cultivada (ha)",
    subtitle = "Islas con mayor superficie",
    caption  = "Fuente: ISTAC, Gobierno de Canarias",
    x = NULL, y = NULL, color = NULL
  ) +
  theme(legend.position = "none")

# Guardo las gráficas -----------------------------------------------------

save(istac_grafs, file = here("data/processed", "istac_graficas.RData"))  
