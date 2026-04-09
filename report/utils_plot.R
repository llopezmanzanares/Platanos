# report/utils_plot.R

## Función auxiliar para eje Y en euros
eje_y_eur <- function(position = "right") {
  scale_y_continuous(
    position = position,
    labels = label_currency(
      suffix = " €",
      prefix = "",
      big.mark = ".",
      decimal.mark = ",",
      scale_cut = cut_long_scale()
    )
  )
}

eje_x_eur <- scale_x_continuous(
  labels = label_currency(scale_cut = cut_long_scale())
)

# Paleta Principal
mis_colores <- c(
  "blanco" = "white", "cream" = "#f4f7be", "mindaro" = "#e5f77d", "ecru" = "#deba6f",
  "wine" = "#823038", "licoire" = "#1e000e"
)
