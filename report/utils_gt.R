# report/utils_gt.R

library(gt)

# Redefinir gt para usar locale="es" automáticamente
gt <- function(...) {
  gt::gt(..., locale = "es")
}


gt_tema <- function(gt_tbl, ancho = 50) {
  gt_tbl |>
    tab_options(
      table.width = pct(ancho),
      table.align = "left",
      table.border.top.color = "white",
      table.border.bottom.color = "white",
      table.background.color = "#f8f9fa",
      # table.font.names = "Inter",
      table.font.size = px(13),
      # Encabezado
      heading.align = "left",
      heading.background.color = "#f0faf7",
      heading.border.bottom.color = "#62bba5",
      heading.border.bottom.width = px(3),
      heading.title.font.size = px(16),
      heading.title.font.weight = "bold",
      # Etiquetas de columnas
      column_labels.background.color = "#f0faf7",
      column_labels.border.top.color = "#62bba5",
      column_labels.border.bottom.color = "gray80",
      column_labels.font.weight = "bold",
      # Filas
      data_row.padding = px(7),
      row_group.font.weight = "bold",
      row_group.border.top.color = "#62bba5",
      row.striping.include_table_body = TRUE,
      row.striping.background_color = "#f7fdfb",
      # Pie de tabla
      source_notes.font.size = px(11),
      source_notes.border.bottom.color = "white"
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = cell_text(color = "#5d6d7e"),
      locations = cells_body()
    ) |>
    opt_row_striping()
}

#  valores negativos se coloreen automáticamente en rojo
gt_fmt_pyg <- function(gt_tbl, columns = where(is.numeric)) {
  gt_tbl |>
    tab_style(
      style = cell_text(
        color = "#e74c3c",
        # weight = "bold"
      ),
      locations = cells_body(columns = {{ columns }}, rows = {{ columns }} < 0)
    )
}
