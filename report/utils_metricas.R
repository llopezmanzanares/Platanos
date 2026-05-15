# report/utils_metricas.R

# --- Ingresos y Gastos anuales -------------------------------------------

ing_gas <-
  left_join(
    finca_eda$liq_total_aa |>
      select(fecha, liquidaciones = total_neto_eur_acum, kg = total_bruto_kg_acum) |> 
      mutate(aa = year(fecha)),
    finca_eda$ooii_total_aa |>
      select(aa, otros_ingresos),
    by = "aa"
  ) |>
  left_join(
    finca_eda$balten_total_aa |>
      mutate(
        aa = year(fecha_act),
        agua = total_eur_acum / 2, .keep = "none"
      ),
    by = "aa"
  ) |>
  left_join(
    finca_eda$oogg_total_aa |>
      select(aa, otros_gastos = neto_acum),
    by = "aa"
  ) |>
  mutate(
    ingresos = liquidaciones + replace_na(otros_ingresos, 0),
    gastos = replace_na(agua, 0) + replace_na(otros_gastos, 0),
    beneficios = ingresos - gastos,
    d_benef = beneficios / ingresos,
    eur_kg = gastos / kg
  ) |>
  select(aa, ingresos, gastos, beneficios, d_benef, eur_kg) |> 
  arrange(desc(aa))
