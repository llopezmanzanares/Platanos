# ******************************************************************************
# SCRIPT: 21_eda_finca.R
# PROYECTO: Control y análisis financiero de la finca de plátanos
# FECHA DE CREACIÓN:   04/04/2026
# ÚLTIMA MODIFICACIÓN: 04/04/2026
# DESCRIPCIÓN:
#   Agregaciones de datos de la Cooperativa:
#
#   El script se ejecuta desde 00_run_all.R
#
# ENTRADAS:
#   - data/processed/coop_raw.rds
#   - data/processed/gastos_otros-ingresos.rds

# SALIDAS:
#   - Objetos: finca_eda
#   - Archivo RDS con los datos data/processed/finca_eda.rds
#   - Mensajes de progreso y validación en consola

# DEPENDENCIAS:
#   - tidyverse, here, glue

# ******************************************************************************

# 1. LIBRERIAS ------------------------------------------------------------

# 2. CONSTANTES -----------------------------------------------------------

# Solo tengo un dato de 2020, quito esta anualidad
QUITO_2020 <- 2020

# 3. VARIABLES ------------------------------------------------------------

## 3.1. Vars Globales -----------------------------------------------------
cargar_rds_si_no_existe(
  "coop_raw", "data/processed/coop_raw.rds",
  solucion = "Verifica integridad del archivo o ejecuta 01_ingest_coop.R"
)
cargar_rds_si_no_existe(
  "gastos_otrosingresos", "data/processed/gastos_otros-ingresos.rds",
  solucion = "Verifica integridad del archivo o ejecuta 03_ingest_otros.R"
)

# 4. EJECUCION ------------------------------------------------------------

finca_eda <- list()

# 4.1 Calcular agregados mensuales ----------------------------------------

# --- Liquidaciones
finca_eda$liq_mes <-
  filter(coop_raw, year(fecha) > QUITO_2020) |>
  select(-fecha_sem) |>
  pivot_wider(names_from = "tipo", values_from = "valor") |>
  mutate(fecha = rollforward(fecha)) |>
  summarise(
    .by = starts_with("fecha"),
    across(!ends_with("eurkg"), sum),
    across(ends_with("eurkg"), mean),
    kg_rac = total_kg / racimos
  )

# --- Otros gastos
finca_eda$oogg_mes <-
  gastos_otrosingresos$gastos |>
  mutate(fecha = as.Date(rollforward(fecha))) |>
  summarise(.by = fecha, neto = sum(total))

# 4.2 Calcular acumulados anuales -----------------------------------------

# --- Liquidaciones
finca_eda$liq_aa_acum <-
  finca_eda$liq_mes |>
  mutate(
    .by = fecha_aa,
    across(matches("(_kg|_eur)$"), cumsum, .names = "{.col}_acum")
    # across(ends_with(c("_kg","_eur")), cumsum, .names = "{.col}_acum")
  ) |>
  select(starts_with("fecha"), ends_with("acum"))

# --- Otros

# 4.3 Calcular totales anuales --------------------------------------------

# --- Liquidaciones
finca_eda$liq_total_aa <-
  finca_eda$liq_aa_acum |>
  filter(.by = fecha_aa, fecha == max(fecha))

# --- Otros Ingresos
finca_eda$ooii_total_aa <-
  gastos_otrosingresos$ingresos |>
  mutate(
    fecha = ymd(fecha),
    mm    = month(fecha, label = TRUE),
    aa    = year(fecha)
  ) |>
  summarise(.by = c(aa, organismo), otros_ingresos = sum(neto))

# 5. GUARDAR --------------------------------------------------------------
