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
#   - data/processed/balten_raw.rds

# SALIDAS:
#   - Objetos: finca_eda
#   - Archivo RDS con los datos data/processed/finca_eda.rds
#   - Mensajes de progreso y validación en consola

# DEPENDENCIAS:
#   - tidyverse, here, glue

# ******************************************************************************

message("\n════════════════════════════════════════════════════════════════")
message("  INICIO: Análisis Exploratorio de Datos")
message("════════════════════════════════════════════════════════════════\n")



# 1. LIBRERIAS ------------------------------------------------------------

# 2. CONSTANTES -----------------------------------------------------------

# Solo tengo un dato de 2020, quito esta anualidad
QUITO_2020 <- "2020"

# 3. VARIABLES ------------------------------------------------------------

message("\n[1/] Cargando datos...")

## 3.1. Vars Globales -----------------------------------------------------
cargar_rds_si_no_existe(
  "coop_raw", "data/processed/coop_raw.rds",
  solucion = "Verifica integridad del archivo o ejecuta 01_ingest_coop.R"
)
cargar_rds_si_no_existe(
  "gastos_otrosingresos", "data/processed/gastos_otros-ingresos.rds",
  solucion = "Verifica integridad del archivo o ejecuta 03_ingest_otros.R"
)
cargar_rds_si_no_existe(
  "balten_ds", "data/processed/balten_raw.rds",
  solucion = "Verifica integridad del archivo o ejecuta 02_ingest_balten.R"
)

# 4. EJECUCION ------------------------------------------------------------

finca_eda <- list()

# 4.1 Calcular agregados mensuales ----------------------------------------

message("\n[2/] Calculando datos mensuales...")

# --- Liquidaciones
finca_eda$liq_mes <-
  filter(coop_raw, fecha_aa != QUITO_2020) |>
  select(-fecha_sem) |>
  pivot_wider(names_from = "tipo", values_from = "valor") |>
  mutate(fecha = rollforward(fecha)) |>
  summarise(
    .by = starts_with("fecha"),
    across(!ends_with("eurkg"), sum),
    across(ends_with("eurkg"), mean),
    kg_rac = total_bruto_kg / total_racimos
  )

# --- Otros gastos
finca_eda$oogg_mes <-
  gastos_otrosingresos$gastos |>
  mutate(fecha = as.Date(rollforward(fecha))) |>
  summarise(.by = fecha, neto = sum(total))

# Nota: Actualmente los Otros Ingresos son esporádicos. No tiene sentido
#       agregar mensualmente

# 4.2 Calcular acumulados anuales -----------------------------------------

message("\n[3/] Calculando acumulados anuales...")

# --- BALTEN
finca_eda$balten_aa_acum <-
  balten_ds$bimensuales |>
  filter(aa_act > QUITO_2020) |>
  mutate(
    .by = aa_act,
    across(where(is.numeric), cumsum, .names = "{.col}_acum")
  )

# --- Liquidaciones
finca_eda$liq_aa_acum <-
  finca_eda$liq_mes |>
  mutate(
    .by = fecha_aa,
    across(matches("(_kg|_eur)$"), cumsum, .names = "{.col}_acum")
    # across(ends_with(c("_kg","_eur")), cumsum, .names = "{.col}_acum")
  ) |>
  select(starts_with("fecha"), ends_with("acum"))

# --- Otros gastos
finca_eda$oogg_aa_acum <-
  finca_eda$oogg_mes |>
  mutate(aa = year(fecha)) |>
  mutate(.by = aa, neto_acum = cumsum(neto))

# Nota: Actualmente los Otros Ingresos son esporádicos. No tiene sentido
#       acumular mensualmente

# 4.3 Calcular totales anuales --------------------------------------------

message("\n[3/] Calculando totales anuales...")

# --- BALTEN
finca_eda$balten_total_aa <-
  finca_eda$balten_aa_acum |>
  filter(.by = aa_act, fecha_act == max(fecha_act)) |>
  select(fecha_act, consumo_m3_acum, total_eur_acum)

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

# --- Otros Gastos
finca_eda$oogg_total_aa <-
  finca_eda$oogg_aa_acum |>
  filter(.by = aa, fecha == max(fecha))


# 5. GUARDAR --------------------------------------------------------------

message("\nGuardando cálculos...\n")

guardar_con_backup(finca_eda, "data/processed/finca_eda.rds")

message("✓ Script 21_eda_finca.R finalizado correctamente.\n")
