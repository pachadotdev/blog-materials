
if (!require("dplyr")) install.packages("dplyr")
if (!require("duckdb")) install.packages("duckdb")
if (!require("chilemapas")) install.packages("chilemapas")
if (!require("d3po")) install.packages("d3po")

library(dplyr)
library(duckdb)
library(chilemapas)
library(d3po)

con <- dbConnect(duckdb::duckdb(), dbdir = "~/Documents/censo2024-duckdb/censo2024_corregido.duckdb", read_only = FALSE)

dbListTables(con)

personas_por_comuna <- tbl(con, "personas") |>
    group_by(comuna) |>
    summarise(n_personas = n()) |>
    inner_join(
        tbl(con, "codigos_territoriales"),
        by = c("comuna" = "codigo_territorial")
    ) |>
    collect()

personas_por_comuna

viviendas_por_comuna <- tbl(con, "viviendas") |>
    group_by(comuna) |>
    summarise(n_viviendas = n()) |>
    inner_join(
        tbl(con, "codigos_territoriales"),
        by = c("comuna" = "codigo_territorial")
    ) |>
    collect()

viviendas_por_comuna

# Con la variable "p5_num_dormitorios" se puede seguir la metodología del Ministerio de Desarrollo Social,
# que consiste en tomar la razón entre el número de personas residentes en la vivienda y el número de dormitorios de la
# misma y luego se tramifica la variable en las siguientes categorías:
# Sin hacinamiento [0;2,5)
# Medio [2,5;3,5)
# Alto [3,5;4,9)
# Crítico [5,+∞)

hacinamiento_por_comuna <- tbl(con, "viviendas") |>
  select(
    id_vivienda,
    comuna,
    n_dormitorios = p5_num_dormitorios,
    n_personas = cant_per,
  ) |>
  filter(n_dormitorios > 0) |>
  mutate(
    razon_hacinamiento = n_personas / n_dormitorios,
    categoria_hacinamiento = case_when(
      razon_hacinamiento < 2.5 ~ "Sin hacinamiento",
      razon_hacinamiento >= 2.5 & razon_hacinamiento < 3.5 ~ "Medio",
      razon_hacinamiento >= 3.5 & razon_hacinamiento < 4.9 ~ "Alto",
      razon_hacinamiento >= 5 ~ "Crítico"
    )
  ) |>
  group_by(comuna, categoria_hacinamiento) |>
  summarise(n_viviendas = n()) |>
  inner_join(
    tbl(con, "codigos_territoriales"),
    by = c("comuna" = "codigo_territorial")
  ) |>
  collect()

porcentaje_con_hacinamiento <- hacinamiento_por_comuna |>
  group_by(comuna, territorio) |>
  mutate(p_viviendas = n_viviendas / sum(n_viviendas) * 100) |>
  arrange(comuna, categoria_hacinamiento)

porcentaje_con_hacinamiento

saveRDS(personas_por_comuna, file = "personas_por_comuna.rds")
# saveRDS(viviendas_por_comuna, file = "viviendas_por_comuna.rds")
saveRDS(porcentaje_con_hacinamiento, file = "hacinamiento_por_comuna.rds")

dbDisconnect(con, shutdown = TRUE)
