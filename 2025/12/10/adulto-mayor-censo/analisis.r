if (!require("dplyr")) install.packages("dplyr")
if (!require("duckdb")) install.packages("duckdb")
if (!require("chilemapas")) install.packages("chilemapas")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("patchwork")) install.packages("patchwork")

library(dplyr)
library(duckdb)
library(chilemapas)
library(ggplot2)
library(patchwork)

con <- dbConnect(duckdb::duckdb(), dbdir = "~/Documents/censo2024-duckdb/censo2024_corregido.duckdb", read_only = FALSE)

dbListTables(con)

tbl(con, "codigos_personas") |>
    filter(nombre_variable == "sexo")

edad_por_region <- tbl(con, "personas") |>
    mutate(
        edad = case_when(
            edad >= 0 & edad <= 4  ~ "0 a 4",
            edad >= 5 & edad <= 9  ~ "5 a 9",
            edad >= 10 & edad <= 14 ~ "10 a 14",
            edad >= 15 & edad <= 19 ~ "15 a 19",
            edad >= 20 & edad <= 24 ~ "20 a 24",
            edad >= 25 & edad <= 29 ~ "25 a 29",
            edad >= 30 & edad <= 34 ~ "30 a 34",
            edad >= 35 & edad <= 39 ~ "35 a 39",
            edad >= 40 & edad <= 44 ~ "40 a 44",
            edad >= 45 & edad <= 49 ~ "45 a 49",
            edad >= 50 & edad <= 54 ~ "50 a 54",
            edad >= 55 & edad <= 59 ~ "55 a 59",
            edad >= 60 & edad <= 64 ~ "60 a 64",
            edad >= 65 & edad <= 69 ~ "65 a 69",
            edad >= 70 & edad <= 74 ~ "70 a 74",
            edad >= 75 & edad <= 79 ~ "75 a 79",
            edad >= 80 & edad <= 84 ~ "80 a 84",
            edad >= 85 & edad <= 89 ~ "85 a 89",
            edad >= 90 & edad <= 94 ~ "90 a 94",
            edad >= 95 & edad <= 99 ~ "95 a 99",
            edad >= 100              ~ "100 o mas",
            TRUE                     ~ NA_character_
        )
    ) |>
    filter(!is.na(edad)) |>
    mutate(
        sexo = case_when(
            sexo == 1 ~ "hombre",
            sexo == 2 ~ "mujer",
            TRUE       ~ NA_character_
        )
    ) |>
    mutate(region = substr(comuna, 1, 2)) |>
    group_by(region, edad, sexo) |>
    summarise(poblacion2024 = n()) |>
    inner_join(
        tbl(con, "codigos_territoriales"),
        by = c("region" = "codigo_territorial")
    ) |>
    collect()

# comparo con censo 2017

edad_por_region <- edad_por_region |>
  filter(!is.na(edad)) |>
  mutate(
    edad = factor(
      edad,
      levels = c(
        "0 a 4", "5 a 9", "10 a 14", "15 a 19", "20 a 24",
        "25 a 29", "30 a 34", "35 a 39", "40 a 44", "45 a 49",
        "50 a 54", "55 a 59", "60 a 64", "65 a 69", "70 a 74",
        "75 a 79", "80 a 84", "85 a 89", "90 a 94", "95 a 99",
        "100 o mas"
      )
    ),
    sexo = factor(
      sexo,
      levels = c("hombre", "mujer")
    )
  ) |>
  inner_join(
    censo_2017_comunas |>
        mutate(region = substr(codigo_comuna, 1, 2)) |>
        rename(poblacion2017 = poblacion) |>
        group_by(region, edad, sexo) |>
        summarise(poblacion2017 = sum(poblacion2017)),
    by = c("region", "edad", "sexo")
  ) |>
  select(region, nombre_region = territorio, region, edad, sexo, poblacion2017, poblacion2024)

# cambio en N de poblacion adulto mayor

edad_por_region <- edad_por_region |>
    mutate(
        edad2 = case_when(
            edad %in% c(
                "65 a 69", "70 a 74", "75 a 79", "80 a 84",
                "85 a 89", "90 a 94", "95 a 99", "100 o mas"
            ) ~ "adulto mayor",
            TRUE ~ "no adulto mayor"
        )
    ) |>
    group_by(region, nombre_region, edad2, sexo) |>
    summarise(
        poblacion2017 = sum(poblacion2017),
        poblacion2024 = sum(poblacion2024)
    ) |>
    mutate(
        n_cambio = poblacion2024 - poblacion2017,
        p_cambio = (n_cambio / poblacion2017) * 100
    )

# regiones con mayor aumento de adultos mayores hombre/mujer

edad_por_region |>
    filter(edad2 == "adulto mayor") |>
    arrange(desc(p_cambio)) |>
    head(10)

edad_por_region <- edad_por_region |>
    select(region, nombre_region, edad2, sexo, n_cambio, p_cambio)

edad_por_region <- generar_regiones() |>
    select(codigo_region, geometry) |>
    left_join(
        edad_por_region,
        by = c("codigo_region" = "region")
    )

edad_por_region |>
    filter(is.na(p_cambio))

g <- function(s = "hombre", v = "n_cambio") {
    g2 <- ggplot() +
        geom_sf(
            data = filter(edad_por_region, sexo == s),
            aes(fill = !!sym(v), geometry = geometry),
            color = "black",
            size = 0.1
        ) +
        labs(
            title = sprintf("Adulto mayor  - %s - %s",
                stringr::str_to_title(s),
                ifelse(v == "n_cambio", "Cambio absoluto", "Cambio porcentual")
                ),
        ) +
        theme_minimal() + 
        coord_sf(xlim = c(-75, -66))

    if (v == "n_cambio") {
        my_pal <- tintin::tintin_clrs(option = "the_blue_lotus")[2:1]

        g2 + scale_fill_gradient2(my_pal[1], low = "white", high = my_pal[2],
        na.value = "lightgrey")
    } else {
        my_pal <- tintin::tintin_clrs(option = "the_seven_crystal_balls")[2:1]

        g2 + scale_fill_gradient2(low = my_pal[1], mid = "white", high = my_pal[2],
        na.value = "lightgrey")
    }
}

gnh <- g("hombre", "n_cambio")
gnm <- g("mujer", "n_cambio")
gph <- g("hombre", "p_cambio")
gpm <- g("mujer", "p_cambio")

dout <- list(
  gnh = gnh,
  gnm = gnm,
  gph = gph,
  gpm = gpm
)

saveRDS(dout, file = "adulto_mayor_censo.rds", compress = "xz")

gnh + gnm

gph + gpm
