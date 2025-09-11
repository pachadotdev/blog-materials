library(readxl)
library(janitor)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

url <- "https://interactivos.museodelamemoria.cl/victims/?page_id=7670&exportar_entradas_dd=true"
file <- "2025/09/11/desaparecidos.xlsx"

if (!file.exists(file)) {
  download.file(url, destfile = file, mode = "wb")
}

desaparecidos <- read_excel(file) %>%
    clean_names()

arrest_year <- desaparecidos %>%
    select(fecha_detencion_muerte) %>%
    mutate(year = as.integer(str_sub(fecha_detencion_muerte, -4, -1)))

arrest_year %>%
    filter(is.na(year))

arrest_year <- arrest_year %>%
    group_by(year) %>%
    count()

desaparecidos %>%
    distinct(edad)

arrest_age <- desaparecidos %>%
    select(edad) %>%
    mutate(
        age = case_when(
            str_detect(edad, "meses") ~ "1 - Less than 1",
            edad <= 18 ~ "2 - 18 of less",
            edad > 18 & edad <= 30 ~ "3 - 19 - 30",
            edad > 30 & edad <= 50 ~ "4 - 31 - 50",
            edad > 50 & edad <= 70 ~ "5 - 51 - 70",
            edad > 70 ~ "6 - 71 or more",
            TRUE ~ "7 - Unknown"
        )
    ) %>%
    group_by(age) %>%
    count()

ggplot(arrest_year, aes(x = year, y = n)) +
    geom_col(fill = "#c95555") +
    labs(
        title = "Year of Arrest and Disappearance",
        subtitle = "Source: Museo de la Memoria y los Derechos Humanos",
        x = "Year",
        y = "Number of Arrests"
    ) +
    theme_minimal(base_size = 13)

ggsave("2025/09/11/desaparecidos_year.png", width = 8, height = 5)

ggplot(arrest_age, aes(x = age, y = n)) +
    geom_col(fill = "#c95555") +
    labs(
        title = "Age of Arrest and Disappearance",
        subtitle = "Source: Museo de la Memoria y los Derechos Humanos",
        x = "Age Group",
        y = "Number of Arrests"
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("2025/09/11/desaparecidos_age.png", width = 8, height = 5)
