library(d3po)
library(dplyr)
library(sf)
library(rvest)
library(janitor)

# better resolution map of London boroughs compared to D3po built-in subnational map
# source: https://gis-tfl.opendata.arcgis.com/datasets/london-boroughs-1/about

url <- "https://hub.arcgis.com/api/v3/datasets/0a92a355a8094e0eb20a7a66cf4ca7cf_10/downloads/data?format=geojson&spatialRefId=4326&where=1%3D1"
finp <- "london_boroughs.geojson"

if (!file.exists(finp)) {
  download.file(url, destfile = finp, mode = "wb")
}

boroughs <- st_read(finp) %>%
  clean_names()

boroughs

names1 <- pull(boroughs, borough)

names1

# we need the population for the 33 boroughs
# https://www.citypopulation.de/en/uk/greaterlondon/ has a table
# => use rvest to scrape the table

url_pop <- "https://www.citypopulation.de/en/uk/greaterlondon/"
finp2 <- "london_population.rds"

if (file.exists(finp2)) {
  pop_table <- readRDS(finp2)
} else {
  page <- read_html(url_pop)

  tables <- page %>% html_nodes("table")

  pop_table <- tables[[1]] %>%
    html_table() %>%
    clean_names()

  pop_table <- pop_table %>%
    select(name, pop = population_estimate2024_06_30)

  pop_table <- pop_table %>%
    mutate(pop = as.numeric(gsub(",", "", pop)))

  names2 <- pull(pop_table, name)

  names2

  # names that do not match
  setdiff(names1, names2)
  setdiff(names2, names1)

  # replace the " and " with " & " in boroughs
  # replace "City of Westminster" with "Westminster"
  pop_table <- pop_table %>%
    mutate(borough = case_when(
      name == "City of Westminster" ~ "Westminster",
      grepl(" and ", name) ~ gsub(" and ", " & ", name),
      TRUE ~ name
    )) %>%
    select(-name)

  saveRDS(pop_table, finp2)
}

# direct map: inhabitants per borough
# not so direct: inhabitants per square km

boroughs <- boroughs %>%
  left_join(pop_table, by = "borough") %>%
  mutate(
    area_km2 = hectares / 100,
    pop_per_km2 = pop / area_km2
  )

my_gradient <- c("#b2d8d8", "#66b2b2", "#008080", "#006666", "#004c4c")

d3po(boroughs, width = 800, height = 600) %>%
  po_geomap(daes(group = borough, size = pop, color = my_gradient, gradient = T, tooltip = borough)) %>%
  po_labels(
    title = "Population in London Boroughs (2024)",
    subtitle = "Source: CityPopulation.DE & TFL London Boroughs"
  )

d3po(boroughs, width = 800, height = 600) %>%
  po_geomap(daes(group = borough, size = pop_per_km2, color = my_gradient, gradient = T, tooltip = borough)) %>%
  po_labels(
    title = "Population per Sq. Kilometer in London Boroughs (2024)",
    subtitle = "Source: CityPopulation.DE & TFL London Boroughs"
  )
