library(tidyverse)       # Pkg para ciencia de datos
library(leaflet)         # Pkg para crear mapas web
library(leaflet.extras)  # Plugins para leaflet
library(sf)              # Pkg para trabajar datos vectoriales
library(cptcity)         # Paleta de colores <3
library(htmltools)       # Integrar html mediante codigos de R
# 1.Lectura de datos ------------------------------------------------------
vacunas_geo <- read_csv(
  "https://www.datosabiertos.gob.pe/node/8298/download"
  ) %>%
  st_as_sf(
    coords = c("longitud","latitud"),
    crs = 4326
    ) %>%
  mutate(
    id = row_number()
    )

gpkg <- st_read(
  'https://github.com/ambarja/gpkg-pe/raw/main/Lima_distritos.gpkg'
  )


# 2.Preparación de datos --------------------------------------------------

lista_id <- st_contains(gpkg, vacunas_geo) %>% unlist()
vacunas_prov <- vacunas_geo %>%
  filter(id %in% lista_id)

vacunas_by_prov <- gpkg %>%
  select(NOMBPROV,NOMBDIST,CAPITAL) %>%
  mutate(
    total_cv = lapply(
      st_contains(gpkg,vacunas_geo),
      function(x){table(x) %>% sum}) %>% unlist()
    )

fill_color <- colorQuantile(
  palette = cpt(pal = 'cb_seq_Reds_05'),
  domain = vacunas_by_prov$total_cv,
  n = 5
  )

hex_color <- unique(fill_color(vacunas_by_prov$total_cv))
newlabels <- sprintf(
  '%s%s%s',
  round(lag(quantile(vacunas_by_prov$total_cv))),
  '-',
  round(quantile(vacunas_by_prov$total_cv))
  ) %>%
  str_replace(
    pattern = 'NA*',
    replacement = '0'
    )
logo <- tags$div(
  HTML(
    '<a href="#">
      <img border="0" src="https://user-images.githubusercontent.com/23284899/141409747-5f9da798-9cb0-46db-b750-f87dd1300a95.png" width="50" height="50">
    </a>')
)


# 3.Creacion de mapa web --------------------------------------------------
vacunas_by_prov %>%
  leaflet() %>%
  addProviderTiles(provider = providers$CartoDB.DarkMatter) %>%
  addPolygons(
    fillColor = ~fill_color(total_cv),
    fillOpacity = 0.9,
    weight = 0.5,
    color = 'white',
    dashArray = "3",
    label = sprintf(
      "%s%s%s",
      str_to_title(vacunas_by_prov$NOMBDIST),
      ":",
      vacunas_by_prov$total_cv),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    highlightOptions = highlightOptions(
      weight = 5,
      color = "white",
      bringToFront = TRUE),
    ) %>%
  addLegend(
    position = 'topright',
    colors = hex_color,
    labels = newlabels,
    opacity = 1,
    title = 'Centros de <br/> vacunación'
    ) %>%
  addControl(logo,position = 'bottomleft') %>%
  addMiniMap(width = 100,height = 100,tiles = providers$CartoDB.DarkMatter) %>%
  addResetMapButton() %>%
  addSearchOSM() %>%
  setView(lng = -76.90,lat = -12.00 ,zoom = 10)

# Leaflet con R : https://ambarja.github.io/blog/2021/10/17/02_rleaflet/