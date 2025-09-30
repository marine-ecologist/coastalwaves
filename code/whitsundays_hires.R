
library(oceanwaves)
library(dhw)
library(sf)
library(tidyverse)
library(lubridate)
library(ggplot2)


### download GBR shape file
if (!exists("gbr_shape")) {
  gbr_shape <- download_gbr_spatial(return = "base") |>
    dplyr::select(LOC_NAME_S, X_COORD, Y_COORD) |>
    sf::st_make_valid() |>
    sf::st_transform(20353)
}


### Grep subset for target reefs
whitsundays <- gbr_shape |>
  filter(grepl("Hook|Hayman|reef", LOC_NAME_S)) |>
  filter(grepl("20", LOC_NAME_S))


whitsundays_union <- whitsundays |>
  st_union() |>
  st_boundary() |>
  st_cast("LINESTRING")


### extract broader region for fetch (50km from focus reefs)
whitsundays_region <- st_crop(gbr_shape,
                              whitsundays |>
                                st_union() |>
                                st_centroid() |>
                                st_buffer(20000))

# create inset for mapping
pinnacle <- st_bbox(c(xmin=148.962, xmax=148.968, ymin=-20.072, ymax=-20.06), crs=4326) |> st_transform(20353)


# load data
df_whitsundays <- read.csv("/Users/rof011/coastalwaves/data/df_whitsundays.csv") |>
  rename(windspeed_ms = wspeed,
         date = time)



whitsundays_points <- calculate_points(whitsundays_union, 20)

whitsundays_fetch <- calculate_fetch(point = whitsundays_points,
                                     degrees = 7.5,
                                     fetch = 20000,
                                     land = whitsundays_region,
                                     parallel = TRUE,
                                     cores = 8) |> drop_na()

whitsundays_waves <- calculate_wave_exposure(wind = df_whitsundays[1:365,],
                                             fetch = whitsundays_fetch,
                                             points = whitsundays_points,
                                             parallel = TRUE,
                                             cores = 8)


wave_grid <- create_wave_grid(whitsundays_union, whitsundays_waves, interval = 20, width = 20)

tmap_mode("view") +
  tm_shape( whitsundays |> mutate(habitat = if_else(!grepl("Reef", LOC_NAME_S), "island", "reef")), bbox=pinnacle) +
  tm_polygons() +
  tm_shape(wave_grid) +
  tm_polygons("wave_energy_total")

tmap_mode("view") +
  tm_shape( whitsundays |> mutate(habitat = if_else(!grepl("Reef", LOC_NAME_S), "island", "reef")), bbox=pinnacle) +
  tm_polygons() +
  tm_shape(whitsundays_waves) +
  tm_dots("wave_energy_total", size=1.5)
