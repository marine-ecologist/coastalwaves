
library(oceanwaves)
library(dhw)
library(sf)
library(tidyverse)
library(lubridate)
library(ggplot2)


### download GBR shape file
gbr_shape <- download_gbr_spatial(return = "base") |>
  select(LOC_NAME_S, X_COORD, Y_COORD) |>
  st_make_valid() |>
  st_transform(20353)


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



spawning_dates <- seq(ymd("2018-11-01"), ymd("2024-12-31"), by = "day") %>%
  keep(~ month(.) %in% c(11, 12)) %>%
  format("%Y%m%d")


whitsundays_weather <- get_wind_data(
  sf_obj = st_point(c(148, -20)) |> st_sfc(crs = 4326) |> st_sf(),
  start_date = "2024-11-01",
  end_date = "2024-12-31"
) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))



whitsundays_points <- calculate_points(whitsundays_union, 50)


whitsundays_fetch <- calculate_fetch(point = whitsundays_points,
                                     degrees = 7.5,
                                     fetch = 5000,
                                     land = whitsundays_region,
                                     parallel = TRUE,
                                     cores = 8)


whitsundays_waves <- calculate_waves(wind = whitsundays_weather,
                                     fetch = whitsundays_fetch,
                                     points = whitsundays_points,
                                     parallel = TRUE,
                                     cores = 8)


library(tmap)
tmap_mode("view") +
tm_shape( whitsundays |> mutate(habitat = if_else(!grepl("Reef", LOC_NAME_S), "island", "reef"))) +
  tm_polygons() +
tm_shape(whitsundays_waves) +
    tm_dots("wave_energy",
            size=1.2,
            fill.scale=tm_scale_continuous(values="brewer.spectral"))


ggplot() + theme_bw() +
  geom_sf(
    data = whitsundays |>mutate(habitat = if_else(!grepl("Reef", LOC_NAME_S), "island", "reef")),
    aes(fill = habitat),
    color = "black",
    linewidth=0.1
  ) +
  scale_fill_manual(values = c("island" = "#ffffc0", "reef" = "#e6f8ff")) +
  ggnewscale::new_scale_fill() +
  geom_sf(data = whitsundays_waves, aes(fill = wave_energy), shape = 21, size = 3) +
  scale_fill_distiller(palette = "RdYlBu", guide = guide_colorbar(title = "Wave Energy"))
