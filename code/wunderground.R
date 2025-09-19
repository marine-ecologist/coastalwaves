library(tidyverse)
library(jsonlite)
library(lubridate)
theme_set(theme_bw())

# json2df <- function(id, date, key) {
#   u <- paste0("https://api.weather.com/v2/pws/history/all?stationId=", id, "&format=json&units=m&date=", date, "&apiKey=", key)
#   u %>%
#     fromJSON() %>%
#     .$observations %>%
#     as_tibble()
# }

json2df <- function(id, date, key) {
  u <- paste0("https://api.weather.com/v2/pws/history/all?stationId=", id, "&format=json&units=m&date=", date, "&apiKey=", key)
  raw <- fromJSON(u)

  obs <- as_tibble(raw$observations)

  if ("metric" %in% names(obs)) {
    obs <- bind_cols(obs %>% select(-metric), obs$metric)
  }

  return(obs)
}


apikey <- "5cada843ef8e4f39ada843ef8e4f392d"

stations <- c("ISHUTE4")

dates <- seq(ymd("2018-11-01"), ymd("2024-12-31"), by = "day") %>%
  keep(~ month(.) %in% c(11, 12)) %>%
  format("%Y%m%d")

shutedata <- expand_grid(station = stations, date = dates) %>%
  mutate(data = map2(station, date, ~json2df(.x, .y, apikey))) %>%
  unnest(data) |>
  mutate(
    windspeedAvg_knots = windspeedAvg * 1.94384,
    windspeedAvg_kmh   = windspeedAvg * 3.6
  ) |>  select(obsTimeLocal, winddirAvg, windspeedAvg_knots, windspeedAvg_kmh) |>
  rename(time = obsTimeLocal, bearing = winddirAvg, speed = windspeedAvg_kmh)

#saveRDS(shutedata, "/Users/rof011/oceanwaves/data/shutedata.csv")

shutedata <- readRDS("/Users/rof011/oceanwaves/data/shutedata.csv")


shutedata %>%
  mutate(dt = ymd_hms(obsTimeLocal)) %>%
  mutate(year=year(dt)) |>
  ggplot(aes(dt, winddirAvg)) +
  geom_line() + facet_wrap(~year, scales="free", ncol=1)


shutedata %>%
  mutate(dt = ymd_hms(obsTimeLocal)) %>%
  mutate(year=year(dt)) |>
  ggplot(aes(dt, windspeedAvg_knots)) +
  geom_line() + facet_wrap(~year, scales="free", ncol=1)
