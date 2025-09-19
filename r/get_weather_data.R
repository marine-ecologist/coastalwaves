#' Download and process Weather Underground PWS wind data
#'
#' @param stations Character vector of station IDs (e.g. `"ISHUTE4"`)
#' @param startdate Start date (string in `"YYYY-MM-DD"` format)
#' @param enddate End date (string in `"YYYY-MM-DD"` format)
#' @param daterange Optional precomputed vector of dates in `"YYYYMMDD"` format
#' @param apikey Your Weather Underground API key
#'
#' @return A tibble with columns: `time`, `bearing`, `speed` (km/h), and `windspeedAvg_knots`
#' @export
#'
get_weather_data <- function(stations, startdate, enddate, daterange = NULL, apikey) {

  json2df <- function(id, date, key) {
    u <- paste0("https://api.weather.com/v2/pws/history/all?stationId=", id, "&format=json&units=m&date=", date, "&apiKey=", key)
    raw <- jsonlite::fromJSON(u)
    obs <- dplyr::as_tibble(raw$observations)
    if ("metric" %in% names(obs)) {
      obs <- dplyr::bind_cols(dplyr::select(obs, -metric), obs$metric)
    }
    return(obs)
  }

  dates <- if (!is.null(daterange)) {
    daterange
  } else {
    seq(lubridate::ymd(startdate), lubridate::ymd(enddate), by = "day") %>%
      purrr::keep(~ lubridate::month(.) %in% c(11, 12)) %>%
      format("%Y%m%d")
  }

  expand_grid(station = stations, date = dates) %>%
    mutate(data = purrr::map2(station, date, ~json2df(.x, .y, apikey))) %>%
    tidyr::unnest(data) |>
    dplyr::mutate(
      windspeedAvg_knots = windspeedAvg * 1.94384,
      windspeedAvg_kmh   = windspeedAvg * 3.6
    ) |>
    dplyr::select(obsTimeLocal, winddirAvg, windspeedAvg_knots, windspeedAvg_kmh) |>
    dplyr::rename(time = obsTimeLocal, bearing = winddirAvg, speed = windspeedAvg_kmh) |>
    dplyr::mutate(windspeed_ms = windspeedAvg_knots * 0.514444,
           date = lubridate::as_date(time))


}
