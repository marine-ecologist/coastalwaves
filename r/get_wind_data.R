#' Download NOAA Blended Daily Winds for an sf bounding box
#'
#' This function queries the NOAA CoastWatch ERDDAP
#' `noaacwBlendedWindsDaily` dataset for `u_wind` and `v_wind` components
#' over the bounding box of an input `sf` object and a specified date range.
#'
#' @param sf_obj An `sf` object. The bounding box (`st_bbox`) is used to
#'   define the spatial extent (latitude and longitude).
#' @param start_date Character string, start date in `"YYYY-MM-DD"` format.
#' @param end_date Character string, end date in `"YYYY-MM-DD"` format.
#' @param zlev Numeric, vertical level in meters. Default = `10.0`.
#'
#' @details
#' The function constructs a direct ERDDAP CSV request URL and downloads
#' the `u_wind` and `v_wind` variables for the given spatial and temporal
#' extent. Results are returned as a data frame with additional columns:
#' \describe{
#'   \item{wind_speed}{Wind speed in m/s calculated as
#'     \eqn{\sqrt{u^2 + v^2}}}
#'   \item{wind_bearing}{Wind direction in degrees clockwise from north,
#'     derived from `atan2(u, v)`.}
#' }
#'
#' @return A `data.frame` with the requested wind data (`u_wind`, `v_wind`,
#'   `time`, `latitude`, `longitude`, and derived `wind_speed` and
#'   `wind_bearing`).
#'
#' @examples
#' \dontrun{
#' library(sf)
#' pt <- st_point(c(144, -11)) |>
#'   st_sfc(crs = 4326) |>
#'   st_sf()
#'
#' wind_df <- get_wind_data(
#'   pt,
#'   start_date = "1988-09-01",
#'   end_date   = "1988-09-03"
#' )
#' head(wind_df)
#' }
#'
#' @importFrom sf st_bbox
#' @importFrom httr GET stop_for_status content
#' @importFrom readr read_csv
#' @importFrom dplyr mutate
#' @export
get_wind_data <- function(sf_obj, start_date, end_date, zlev = 10.0) {
  # Extract bbox
  bb <- sf::st_bbox(sf_obj |> st_transform(4326))

  base <- "https://coastwatch.noaa.gov/erddap/griddap/noaacwBlendedWindsDaily.csv"

  url <- sprintf(
    paste0("%s?",
           "u_wind[(%s):1:(%s)][(%s):1:(%s)][(%s):1:(%s)][(%s):1:(%s)],",
           "v_wind[(%s):1:(%s)][(%s):1:(%s)][(%s):1:(%s)][(%s):1:(%s)]"),
    base,
    start_date, end_date,    # time
    zlev, zlev,              # zlev
    bb["ymin"], bb["ymax"],  # latitude
    bb["xmin"], bb["xmax"],  # longitude
    start_date, end_date,
    zlev, zlev,
    bb["ymin"], bb["ymax"],
    bb["xmin"], bb["xmax"]
  )

  res <- httr::GET(url)
  httr::stop_for_status(res)

  df <- readr::read_csv(
    httr::content(res, as = "text", encoding = "UTF-8"),
    show_col_types = FALSE
  )

  # Add wind speed and bearing
 df <- df |>
   dplyr::slice(-1) |>
   dplyr::mutate(across(c("zlev", "latitude", "longitude", "u_wind", "v_wind"), as.numeric)) |>
   dplyr::mutate(time = as.POSIXlt(time)) |>
    dplyr::mutate(
      wind_speed_knots = sqrt(u_wind^2 + v_wind^2),
      windspeed_ms =  wind_speed_knots / 0.514444,
      bearing = (atan2(u_wind, v_wind) * 180 / pi + 180) %% 360
  ) |> dplyr::select(-zlev) |> dplyr::rename(date=time)

  return(df)
}
