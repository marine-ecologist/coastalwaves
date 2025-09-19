#' Calculate Total and Mean Wave Energy at Coastal Points
#'
#' This function calculates daily and total wave energy at specified coastal points
#' using wind data and fetch distances for each directional bearing. Optionally supports parallel processing.
#'
#' @param wind A `data.frame` or `tibble` with columns `date`, `bearing`, and `windspeed_ms` (wind speed in m/s).
#' @param fetch An `sf` object with columns `id`, `bearing`, and `length_km`, typically returned by `calculate_fetch()`.
#' @param points An `sf` object with coastal points and an `id` column to join wave energy estimates.
#' @param parallel Logical. If `TRUE`, enables parallel processing using `furrr::future_map_dfr`. Default is `TRUE`.
#' @param cores Integer. Number of workers to use if parallel is enabled. (Note: must be set via `future::plan()` externally.)
#'
#' @return An `sf` object with original `points` and two new columns:
#'   - `wave_energy`: total cumulative wave energy (arbitrary units).
#'   - `wave_mean`: daily mean wave energy over the time series.
#'
#' @details
#' Wave energy is approximated using wind stress scaled by cosine of angular difference between wind and fetch bearing:
#' \deqn{wave\_energy = \tau \cdot \cos(\theta) \cdot \text{fetch}}
#' where \eqn{\tau} is wind stress and \eqn{\theta} is angular difference.
#'
#' Wind stress is calculated using:
#' \deqn{\tau = 0.001 \cdot (1.1 + 0.035 \cdot U) \cdot U^2}
#' where \eqn{U} is wind speed in m/s.
#'
#' @examples
#' \dontrun{
#'   future::plan(multisession, workers = 4)
#'   wave_output <- calculate_waves(wind_df, fetch_sf, points_sf, parallel = TRUE, cores = 4)
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise select left_join row_number if_else
#' @importFrom tidyr crossing drop_na
#' @importFrom furrr future_map_dfr furrr_options
#' @export


calculate_waves <- function(wind, fetch, points, parallel = TRUE, cores = 9) {

  wave_energy_daily <- if (parallel) {
    furrr::future_map_dfr(unique(fetch$id), function(i) {
      bearings_i <- dplyr::filter(fetch, id == i) |> sf::st_drop_geometry() |> dplyr::rename(fetch_bearing = bearing)

      wind |>
        tidyr::crossing(bearings_i) |>
        dplyr::mutate(
          angle_diff = abs(bearing - fetch_bearing),
          angle_diff = dplyr::if_else(angle_diff > 180, 360 - angle_diff, angle_diff),
          cos_theta = cos(angle_diff * pi / 180),
          windstress = 0.001 * (1.1 + 0.035 * windspeed_ms) * windspeed_ms^2,
          effective_windstress = windstress * cos_theta,
          wave_energy = effective_windstress * length_km * 1000
        ) |>
        dplyr::mutate(wave_energy = pmax(0, wave_energy)) |>
        dplyr::group_by(date) |>
        dplyr::summarise(
          id = i,
          total_wave_energy = sum(wave_energy, na.rm = TRUE),
          .groups = "drop"
        )
    }, .options = furrr::furrr_options(), .progress = TRUE)
  } else {
    purrr::map_dfr(unique(fetch$id), function(i) {
      bearings_i <- dplyr::filter(fetch, id == i) |> sf::st_drop_geometry() |> dplyr::rename(fetch_bearing = bearing)

      wind |>
        tidyr::crossing(bearings_i) |>
        dplyr::mutate(
          angle_diff = abs(bearing - fetch_bearing),
          angle_diff = dplyr::if_else(angle_diff > 180, 360 - angle_diff, angle_diff),
          cos_theta = cos(angle_diff * pi / 180),
          windstress = 0.001 * (1.1 + 0.035 * windspeed_ms) * windspeed_ms^2,
          effective_windstress = windstress * cos_theta,
          wave_energy = effective_windstress * length_km * 1000
        ) |>
        dplyr::mutate(wave_energy = pmax(0, wave_energy)) |>
        dplyr::group_by(date) |>
        dplyr::summarise(
          id = i,
          total_wave_energy = sum(wave_energy, na.rm = TRUE),
          .groups = "drop"
        )
    })
  }

  wave_energy_total <- wave_energy_daily |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      wave_energy = sum(total_wave_energy, na.rm = TRUE),
      wave_mean = mean(total_wave_energy, na.rm = TRUE),
      .groups = "drop"
    )

  points |>
    dplyr::left_join(wave_energy_total, by = "id") |>
    tidyr::drop_na(wave_energy)
}
