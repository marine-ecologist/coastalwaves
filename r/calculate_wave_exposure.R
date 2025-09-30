#' Calculate Wind Stress (kN/m) and Wave Energy Flux at Specified Depth (J/m/s)
#'
#' Computes two site-level exposure metrics from wind time series and fetch geometry:
#' \itemize{
#'   \item \strong{Wind stress index:} wind stress projected along fetch rays (kN/m).
#'   \item \strong{Wave energy flux at depth h:} SMB-based offshore waves corrected to user depth (J/m/s).
#' }
#'
#' @param wind Data frame with columns \code{date}, \code{bearing}, \code{windspeed_ms}.
#' @param fetch sf with \code{id}, \code{bearing}, \code{length_km} from \code{\link{calculate_fetch}}.
#' @param points sf with an \code{id} column to join results.
#' @param depth Numeric. Water depth in m (default 20).
#' @param parallel Logical. Use \pkg{furrr} for parallel computation.
#' @param cores Integer. Number of workers.
#'
#' @return sf with:
#' \describe{
#' \item{wind_stress_total}{Total wind stress × fetch (kN/m).}
#' \item{wind_stress_mean}{Mean daily wind stress × fetch (kN/m).}
#' \item{wave_energy_total}{Total wave energy flux at depth (J/m).}
#' \item{wave_energy_mean}{Mean daily wave energy flux at depth (J/m/s).}
#' }
#' @export
calculate_wave_exposure <- function(wind, fetch, points, depth = 20, parallel = TRUE, cores = 9) {
  rho <- 1025
  g   <- 9.81

  # helper to solve dispersion for k
  solve_k <- function(T, h) {
    omega <- 2*pi/T
    k <- omega^2 / g  # deep-water start
    for (j in 1:20) {
      k <- omega^2 / (g * tanh(k*h))
    }
    k
  }

  process_site <- function(i) {
    b <- dplyr::filter(fetch, id == i) |> sf::st_drop_geometry() |>
      dplyr::rename(fetch_bearing = bearing)

    wind |>
      tidyr::crossing(b) |>
      dplyr::mutate(
        angle_diff = abs(bearing - fetch_bearing),
        angle_diff = dplyr::if_else(angle_diff > 180, 360 - angle_diff, angle_diff),
        cos_theta  = cos(angle_diff * pi/180),
        U = windspeed_ms,
        F = length_km * 1000 * cos_theta,

        # Wind stress × fetch (kN/m)
        tau = 0.001 * (1.1 + 0.035 * U) * U^2,
        wind_stress_val = pmax(0, tau * F) / 1000,

        # --- SMB deep-water Hs and T ---
        Hs0 = 0.283 * tanh((0.0125*g*F/U^2)^0.42) * U^2/g,
        T0  = 7.54  * tanh((0.077*g*F/U^2)^0.25) * U/g,

        # --- Shallow/intermediate water correction ---
        k = solve_k(T0, depth),
        C = (2*pi/T0) / k,
        n = 0.5*(1 + (2*k*depth)/sinh(2*k*depth)),
        Cg = n * C,
        Cg_deep = g*T0/(4*pi),
        Ks = sqrt(Cg_deep / Cg),
        Hs = pmin(Hs0 * Ks, 0.78*depth),  # shoaled and depth-limited

        # Energy flux at depth
        E = 0.125 * rho * g * Hs^2,
        wave_energy_val = pmax(0, E * Cg)
      ) |>
      dplyr::group_by(date) |>
      dplyr::summarise(
        id = i,
        total_wind_stress = sum(wind_stress_val, na.rm = TRUE),
        total_wave_energy = sum(wave_energy_val, na.rm = TRUE),
        .groups = "drop"
      )
  }

  daily <- if (parallel) {
    furrr::future_map_dfr(unique(fetch$id), process_site, .options = furrr::furrr_options(), .progress = TRUE)
  } else {
    purrr::map_dfr(unique(fetch$id), process_site)
  }

  combined <- daily |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      wind_stress_total = sum(total_wind_stress, na.rm = TRUE),
      wind_stress_mean  = mean(total_wind_stress, na.rm = TRUE),
      wave_energy_total = sum(total_wave_energy, na.rm = TRUE),
      wave_energy_mean  = mean(total_wave_energy, na.rm = TRUE),
      .groups = "drop"
    )

  points |>
    dplyr::left_join(combined, by = "id") |>
    tidyr::drop_na(wind_stress_total, wave_energy_total)
}
