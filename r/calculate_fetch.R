#' Calculate Fetch Lengths from Points Using Radiating Bearings
#'
#' This function generates rays from input points at specified angular intervals,
#' clips them to land polygons, and calculates the effective fetch length
#' (open-water distance) for each direction. Optionally runs in parallel.
#'
#' @param points An `sf` object of type `POINT` with a column `id`.
#' @param degrees Numeric. Angular spacing between bearings (e.g., 7.5, 10).
#' @param fetch Numeric. Maximum fetch distance in meters. Default is 5000.
#' @param land An `sf` object representing land polygons (must overlap rays).
#' @param parallel Logical. If `TRUE`, uses `furrr::future_map_dfr` for parallel execution.
#' @param cores Integer. Number of cores to use if `parallel = TRUE`.
#'
#' @return An `sf` object with `id`, `bearing`, `segment_id`, `geometry`, and `length_km` columns.
#'
#' @details
#' - Rays are generated at `degrees` intervals around the compass.
#' - Rays are clipped by land polygons to estimate open-water fetch.
#' - Fetch is measured as the remaining ray segment length after clipping land.
#'
#' @examples
#' \dontrun{
#'   future::plan(multisession, workers = 4)
#'   fetch_data <- calculate_fetch(my_points, degrees = 10, fetch = 5000, land = gbr_land, parallel = TRUE)
#' }
#'
#' @importFrom sf st_coordinates st_linestring st_sfc st_sf st_union st_transform st_difference st_buffer st_length st_cast st_join st_crs
#' @importFrom purrr map_dfr
#' @importFrom furrr future_map_dfr furrr_options
#' @importFrom dplyr filter mutate row_number rename select
#' @export
#'

calculate_fetch <- function(points, degrees, fetch = 5000, land, parallel = TRUE, cores = 9) {

  bearings <- sort(seq(degrees, 360, by = degrees) %% 360)

  make_rays <- function(point, id, length = fetch) {
    coord <- sf::st_coordinates(point)

    lines <- purrr::map(bearings, function(bearing) {
      angle_rad <- bearing * pi / 180
      x_end <- coord[1] + length * sin(angle_rad)
      y_end <- coord[2] + length * cos(angle_rad)
      sf::st_linestring(rbind(coord, c(x_end, y_end)))
    })

    sf::st_sf(
      id = id,
      bearing = bearings,
      geometry = sf::st_sfc(lines, crs = 20353)
    )
  }

  rays <- if (parallel) {
    furrr::future_map_dfr(1:nrow(points), function(i) make_rays(points[i, ], points$id[i]), .options = furrr::furrr_options(), .progress = TRUE)
  } else {
    purrr::map_dfr(1:nrow(points), function(i) make_rays(points[i, ], points$id[i]))
  }

  land_union <- sf::st_union(land)
  land_union_proj <- sf::st_transform(land_union, sf::st_crs(rays))
  rays_intersect <- sf::st_difference(rays, land_union_proj)

  points_buffered <- sf::st_buffer(points, 1)

  final_bearings <- if (parallel) {
    furrr::future_map_dfr(unique(points$id), function(i) {
      pts_i <- dplyr::filter(points_buffered, id == i)
      rays_i <- dplyr::filter(rays_intersect, id == i)

      rays_i |>
        sf::st_cast("LINESTRING", warn = FALSE) |>
        dplyr::mutate(segment_id = dplyr::row_number()) |>
        sf::st_join(
          dplyr::rename(pts_i, id_poly = id),
          join = sf::st_intersects,
          left = FALSE
        ) |>
        dplyr::mutate(length_km = as.numeric(sf::st_length(geometry)) / 1000) |>
        dplyr::select(id = id_poly, bearing, segment_id, geometry, length_km)
    }, .options = furrr::furrr_options(), .progress = TRUE)
  } else {
    purrr::map_dfr(unique(points$id), function(i) {
      pts_i <- dplyr::filter(points_buffered, id == i)
      rays_i <- dplyr::filter(rays_intersect, id == i)

      rays_i |>
        sf::st_cast("LINESTRING", warn = FALSE) |>
        dplyr::mutate(segment_id = dplyr::row_number()) |>
        sf::st_join(
          dplyr::rename(pts_i, id_poly = id),
          join = sf::st_intersects,
          left = FALSE
        ) |>
        dplyr::mutate(length_km = as.numeric(sf::st_length(geometry)) / 1000) |>
        dplyr::select(id = id_poly, bearing, segment_id, geometry, length_km)
    })
  }

  return(final_bearings)
}
