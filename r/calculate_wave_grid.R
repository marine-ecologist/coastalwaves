#' Create Wave Exposure Grid Polygons
#'
#' This function builds an unstructured grid of polygons along a reef/shoreline line,
#' joins wave exposure metrics to the polygons, and returns the grid ready for mapping.
#'
#' @param line An `sf` or `sfc` LINESTRING object representing the reef or shoreline boundary.
#' @param waves An `sf` object returned by `calculate_wave_exposure()` containing wave metrics at points.
#' @param interval Numeric. Spacing between sample points along the line (map units, e.g. meters).
#' @param width Numeric. Total buffer width around the line (map units, e.g. meters).
#'
#' @return An `sf` polygon grid with the wave metrics columns joined.
#'
#' @details
#' - Points are sampled along the input line at `interval` spacing.
#' - A Voronoi tessellation is created from those points and clipped to a corridor `width` wide.
#' - Each polygon is assigned wave metrics from the nearest sampled point.
#'
#' @examples
#' \dontrun{
#' grid <- create_wave_grid(whitsundays_union, whitsundays_waves, interval = 50, width = 20)
#' }
#'
#' @importFrom sf st_line_sample st_cast st_sf st_buffer st_union st_voronoi st_collection_extract st_intersection st_join st_nearest_feature st_geometry st_sfc
#' @importFrom dplyr mutate left_join select
#' @export
create_wave_grid <- function(line, waves, interval = 50, width = 50) {

  # 1. Sample points along line
  pts <- sf::st_line_sample(line, density = 1/interval) |>
    sf::st_cast("POINT") |>
    sf::st_sf() |>
    dplyr::mutate(id = seq_len(dplyr::n()))

  # 2. Buffer line to get corridor
  buf <- sf::st_buffer(line, dist = width/2)

  # 3. Voronoi tessellation and clip to buffer
  vor <- sf::st_voronoi(sf::st_union(pts)) |>
    sf::st_collection_extract("POLYGON")
  vor <- sf::st_intersection(sf::st_sf(geometry = vor), buf)

  # 4. Assign polygons to nearest points and join wave data
  grid <- sf::st_join(vor, pts, join = sf::st_nearest_feature)

  grid_with_waves <- grid %>%
    dplyr::left_join(
      waves |> sf::st_drop_geometry(), # |> dplyr::select(-geometry),
      by = "id"
    ) %>%
    tidyr::drop_na()

  grid_with_waves
}
