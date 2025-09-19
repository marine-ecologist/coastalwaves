#' Generate Regularly Spaced Points Along Lines
#'
#' This function takes a `LINESTRING` or `MULTILINESTRING` `sf` object
#' and returns regularly spaced points along each line at a specified interval.
#'
#' @param input An `sf` object of type `LINESTRING` or `MULTILINESTRING`.
#' @param interval Numeric. Distance between points along the line (in CRS units).
#'
#' @return An `sf` object of `POINT` geometries with an `id` column.
#'
#' @examples
#' # lines <- st_read("coastline.gpkg")
#' # pts <- calculate_points(lines, interval = 500)
#' # plot(pts)
#'
#' @export


calculate_points <- function(input, interval){

  lengths <- sf::st_length(input)
  n_points <- ceiling(as.numeric(lengths) / interval)

  points_intervals <- purrr::map2_dfr(input, n_points, function(geom, n) {
    sf::st_line_sample(geom, sample = seq(0, 1, length.out = n)) |>
      sf::st_cast("POINT") |>
      sf::st_sf()
  }) |>
    dplyr::mutate(id = seq_len(dplyr::n())) |>
    sf::st_set_crs(sf::st_crs(input)) |>
    dplyr::rename(geometry=1)

  return(points_intervals)

}
