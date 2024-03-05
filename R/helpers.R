#' Use {osmdata} to easily get a bbox object for a given place name
#'
#' Only the first search result from osmdata gets used, however
#'
#' @param place string: place name to search for
#' @param crs integer or string: the EPSG code for the desired CRS
#' @export
create_bbox <- function(place, crs) {
  osmdata::getbb(place, limit = 1, format_out = "sf_polygon") |>
    sf::st_transform(crs = crs) |>
    sf::st_bbox()
}
