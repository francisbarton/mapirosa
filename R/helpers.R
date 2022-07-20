create_bbox <- function(place, crs) {
  osmdata::getbb(place, limit = 1, format_out = "sf_polygon") |>
    sf::st_transform(crs = crs) |>
    sf::st_bbox()
}


