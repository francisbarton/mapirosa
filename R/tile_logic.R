tile_logic_27700 <- function(x, zoom, type = c("easting", "northing")) {
  if (type == "easting") {

    number_of_tiles_across <- grid_tiles_27700 %>%
      dplyr::filter(.data$zoom == .env$zoom) %>%
      dplyr::pull(.data$x)

    pct_across <- x / 7e5

    tile_number <- if (pct_across == 1) {
      number_of_tiles_across - 1
    } else {
      floor(pct_across * number_of_tiles_across)
    }
  }

  if (type == "northing") {

    number_of_tiles_down <- grid_tiles_27700 %>%
      dplyr::filter(.data$zoom == .env$zoom) %>%
      dplyr::pull(.data$y)

    pct_down <- (1.3e6 - x) / 1.3e6

    tile_number <- if (pct_down == 1) {
      number_of_tiles_down - 1
    } else {
      floor(pct_down * number_of_tiles_down)
    }
  }

  tile_number
}

tile_logic_3857 <- function(x, zoom, type = c("longitude", "latitude")) {

  # https://epsg.io/3857
  # equator <- 40052753 # length of equator in m
  # equator <- 40097932 # width / height of EPSG:3857 map in m
  equator <- mean(c(40052753, 40097932))

  if (type == "longitude") {
    pct_across <- ((equator / 2) + x) / equator
    tile_number <- floor(pct_across * 2^zoom)
  }
  if (type == "latitude") {
    pct_down <- ((equator / 2) - x) / equator
    tile_number <- floor(pct_down * 2^zoom)
  }

  tile_number
}
