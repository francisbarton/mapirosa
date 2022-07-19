east_west_m <- 40052753 # length of equator in m
north_south_m <- 40097932

zoom <- 12

# 4 tiles
xmin_tile <- 2019
xmax_tile <- 2020
ymin_tile <- 1360
ymax_tile <- 1361

grid <- tidyr::expand_grid(x = seq(xmin_tile, xmax_tile), y = seq(ymin_tile, ymax_tile))

tile_width <- east_west_m / 2^zoom
tile_height <- north_south_m / 2^zoom

extents <- grid %>%
  dplyr::mutate(xmin = (tile_width * x) - (east_west_m / 2)) %>%
  dplyr::mutate(xmax = (tile_width * (x + 1)) - (east_west_m / 2)) %>%
  dplyr::mutate(ymin = (north_south_m / 2) - (tile_height * (y + 1))) %>%
  dplyr::mutate(ymax = (north_south_m / 2) - (tile_height * y)) %>%
  dplyr::select(xmin:ymax) %>%
  dplyr::rowwise() %>%
  dplyr::group_split() %>%
  purrr::map(as.vector) %>%
  purrr::map(unlist)

# nailsworth_3857 <- osmdata::getbb("Nailsworth", limit = 1, format_out = "sf_polygon") |>
#   sf::st_transform(crs = 3857) |>
#   sf::st_bbox()
nailsworth_3857 <- structure(c(xmin = -251731.974282999, ymin = 6742840.4730051,
                          xmax = -244890.601205367, ymax = 6747653.11339102))

# nailsworth_27700 <- osmdata::getbb("Nailsworth", limit = 1, format_out = "sf_polygon") |>
#   sf::st_transform(crs = 27700) |>
#   sf::st_bbox()
nailsworth_27700 <- structure(c(xmin = 382036.196778455, ymin = 198166.20158993,
                                xmax = 386279.599388408, ymax = 201155.90450143))

"check extents 1" %>%
  test_that({
    expect_equal(nrow(grid), length(extents))
    expect_length(extents[[1]], 4)
  })

"check extents 2" %>%
  test_that({
    expect_equal(extents[[1]][["xmax"]], extents[[1]][["xmin"]] + tile_width)
    expect_equal(extents[[2]][["ymax"]], extents[[2]][["ymin"]] + tile_height)
  })

"tile width" %>%
  test_that({
    zoom <- 4

    tile_width <- grid_tiles_27700 %>%
      dplyr::filter(.data$zoom == .env$zoom) %>%
      dplyr::pull(.data$res) %>%
      `*`(256)

    expect_equal(tile_width, 14336)
  })

"aspect_adjustment" %>%
  test_that({

    adjust_aspect <- function(aspect, xmin_tile, xmax_tile, ymin_tile, ymax_tile, expand_dir = c("eastwards", "northwards")) {
      x_length <- length(seq(xmin_tile:xmax_tile))
      y_length <- length(seq(ymin_tile:ymax_tile))

      current_aspect <- x_length / y_length

      if (aspect > current_aspect) { # widen (add cols) if poss.
        new_x_length <- ceiling(y_length * aspect)
        if ("westwards" %in% expand_dir) {
          xmin_tile <- xmin_tile - (new_x_length - x_length)
        } else {
          xmax_tile <- xmax_tile + (new_x_length - x_length)
        }
      } else if (aspect < current_aspect) { # add rows if poss.
        new_y_length <- ceiling(x_length * aspect)
        if ("southwards" %in% expand_dir) {
          ymax_tile <- ymax_tile + (new_y_length - y_length)
        } else {
          ymin_tile <- ymin_tile - (new_y_length - y_length)
        }
      }
      c(xmin_tile, xmax_tile, ymin_tile, ymax_tile)
    }

    exp_out <- c(2000, 2004, 1299, 1303)
    out <- adjust_aspect(1, 2000, 2004, 1300, 1303)

    expect_equal(out, exp_out)

    exp_out <- c(2000, 2004, 1300, 1304)
    out <- adjust_aspect(1, 2000, 2004, 1300, 1303, expand_dir = "southwards")

    expect_equal(out, exp_out)
  })

"tile logix" %>%
  test_that({
    zoom <- 4
    tile_width <- 14336
    xmin_tile <- tile_logic_27700(nailsworth_27700[["xmin"]], zoom, "easting")
    xmax_tile <- tile_logic_27700(nailsworth_27700[["xmax"]], zoom, "easting")
    ymin_tile <- tile_logic_27700(nailsworth_27700[["ymin"]], zoom, "northing")
    ymax_tile <- tile_logic_27700(nailsworth_27700[["ymax"]], zoom, "northing")

    grid <- tidyr::expand_grid(x = seq(xmin_tile, xmax_tile), y = seq(ymin_tile, ymax_tile))

    png_data <- grid %>%
      purrr::pmap(read_png, zoom = zoom, type = "road", crs = 27700)

    expect_equal(nrow(grid), length(png_data))
  })
