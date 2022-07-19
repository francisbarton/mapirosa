"tile logic 1" %>%
  test_that({
    zoom <- 5
    number_of_tiles_across <- grid_tiles_27700 %>%
      dplyr::filter(.data$zoom == .env$zoom) %>%
      dplyr::pull(x)

    expect_equal(number_of_tiles_across, 159)
})

"percent across 1" %>%
  test_that({
    x <- 362706.5 # western edge of Stroud district
    pct_across <- x / 7e5
    number_of_tiles_across <- 159 # zoom 5
    tile_number <- floor(pct_across * number_of_tiles_across)
    expect_equal(tile_number, 82)
  })

"percent across 2" %>%
  test_that({
    x <- 395352.2 # eastern edge of Stroud district
    pct_across <- x / 7e5
    number_of_tiles_across <- 159 # zoom 5
    tile_number <- floor(pct_across * number_of_tiles_across)
    expect_equal(tile_number, 89)
  })

"tile logic 2" %>%
  test_that({
    tile_number <- tile_logic_27700(395352.2, 5, "easting")
    expect_equal(tile_number, 89)
  })

"tile logic 3" %>%
  test_that({
    x <- 6726301.1 # northern border of Stroud district
    tile_number <- tile_logic_3857(x, 12, "latitude")
    expect_equal(tile_number, 1360)
  })

# nailsworth_3857 <- osmdata::getbb("Nailsworth", limit = 1, format_out = "sf_polygon") |>
#   sf::st_transform(crs = 3857) |>
#   sf::st_bbox()
nailsworth_27700 <- structure(c(xmin = 382036.196778455, ymin = 198166.20158993,
                                xmax = 386279.599388408, ymax = 201155.90450143))

"tile logix" %>%
  test_that({
    zoom <- 4
    tile_width <- 14336
    xmin_tile <- tile_logic_27700(nailsworth_27700[["xmin"]], zoom, "easting")
    expect_equal(xmin_tile, 43)
  })
