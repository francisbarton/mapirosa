"percent across 1" |>
  test_that({
    x <- 362706.5 # western edge of Stroud district
    pct_across <- x / 7e5
    number_of_tiles_across <- 159 # zoom 5
    tile_number <- floor(pct_across * number_of_tiles_across)
    expect_equal(tile_number, 82)
  })

"percent across 2" |>
  test_that({
    x <- 395352.2 # eastern edge of Stroud district
    pct_across <- x / 7e5
    number_of_tiles_across <- 159 # zoom 5
    tile_number <- floor(pct_across * number_of_tiles_across)
    expect_equal(tile_number, 89)
  })



"tile logix" |>
  test_that({
  nailsworth_27700 <- structure(c(
    xmin = 382036.196778455, ymin = 198166.20158993,
    xmax = 386279.599388408, ymax = 201155.90450143))
    zoom <- 4
    tile_width <- 14336
    xmin_tile <- tile_logic_27700(nailsworth_27700, zoom, tile_width)[[1]]
    expect_equal(xmin_tile, 43)
  })
