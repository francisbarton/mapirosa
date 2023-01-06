"test create_bbox 1" |>
  test_that({
    oxford <- create_bbox("Oxford", 27700)
    expect_equal(length(oxford), 4)
    expect_true(all(is.numeric(oxford)))
    expect_named(oxford, c("xmin", "ymin", "xmax", "ymax"))

    expect_true(all(c(oxford[["xmin"]], oxford[["xmax"]]) <= 7e5))
    expect_true(all(c(oxford[["xmin"]], oxford[["xmax"]]) >= 0))

    expect_true(all(c(oxford[["ymin"]], oxford[["ymax"]]) <= 1.3e6))
    expect_true(all(c(oxford[["ymin"]], oxford[["ymax"]]) >= 0))
  })



"test tile logic" |>
  test_that({
    style <- "road"
    zoom <- 7
    crs <- 27700

    # 27700 coords
    oxford <- c(
      xmin = 448100.3,
      ymin = 201646.6,
      xmax = 457014.2,
      ymax = 211088.8)

    expect_true(check_zoom(zoom = zoom, style = style, crs = crs))

    false_origin_x <- -238375.0
    false_origin_y <- 1376256.0
    tile_size <- 256 * (896 / 2^zoom)

    corner_tiles <- tile_logic_27700(oxford, zoom, tile_size)

    expect_vector(corner_tiles, 4)
    expect_equal(corner_tiles, c(383, 388, 650, 655))

    # sqt <- squarify(corner_tiles, squarify_to = c("south", "east"))

    ct <- corner_tiles
    x_length <- length(ct[[1]]:ct[[2]])
    y_length <- length(ct[[3]]:ct[[4]])

    # for this particular combination of bbox and zoom, x should == y,
    # obviously not always true
    expect_equal(x_length, y_length)
  })
