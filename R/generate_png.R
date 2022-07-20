#' Create a list of PNGs with a list of their spatial extents
#'
#' @inheritParams build_basemap

generate_png <- function(
    bbox,
    zoom,
    type = c("outdoor", "road", "light", "leisure"),
    crs = c(27700, 3857),
    squarify = TRUE,
    squarify_to = c("south", "east"),
    allow_premium = FALSE,
    chatty = NULL
    ) {



  # Run a quick check to make sure our zoom, type and CRS options
  # form a valid combination, and given whether we want to allow use of the
  # Premium (potentially chargeable) service
  check_zoom(zoom = zoom,
             type = type,
             crs = crs,
             allow_premium = allow_premium,
             chatty = chatty)





  # Check that bbox values are within the limits of the BNG
  if (crs == 27700) {
    assert_that(
      all(c(bbox[["xmin"]], bbox[["xmax"]]) <= 7e5) &
      all(c(bbox[["xmin"]], bbox[["xmax"]]) >= 0),
      msg = "One or more x coordinates in the supplied bbox is out of scope for the British National Grid (less than zero or greater than 700,000).")

    assert_that(
      all(c(bbox[["ymin"]], bbox[["ymax"]]) <= 1.3e6) &
      all(c(bbox[["ymin"]], bbox[["ymax"]]) >= 0),
      msg = "One or more y coordinates in the supplied bbox is out of scope for the British National Grid (less than zero or greater than 1,300,000).")
  }





  if (crs == 27700) {

    west_tile_col <- tile_logic_27700(bbox[["xmin"]], zoom, "easting")
    east_tile_col <- tile_logic_27700(bbox[["xmax"]], zoom, "easting")
    north_tile_row <- tile_logic_27700(bbox[["ymax"]], zoom, "northing")
    south_tile_row <- tile_logic_27700(bbox[["ymin"]], zoom, "northing")

    # tile width in metres (also tile height)
    tile_side <- grid_tiles_27700 %>%
      dplyr::filter(.data$zoom == .env$zoom) %>%
      dplyr::pull(.data$res) %>%
      `*`(256)
  }
  if (crs == 3857) {

    # https://epsg.io/3857
    # 40052753 = length of equator in m
    # 40097932 = width / height of EPSG:3857 map in m
    #
    # To make things work here, we take the mean of these two numbers,
    # and we don't ask any difficult questions about why this works.
    equator <- mean(c(40052753, 40097932))

    west_tile_col <- tile_logic_3857(bbox[["xmin"]], zoom, "easting")
    east_tile_col <- tile_logic_3857(bbox[["xmax"]], zoom, "easting")
    north_tile_row <- tile_logic_3857(bbox[["ymax"]], zoom, "northing")
    south_tile_row <- tile_logic_3857(bbox[["ymin"]], zoom, "northing")

    tile_side <- equator / 2^zoom
  }





  if (squarify) {
    sqt <- squarify(west_tile_col, east_tile_col, north_tile_row, south_tile_row, squarify_to = squarify_to)

    grid <- tidyr::expand_grid(x = seq(sqt[1], sqt[2]), y = seq(sqt[3], sqt[4]))
  } else {
    grid <- tidyr::expand_grid(x = seq(west_tile_col, east_tile_col), y = seq(north_tile_row, south_tile_row))
  }





  if (chatty & requireNamespace("usethis")) {
    stopifnot(
      stringr::str_glue("This request will retrieve {nrow(grid)} tiles, weighing approximately {nrow(grid) * 1.5}MB in total. Proceed?") %>%
        ui_yeah()
    )
  }





  read_png <- function(...) {
    out <- safely_query_maps_api(...) %>%
      purrr::pluck("result")
    if (!is.null(out)) png::readPNG(out, info = TRUE) else out
  }

  png_data <- grid %>%
    purrr::pmap(read_png, zoom = zoom, type = type, crs = crs)






  if (crs == 27700) {
    extents <- grid %>%
      dplyr::mutate(xmin = tile_side * .data$x) %>%
      dplyr::mutate(xmax = tile_side * (.data$x + 1)) %>%
      dplyr::mutate(ymin = 1.3e6 - (tile_side * (.data$y + 1))) %>%
      dplyr::mutate(ymax = 1.3e6 - (tile_side * .data$y))

  } else if (crs == 3857) {
    extents <- grid %>%
      dplyr::mutate(xmin = (tile_side * .data$x) - (equator / 2)) %>%
      dplyr::mutate(xmax = (tile_side * (.data$x + 1)) - (equator / 2)) %>%
      dplyr::mutate(ymin = (equator / 2) - (tile_side * (.data$y + 1))) %>%
      dplyr::mutate(ymax = (equator / 2) - (tile_side * .data$y))
  }

  extents_list <- extents %>%
    dplyr::select(.data$xmin:.data$ymax) %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(as.vector) %>%
    purrr::map(unlist)




  # return
  list(png_data, extents_list)
}



squarify <- function(west_tile_col, east_tile_col, north_tile_row, south_tile_row, squarify_to) {

  x_length <- length(west_tile_col:east_tile_col)
  y_length <- length(north_tile_row:south_tile_row)

  current_aspect <- x_length / y_length

  if (current_aspect > 1) { # "landscape": add rows to squarify
    new_y_length <- x_length
    add_north <- floor((new_y_length - y_length) / 2)
    add_south <- ceiling((new_y_length - y_length) / 2)
    north_tile_row <- north_tile_row - add_north
    south_tile_row <- south_tile_row + add_south
  }

  if (current_aspect < 1) { # "portrait": add cols to squarify
    new_x_length <- y_length
    add_west <- floor((new_x_length - x_length) / 2)
    add_east <- ceiling((new_x_length - x_length) / 2)
    west_tile_col <- west_tile_col - add_west
    east_tile_col <- east_tile_col + add_east
  }

  c(west_tile_col, east_tile_col, north_tile_row, south_tile_row)
}
