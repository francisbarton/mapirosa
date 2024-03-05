#' Retrieve PNG data (tiles) to cover a bbox, and generate a list of extents for each tile
#'
#' @inheritParams build_basemap
#' @returns a list of length 2: a list of PNG data and a list of extents for each tile
generate_png_data <- function(
    bbox,
    zoom,
    style = c("outdoor", "road", "light", "leisure"),
    crs = c(27700, 3857),
    squarify = TRUE,
    squarify_to = c("south", "east"),
    allow_premium = FALSE,
    debug = FALSE
    ) {


  assert_that(length(bbox) == 4)
  assert_that(all(is.numeric(bbox)))
  assert_that(crs %in% c(27700, 3857))
  style <- tolower(style)
  assert_that(style %in% c("outdoor", "road", "light", "leisure"))

  # Check zoom, style, crs and data tier -------------------------------------


  # Run a quick check to make sure our zoom, style and CRS options
  # form a valid combination, and given whether we want to allow use of the
  # Premium (potentially chargeable) data tier
  check_zoom(
    zoom = zoom,
    style = style,
    crs = crs,
    allow_premium = allow_premium
  )





  # Check that bbox values are within the limits of the BNG -----------------


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




  # Set up some key constants ----------------------------------------------



  if (crs == 27700) {
    # https://github.com/OrdnanceSurvey/tile-name-derivation
    false_origin_x <- -238375.0
    false_origin_y <- 1376256.0
    tile_size <- 256 * (896 / 2^zoom)

    # get the left, right, top and bottom column / row numbers
    corner_tiles <- tile_logic_27700(bbox, zoom, tile_size)

    left_edge <- false_origin_x
    top_edge <- false_origin_y
  }


  if (crs == 3857) {

    # https://epsg.io/3857
    # 40052753 = length of equator in m
    # 40097932 = width / height of EPSG:3857 map in m
    #
    # To make things work here, we take the mean of these two numbers,
    # and we don't ask any awkward questions about why this works.
    equator <- mean(c(40052753, 40097932))

    tile_size <- equator / 2^zoom
    # tile_size <- 0.1492923 * 2^(20 - zoom) * 256
    # tile_size <- 256 * (1223 / 2^(zoom - 7))


    # get the left, right, top and bottom column / row numbers
    corner_tiles <- tile_logic_3857(bbox, zoom, equator)

    left_edge <- (equator / 2) * -1
    top_edge <- equator / 2
  }



  # squarify if desired, then build data grid -------------------------------

  if (squarify) {
    sqt <- squarify(corner_tiles, squarify_to = squarify_to)
  } else{
    sqt <- corner_tiles
  }

  grid <- tidyr::expand_grid(
    x = seq(sqt[[1]], sqt[[2]]),
    y = seq(sqt[[3]], sqt[[4]]))


  if (interactive()) {
    if (ui_yeah(c(
        "This request will retrieve {nrow(grid)} tiles, weighing approximately",
        "{nrow(grid) * 1.5}MB in total. Proceed?"
      ))) {
      invisible(TRUE)
    } else {
      return(invisible(TRUE))
    }
  }


  # Use data grid and API query function to retrieve PNG data ---------------


  title_style <- stringr::str_to_title(style)

  # `possibly()` might be more appropriate than `safely()`, as we are not
  # intending to do anything with any error messages anyway - just return NULL
  api_response <- grid |>
    purrr::pmap(safely_query_maps_api,
    zoom = zoom,
    style = title_style,
    crs = crs)

  if (debug) {
    api_errors <- api_response |>
      purrr::map("error") |>
      purrr::compact()

    # Let's see how well this works. Presentation of readout may need improving
    if (length(api_errors) > 0) cat(api_errors)
  }

  api_results <- api_response |>
    purrr::map("result")

  # Building in some safety catches,
  # just in case we got some NULLs
  success <- which(!purrr::map_lgl(api_results, is.null))

  if (length(success) < nrow(grid)) {
    stringr::str_glue("{length(success)} of {nrow(grid)} tiles retrieved.") |>
      ui_info()
  }


  # only once we have got rid of NULLs do we try to read in the raw data
  # to PNG format
  png_data <- api_results |>
    purrr::compact() |>
    purrr::map(png::readPNG, info = TRUE)

  # If we did get some NULLs that we removed from `png_data` using `compact()`
  # we also need to remove those rows from the grid so the two match up
  grid <- grid |>
    dplyr::slice(success)

  assert_that(length(png_data) == nrow(grid))


  # Use constants (by CRS) to create tile extents from data grid ------------


  tile_extents <- grid |>
    dplyr::mutate(xmin = (tile_size * .data[["x"]]) + left_edge, .keep = "unused") |>
    dplyr::mutate(xmax = .data[["xmin"]] + tile_size) |>
    dplyr::mutate(ymin = top_edge - (tile_size * (.data[["y"]] + 1)), .keep = "unused") |>
    dplyr::mutate(ymax = .data[["ymin"]] + tile_size) |>
    dplyr::rowwise() |>
    dplyr::group_split() |>
    # purrr::map(as.vector) |>
    purrr::map(unlist)



  # return
  list(png_data, tile_extents)
}



squarify <- function(corner_tiles, squarify_to) {

  ct <- corner_tiles
  x_length <- length(ct[[1]]:ct[[2]])
  y_length <- length(ct[[3]]:ct[[4]])

  current_aspect <- x_length / y_length

  if (current_aspect < 1) { # "portrait": add cols to squarify
    new_x_length <- y_length

    if ("east" %in% squarify_to) {
      add_west <- floor((new_x_length - x_length) / 2)
      add_east <- ceiling((new_x_length - x_length) / 2)
    } else {
      add_west <- ceiling((new_x_length - x_length) / 2)
      add_east <- floor((new_x_length - x_length) / 2)
    }

    ct[[1]] <- ct[[1]] - add_west
    ct[[2]] <- ct[[2]] + add_east
  }

  if (current_aspect > 1) { # "landscape": add rows to squarify
    new_y_length <- x_length

    if ("south" %in% squarify_to) {
      add_north <- floor((new_y_length - y_length) / 2)
      add_south <- ceiling((new_y_length - y_length) / 2)
    } else {
      add_north <- ceiling((new_y_length - y_length) / 2)
      add_south <- floor((new_y_length - y_length) / 2)
    }

    ct[[3]] <- ct[[3]] - add_north
    ct[[4]] <- ct[[4]] + add_south
  }

  # return
  ct
}
