#' Retrieve PNG data (tiles) to cover a bbox, and generate a list of extents for each tile
#'
#' @param bbox A bbox object (a length 4 vector with names xmin, ymin, xmax,
#' ymax)
#' @param zoom Zoom level, an integer. For `crs = 27700`, this must be between
#' 0 and 13. For `crs = 3857`, this must be between 7 and 20. Certain
#' combinations of zoom level and style retrieve tiles from the OS's Premium
#' service, which can generate a financial charge once past a certain level of
#' usage. See the OS Maps API webpages for details.
#' @param crs CRS code (EPSG), an integer, either `27700` (British National
#' Grid) or `3857` (standard web mapping projected coordinate system eg Google
#' Maps, OSM).
#' @param style Map style, a string. One of "road", "outdoor", "light",
#' "leisure". Leisure is only available in the 27700 CRS.
#' @param squarify Whether to add rows/columns to a basemap to make it square.
#' `TRUE` by default. If a bbox covers a set of basemap tiles that is wider
#' than it is high ("landscape"), `squarify` will add row(s) of tiles as
#' necessary to make the basemap square. For a portrait basemap (higher than it
#' is wide), `squarify` retrieves extra columns to make the basemap square.
#' @param squarify_to Where `squarify` adds an odd number of columns or rows,
#' one more will be added to one side than to the opposite side. In case you
#' have a preference which side should receive more, you can stipulate this
#' here. Needs a character vector. This is set to `c("south", "east")` by
#' default. If you know that `squarify` will definitely add columns, say, you
#' can stipulate a single string e.g. `"west"`. If you are not sure whether it
#' will add rows or columns, then enter a vector of length 2, e.g.
#' `c("north", "west")` depending on your preferences. Note that if 5 rows are
#' added, 3 will be added to your preferred direction, and 2 to the other side;
#' this parameter does not force all 5 rows to be added to your preferred side!
#' @param cache_tiles Instead of converting PNG data from the API into a
#' basemap on the fly, cache data as local PNG files. Not functional yet;
#' hence set to `FALSE`.
#' @param allow_premium Whether to only access tiles from zoom levels that are
#' within the "OpenData" tier of the OS API. These vary according to map style
#' and CRS. See the [API Technical Specification](https://osdatahub.os.uk/docs/wmts/technicalSpecification) for details. Defaults to FALSE. Set to TRUE if
#' you wish to access zoom levels within the "Premium" service tier (chargeable
#' - see [https://osdatahub.os.uk/plans](https://osdatahub.os.uk/plans))
#' @param debug Whether to show any errors that were received from the API.
#' This package should handle errors gracefully in general, but if your basemap
#' is not complete then you may wish to turn this on to see what errors there
#' might be.
#' @returns a list of length 2: a list of PNG data and a list of extents for each tile
generate_png_data <- function(
  bbox,
  zoom,
  crs,
  style = c("outdoor", "road", "light", "leisure"),
  squarify = TRUE,
  squarify_to = c("south", "east"),
  cache_tiles = FALSE,
  allow_premium = FALSE,
  debug = FALSE
) {
  style <- rlang::arg_match(style)
  assert_that(squarify %in% c(TRUE, FALSE))
  assert_that(allow_premium %in% c(TRUE, FALSE))
  assert_that(debug %in% c(TRUE, FALSE))
  



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
    equator <- mean(c(40052753L, 40097932L))

    tile_size <- equator / 2^zoom
    # tile_size <- 0.1492923 * 2^(20 - zoom) * 256
    # tile_size <- 256 * (1223 / 2^(zoom - 7))


    # get the left, right, top and bottom column / row numbers
    corner_tiles <- tile_logic_3857(bbox, zoom, equator)

    left_edge <- (equator / 2) * -1L
    top_edge <- equator / 2
  }



  # squarify if desired, then build data grid -------------------------------

  if (squarify) {
    sqt <- squarify(corner_tiles, squarify_to = squarify_to)
  } else{
    sqt <- corner_tiles
  }


  # Create basic tile grid (x and y indices for all required tiles)
  grid <- tidyr::expand_grid(
    x = seq(sqt[[1]], sqt[[2]]),
    y = seq(sqt[[3]], sqt[[4]])
  ) |>
    dplyr::mutate(
      zoom = zoom,
      style = stringr::str_to_title(style),
      crs = crs
    )


  halt <- FALSE
  if (interactive()) {
    halt <- ui_nope(c(
      "This request will retrieve {nrow(grid)} tiles, weighing approximately",
      "{nrow(grid) * 1.5}MB in total. Proceed?"
    ), n_no = 1)
  }

  if (halt) {
    ui_info("Quitting process through user choice.")
    return(invisible(TRUE))
  } else {
    api_response <- handle_maps_api_response(grid, debug)

    png_data <- api_response[[1]]
    ret_grid <- api_response[[2]]
    
    # Use constants (by CRS) to create tile extents from data grid ------------
    tile_extents <- ret_grid |>
      dplyr::mutate(
        xmin = (.env[["tile_size"]] * .data[["x"]]) + .env[["left_edge"]],
        .keep = "unused"
      ) |>
      dplyr::mutate(xmax = .data[["xmin"]] + .env[["tile_size"]]) |>
      dplyr::mutate(
        ymin = .env[["top_edge"]] - (.env[["tile_size"]] * (.data[["y"]] + 1)),
        .keep = "unused"
      ) |>
      dplyr::mutate(ymax = .data[["ymin"]] + .env[["tile_size"]]) |>
      dplyr::rowwise() |>
      dplyr::group_split() |>
      # purrr::map(as.vector) |>
      purrr::map(unlist)

    list(png_data, tile_extents)
  }
}


handle_maps_api_response <- function(.data, debug) {

  assert_that(is.data.frame(.data))
  cl <- c("x", "y", "zoom", "style", "crs")
  assert_that(identical(colnames(.data), cl))
  assert_that(debug %in% c(TRUE, FALSE))

  # Use data (grid) and API query function to retrieve PNG data ---------------

  api_response <- .data  |>
    purrr::pmap(safely_query_maps_api)

  api_errors <- api_response |>
    purrr::map("error") |>
    purrr::compact() |>
    purrr::list_c() |>
    unique()

  api_results <- api_response |>
    purrr::map("result")

  # Building in some safety catches, just in case we got some NULLs
  success <- which(!purrr::map_lgl(api_results, is.null))

  if (length(success) < nrow(.data)) {
    ui_info("{length(success)} of {nrow(.data)} tiles retrieved.")
  }

  # Let's see how well this works. Presentation of readout may need improving
  if (debug & length(api_errors) > 0) {
    ui_info("Sample error: {utils::head(api_errors, 1L)}")
  }

  # Only once we have got rid of NULLs do we try to read raw data to PNG format
  png_data <- api_results |>
    purrr::compact() |>
    purrr::map(png::readPNG, info = TRUE)

  # If we did get some NULLs that we removed from `png_data` using `compact()`,
  # we also need to remove those rows from the grid so the two match up
  df_out <- .data |>
    dplyr::select(!all_of(c("zoom", "style", "crs"))) |>
    dplyr::slice(success)

  assert_that(length(png_data) == nrow(df_out))

  list(png_data, df_out)
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
