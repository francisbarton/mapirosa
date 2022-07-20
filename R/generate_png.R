
# generate_png_27700 ------------------------------------------------------



#' Create a set of PNGs and their spatial extents
#' @title generate_png_27700
#'
#' @description BNG: 700km (E|x) by 1300km (N|y) ie 7*13 = 91 100km^2 squares
#' @inheritParams build_basemap
#' @param ... params to pass to check_zoom(): allow_premium
generate_png_27700 <- function(bbox, zoom, type, squarify = TRUE, chatty = FALSE, ...) {


  assert_that(all(c(bbox[["xmin"]], bbox[["xmax"]]) <= 7e5) & all(c(bbox[["xmin"]], bbox[["xmax"]]) >= 0),
                          msg = "One or more x coordinates in the supplied bbox is out of scope for the British National Grid (less than zero or greater than 700,000).")
  assert_that(all(c(bbox[["ymin"]], bbox[["ymax"]]) <= 1.3e6) & all(c(bbox[["ymin"]], bbox[["ymax"]]) >= 0),
                          msg = "One or more y coordinates in the supplied bbox is out of scope for the British National Grid (less than zero or greater than 1,300,000).")


  check_zoom(zoom = zoom, type = type, crs = 27700, chatty = chatty, ...)

  xmin_tile <- tile_logic_27700(bbox[["xmin"]], zoom, "easting")
  xmax_tile <- tile_logic_27700(bbox[["xmax"]], zoom, "easting")
  ymin_tile <- tile_logic_27700(bbox[["ymin"]], zoom, "northing")
  ymax_tile <- tile_logic_27700(bbox[["ymax"]], zoom, "northing")

  # tile width in metres (also tile height)
  tile_width <- grid_tiles_27700 %>%
    dplyr::filter(.data$zoom == .env$zoom) %>%
    dplyr::pull(.data$res) %>%
    `*`(256)

  if (squarify) {
    sqt <- squarify(xmin_tile, xmax_tile, ymin_tile, ymax_tile)

    grid <- tidyr::expand_grid(x = seq(sqt[1], sqt[2]), y = seq(sqt[3], sqt[4]))
  } else {
    grid <- tidyr::expand_grid(x = seq(xmin_tile, xmax_tile), y = seq(ymin_tile, ymax_tile))
  }

  if (chatty & requireNamespace("usethis")) {
    stringr::str_glue("This request will retrieve {nrow(grid)} tiles, weighing approximately {nrow(grid) * 1.5}MB in total. Proceed?") %>%
      ui_yeah(n_no = 1, shuffle = FALSE)
  }


  png_data <- grid %>%
    purrr::pmap(read_png, zoom = zoom, type = type, crs = 27700)

  extents <- grid %>%
    dplyr::mutate(xmin = tile_width * .data$x) %>%
    dplyr::mutate(xmax = tile_width * (.data$x + 1)) %>%
    dplyr::mutate(ymin = 1.3e6 - (tile_width * (.data$y + 1))) %>%
    dplyr::mutate(ymax = 1.3e6 - (tile_width * .data$y)) %>%
    dplyr::select(.data$xmin:.data$ymax) %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(as.vector) %>%
    purrr::map(unlist)

  list(png_data, extents)
}



# generate_png_3857 -------------------------------------------------------



#' Create a set of PNGs and their spatial extents
#' @title generate_png_3857
#'
#' @inheritParams build_basemap
#' @param ... params to pass to check_zoom(): allow_premium
generate_png_3857 <- function(bbox, zoom, type, squarify = TRUE, chatty = FALSE, ...) {

  # assert_that(0 <= bbox["xmin"] <= 7e5 & 0 <= bbox["xmax"] <= 7e5,
  #             msg = "One or more x coordinates in the supplied bbox is out of scope for the British National Grid (less than zero or greater than 700,000).")
  # assert_that(0 <= bbox["ymin"] <= 1.3e6 & 0 <= bbox["ymax"] <= 1.3e6,
  #             msg = "One or more y coordinates in the supplied bbox is out of scope for the British National Grid (less than zero or greater than 1,300,000).")


  check_zoom(zoom = zoom, type = type, crs = 3857, chatty = chatty, ...)

  # https://epsg.io/3857
  # equator <- 40052753 # length of equator in m
  # equator <- 40097932 # width / height of EPSG:3857 map in m
  equator <- mean(c(40052753, 40097932))

  xmin_tile <- tile_logic_3857(bbox[["xmin"]], zoom, "longitude")
  xmax_tile <- tile_logic_3857(bbox[["xmax"]], zoom, "longitude")
  ymin_tile <- tile_logic_3857(bbox[["ymin"]], zoom, "latitude")
  ymax_tile <- tile_logic_3857(bbox[["ymax"]], zoom, "latitude")


  if (squarify) {
    sqt <- squarify(xmin_tile, xmax_tile, ymin_tile, ymax_tile)

    grid <- tidyr::expand_grid(x = seq(sqt[1], sqt[2]), y = seq(sqt[3], sqt[4]))
  } else {
    grid <- tidyr::expand_grid(x = seq(xmin_tile, xmax_tile), y = seq(ymin_tile, ymax_tile))
  }

  if (chatty & requireNamespace("usethis")) {
    stringr::str_glue("This request will retrieve {nrow(grid)} tiles, weighing approximately {nrow(grid) * 1.5}MB in total. Proceed?") %>%
      ui_yeah(n_no = 1, shuffle = FALSE)
  }

  png_data <- grid %>%
    purrr::pmap(read_png, zoom = zoom, type = type, crs = 3857)

  tile_side <- equator / 2^zoom


  extents <- grid %>%
    dplyr::mutate(xmin = (tile_side * .data$x) - (equator / 2)) %>%
    dplyr::mutate(xmax = (tile_side * (.data$x + 1)) - (equator / 2)) %>%
    dplyr::mutate(ymin = (equator / 2) - (tile_side * (.data$y + 1))) %>%
    dplyr::mutate(ymax = (equator / 2) - (tile_side * .data$y)) %>%
    dplyr::select(.data$xmin:.data$ymax) %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(as.vector) %>%
    purrr::map(unlist)

  list(png_data, extents)
}


# Helper functions --------------------------------------------------------


create_bb <- function(place, crs) {
  osmdata::getbb(place, limit = 1, format_out = "sf_polygon") |>
    sf::st_transform(crs = crs) |>
    sf::st_bbox()
}


squarify <- function(xmin_tile, xmax_tile, ymin_tile, ymax_tile) {

  x_length <- length(xmin_tile:xmax_tile)
  y_length <- length(ymin_tile:ymax_tile)

  current_aspect <- x_length / y_length

  if (current_aspect > 1) { # landscape: add rows to squarify
    new_y_length <- x_length
    add_top <- floor((new_y_length - y_length) / 2)
    add_bottom <- ceiling((new_y_length - y_length) / 2)
    ymax_tile <- ymax_tile - add_top
    ymin_tile <- ymin_tile + add_bottom
  }

  if (current_aspect < 1) { # portrait: add cols to squarify
    new_x_length <- y_length
    add_left <- floor((new_x_length - x_length) / 2)
    add_right <- ceiling((new_x_length - x_length) / 2)
    xmin_tile <- xmin_tile - add_left
    xmax_tile <- xmax_tile + add_right
  }

  c(xmin_tile, xmax_tile, ymin_tile, ymax_tile)
}


read_png <- function(...) {
  out <- safely_query_maps_api(...) %>%
    purrr::pluck("result")
  if (!is.null(out)) png::readPNG(out, info = TRUE) else out
}

#' Build and perform a query to the OS Maps API
#'
#' @title query_maps_api
#' @inheritParams build_basemap
#' @param x Tile number (horizontally)
#' @param y Tile number (vertically)
#' @param user_agent A User-Agent string to pass to the API
#' @return Raw PNG data
#' @export
query_maps_api <- function(x, y, zoom, type, crs, user_agent = NULL) {
  # OS Maps API - zxy (instead of WMTS)
  # See https://osdatahub.os.uk/docs/wmts/technicalSpecification
  # for details

  os_maps_base_url <- "https://api.os.uk/maps/raster/v1/zxy/"
  zxy_string <- stringr::str_glue(
    "{stringr::str_to_sentence(type)}_{crs}/{zoom}/{x}/{y}.png")


  # TODO implement proper process for key mgmt/user supply via {httr2} in time
  # this will to do for now...
  os_data_key <- Sys.getenv("OS_DATA_KEY")

  assert_that(length(os_data_key) > 0)

  default_user_agent <- "Francis Barton fbarton@alwaysdata.net"
  user_agent <- match.arg(user_agent, choices = default_user_agent)

  request(os_maps_base_url) %>%
    # replace UA string with package name once made into package
    req_user_agent(user_agent) %>%
    req_url_path_append(zxy_string) %>%
    req_url_query(key = os_data_key) %>%
    req_perform() %>%
    resp_check_status() %>%
    resp_body_raw() # raw PNG data
}

safely_query_maps_api <- function(...) "dummy"
