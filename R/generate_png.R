#' Create a set of PNGs and their spatial extents
#' @title generate_png_27700
#'
#' @description BNG: 700km (E|x) by 1300km (N|y) ie 7*13 = 91 100km^2 squares
#' @inheritParams build_basemap
#' @param ... params to pass to check_zoom(): allow_premium
generate_png_27700 <- function(bbox, zoom, type, aspect_ratio = NULL, expand_dir = c("eastwards", "northwards"), chatty = FALSE, ...) {


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

  if (!is.null(aspect_ratio)) {
    aspect_adjusted_tiles <- adjust_aspect(aspect_ratio, xmin_tile, xmax_tile, ymin_tile, ymax_tile, expand_dir = expand_dir)

    adj <- aspect_adjusted_tiles
    grid <- tidyr::expand_grid(x = seq(adj[1], adj[2]), y = seq(adj[3], adj[4]))
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

#' Create a set of PNGs and their spatial extents
#' @title generate_png_3857
#'
#' @inheritParams build_basemap
#' @param ... params to pass to check_zoom(): allow_premium and chatty
generate_png_3857 <- function(bbox, zoom, type, aspect, ...) {

  # assert_that(0 <= bbox["xmin"] <= 7e5 & 0 <= bbox["xmax"] <= 7e5,
  #             msg = "One or more x coordinates in the supplied bbox is out of scope for the British National Grid (less than zero or greater than 700,000).")
  # assert_that(0 <= bbox["ymin"] <= 1.3e6 & 0 <= bbox["ymax"] <= 1.3e6,
  #             msg = "One or more y coordinates in the supplied bbox is out of scope for the British National Grid (less than zero or greater than 1,300,000).")


  check_zoom(zoom = zoom, type = type, crs = 3857, ...)

  # https://epsg.io/3857
  east_west_m <- 40052753 # length of equator in m
  north_south_m <- 40097932

  xmin_tile <- tile_logic_3857(bbox[["xmin"]], zoom, "longitude")
  xmax_tile <- tile_logic_3857(bbox[["xmax"]], zoom, "longitude")
  ymin_tile <- tile_logic_3857(bbox[["ymin"]], zoom, "latitude")
  ymax_tile <- tile_logic_3857(bbox[["ymax"]], zoom, "latitude")


  grid <- tidyr::expand_grid(x = seq(xmin_tile, xmax_tile), y = seq(ymin_tile, ymax_tile))

  stringr::str_glue("This request will retrieve {nrow(grid)} tiles, weighing approximately {nrow(grid) * 1.5}MB in total. Proceed?") %>%
    ui_yeah(n_no = 1, shuffle = FALSE)


  png_data <- grid %>%
    purrr::pmap(read_png, zoom = zoom, type = type, crs = 3857)

  tile_width <- east_west_m / 2^zoom
  tile_height <- north_south_m / 2^zoom


  extents <- grid %>%
    dplyr::mutate(xmin = (tile_width * .data$x) - (east_west_m / 2)) %>%
    dplyr::mutate(xmax = (tile_width * (.data$x + 1)) - (east_west_m / 2)) %>%
    dplyr::mutate(ymin = (north_south_m / 2) - (tile_height * (.data$y + 1))) %>%
    dplyr::mutate(ymax = (north_south_m / 2) - (tile_height * .data$y)) %>%
    dplyr::select(.data$xmin:.data$ymax) %>%
    dplyr::rowwise() %>%
    dplyr::group_split() %>%
    purrr::map(as.vector) %>%
    purrr::map(unlist)

  list(png_data, extents)
}


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


read_png <- function(...) {
  safely_query_maps_api(...) %>%
    purrr::pluck("result") %>%
    png::readPNG(info = TRUE)
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
