#' Build and perform a query to the OS Maps API
#'
#' @inheritParams build_basemap
#' @param x Tile number (horizontally)
#' @param y Tile number (vertically)
#' @param user_agent A User-Agent string to pass to the API
#'
#' @returns Raw PNG data
#' @export
query_maps_api <- function(x, y, zoom, style, crs, user_agent = NULL) {
  # OS Maps API - zxy (instead of WMTS)
  # See https://osdatahub.os.uk/docs/wmts/technicalSpecification
  # for details

  os_maps_base_url <- "https://api.os.uk/maps/raster/v1/zxy/"
  style <- stringr::str_to_title(style)
  zxy_string <- stringr::str_glue(
    "{style}_{crs}/{zoom}/{x}/{y}.png")


  # TODO implement proper process for key mgmt/user supply via {httr2} in time
  # this will to do for now...
  os_data_key <- Sys.getenv("OS_DATA_KEY")

  assert_that(length(os_data_key) > 0)

  default_ua <- "mapirosa R package https://github.com/francisbarton/mapirosa"
  user_agent <- ifelse(is.null(user_agent), default_ua, user_agent)

  request(os_maps_base_url) |>
    req_user_agent(user_agent) |>
    req_url_path_append(zxy_string) |>
    req_url_query(key = os_data_key) |>
    req_perform() |>
    resp_check_status() |>
    resp_body_raw() # raw PNG data
}

safely_query_maps_api <- \(...) purrr::safely(query_maps_api)(...)
