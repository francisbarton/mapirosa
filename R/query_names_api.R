#' Retrieve data from the OS Names API
#'
#' Only uses 'find' method currently, not 'nearest' yet.
#' See https://osdatahub.os.uk/docs/names/technicalSpecification for details.
#'
#' @param x character. The name of a place, or a postcode for example. The API
#'  docs say 'A free string text search of OS Names, intended to be an
#'  ambiguous/fuzzy search.'
#' @param local_types character vector. Types of result to return. Examples are:
#'  'City', 'Village'. See API docs for all options. `NULL` by default (= no
#'  filtering by type).
#' @param bounds bbox. The bounding box of an area to limit search to. `NULL` by
#'  default - no filtering by area. Can be supplied as a vector of 4 numerics,
#'  in the following XMIN,YMIN,XMAX,YMAX form: c(414000, 114000, 414100, 114100)
#' @param max_results integer. How many results to return. Can be from 1-100.
#'  Set to 1 by default.
#' @param user_agent character. `NULL` by default, which will use the package
#'  name.
#'
#' @returns An sfc geospatial tibble
#' @examples
#'
#' query_names_api(c("Stroud", "Gloucester"))
#' @export
query_names_api <- function(
    x,
    local_types = NULL,
    bounds = NULL,
    max_results = 1L,
    user_agent = NULL) {

  assert_that(is.character(x))
  assert_that(max_results %in% seq(100L))


  if (!is.null(local_types)) {
    local_types <- paste0(
      "LOCAL_TYPE:",
      snakecase::to_any_case(
        local_types,
        case = "title",
        sep_out = "_",
        parsing_option = 1
      )
    ) |>
    stringr::str_c(collapse = " ")
  }

  if (!is.null(bounds)) {
    # if area is not already a vector then assume it is an sf object,
    # check its CRS is 27700 (or convert if not) and get its bbox
    if (!rlang::is_vector(bounds, n = 4)) {
      bounds <- bounds |>
        sf::st_transform(27700) |>
        sf::st_bbox()
    }

    # if bounds was already passed in as a bbox or other vector
    # then just collapse it with commas (doesn't check if coords are in 27700)
    bounds <- round(bounds, 2L) # API requires 2dp max.
    bounds <- stringr::str_flatten(bounds, collapse = ",")
  }
  assert_that(is.character(bounds))

  default_ua <- "mapirosa R package https://github.com/francisbarton/mapirosa"
  user_agent <- ifelse(is.null(user_agent), default_ua, user_agent)
  assert_that(is.character(user_agent))

  snq <- \(...) purrr::safely(names_query)(...)

  args <- list(
    local_types = local_types,
    bounds = bounds,
    max_results = max_results,
    user_agent = user_agent
  )

  res <- x |>
    purrr::map(\(x) snq(x, !!!args))

  errs <- purrr::map(res, "error") |> purrr::compact()
  ui_info("{length(errs)} errors occurred")

  res |>
    purrr::map("result") |>
    purrr::compact() |>
    purrr::map(httr2::resp_body_json) |>
    purrr::map("results") |>
    purrr::map(\(x) purrr::pluck(x, 1L, 1L)) |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind() |>
    janitor::clean_names() |>
    sf::st_as_sf(
      coords = c("geometry_x", "geometry_y"),
      crs = 27700,
      remove = FALSE
    )
}


names_query <- function(x, ...) {

  args <- rlang::list2(...)

  # OS Names API. See
  # https://osdatahub.os.uk/docs/names/technicalSpecification
  # for details

  # TODO implement proper process for key mgmt/user supply via {httr2}.
  # This will to do for now...
  os_data_key <- Sys.getenv("OS_DATA_KEY")

  os_base_url <- "https://api.os.uk/search/names/v1/find"

  request(os_base_url) |>
    req_url_query(key = os_data_key) |>
    req_url_query(query = x) |>
    req_url_query(maxresults = args[["max_results"]]) |>
    req_url_query(bounds = args[["bounds"]]) |>
    req_url_query(fq = args[["local_types"]]) |>
    req_user_agent(args[["user_agent"]]) |>
    req_perform()
}
