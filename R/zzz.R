.onLoad <- function(lib, pkg) {
  safely_query_maps_api <<- purrr::safely(query_maps_api)
}
