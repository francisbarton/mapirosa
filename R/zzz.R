.onLoad <- function(lib, pkg) {
  safely_query_maps_api <<- purrr::safely(query_maps_api)
  possibly_query_maps_api <<- purrr::possibly(query_maps_api, NULL)
}
