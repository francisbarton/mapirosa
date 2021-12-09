#' Retrieve data from the OS Names API ('find' method currently, not 'nearest' yet)
#'
#' See https://osdatahub.os.uk/docs/names/technicalSpecification for details.
#'
#' @param x The name of a place, or a postcode for example. The API docs say 'A free
#'   string text search of OS Names, intended to be an ambiguous/fuzzy search.'
#' @param max_results How many results to return. Can be from 1-100. Set to 1 by
#'   default.
#' @param area The bounding box of an area to limit search to. NULL by default - no
#'   filtering by area.
#' @param local_types Types of result to return. Can be a vector of strings. Examples
#'   are 'City', 'Village'. See API docs for all options. NULL by default (no
#'   filtering by type).
#' @param user_agent Currently NULL by default.
#'
#' @return A raw response from the API endpoint - you will want to process this.
#' @export
names_api_query <- function(x, max_results = 1, area = NULL, local_types = NULL, user_agent = NULL) {

  # OS Names API
  # See https://osdatahub.os.uk/docs/names/technicalSpecification
  # for details

  # be good to wrap all this in a safely() or a possibly() eventually

  os_base_url <- "https://api.os.uk/search/names/v1/find"

  if (!is.null(local_types)) {
    local_types <- local_types %>%
      snakecase::to_any_case(
        case = "title",
        sep_out = "_",
        parsing_option = 1
      ) %>%
      paste0("LOCAL_TYPE:", .) %>%
      stringr::str_c(collapse = " ")
  }




  # TODO implement proper process for key mgmt/user supply via {httr2} in time
  # this will to do for now...
  os_data_key <- Sys.getenv("OS_DATA_KEY")


  default_user_agent <- "Francis Barton fbarton@alwaysdata.net"
  user_agent <- match.arg(user_agent, choices = default_user_agent)

  httr2::request(os_base_url) %>%
    # replace UA string with package name once made into public package
    httr2::req_user_agent(user_agent) %>%
    httr2::req_url_query(query = as.character(x)) %>%
    httr2::req_url_query(maxresults = max_results) %>%
    httr2::req_url_query(fq = local_types) %>%
    httr2::req_url_query(key = os_data_key) %>%
    httr2::req_perform()

  # places_vector %>%
  # purrr::map( ~ names_api_query(., local_types = c("city", "town")) %>%
  #               httr2::resp_body_json() %>%
  #               purrr::pluck("results", 1)
  # ) %>%
  #   purrr::map_df(1) %>%
  #   janitor::clean_names() %>%
  #   sf::st_as_sf(coords = c("geometry_x", "geometry_y"), crs = 27700)

}
