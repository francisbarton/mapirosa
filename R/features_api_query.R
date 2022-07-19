#' Retrieve data from the OS Features API
#' See https://osdatahub.os.uk/docs/wfs/technicalSpecification for details.
#'
#' @param x The name of a place, or a postcode for example. The API docs say
#'   'A free string text search of OS Names, intended to be an ambiguous/fuzzy
#'   search.'
#' @param max_results How many results to return. Can be from 1-100. Set to 1 by
#'   default.
#' @param area The bounding box of an area to limit search to. NULL by default -
#'   no filtering by area.
#' @param local_types Types of result to return. Can be a vector of strings.
#'   Examples are 'City', 'Village'. See API docs for all options.
#'   NULL by default (no filtering by type).
#' @param version API version
#' @param user_agent Currently NULL by default.
#'
#' @return A raw response from the API endpoint - you will want to process this.
#' @export
features_api_query <- function(x, version = "2.0.0", user_agent = NULL) {


  os_base_url <- "https://api.os.uk/features/v1/wfs"

  # TODO implement proper process for key mgmt/user supply via {httr2} in time
  # this will to do for now...
  os_data_key <- Sys.getenv("OS_DATA_KEY")

  # replace UA string with package name once made into public package
  default_user_agent <- "Francis Barton fbarton@alwaysdata.net"
  user_agent <- match.arg(user_agent, choices = default_user_agent)
  version <- match.arg(version, choices = c("2.0.0", "1.1.0", "1.0.0"))

  get_capabilities <- function() {
    req <- httr2::request(os_base_url)
    resp <- req %>%
      httr2::req_url_query(service = "wfs") %>%
      httr2::req_url_query(request = "getcapabilities") %>%
      httr2::req_url_query(key = os_data_key) %>%
      httr2::req_perform()
    httr2::resp_check_status(resp)

    # convert API response to XML and return an R list object
    httr2::resp_body_xml(resp) %>%
      XML::xmlParse() %>%
      XML::xmlToList()
  }

  capabilities <- get_capabilities()
  feature_types <- capabilities[["FeatureTypeList"]] %>%
    purrr::map_chr("Title")

  return_feature_desc <- function() {}

}
