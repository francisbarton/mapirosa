#' Retrieve data from the OS Features API
#'
#' Function not yet completed and functional.
#' See https://osdatahub.os.uk/docs/wfs/technicalSpecification for details.
#'
#' @inheritParams query_names_api
#' @param version character. API version. Most recent (2.0.0) by default.
#'
#' @returns A raw response from the API endpoint - you will want to process
#'  this.
query_features_api <- function(
  x,
  version = c("2.0.0", "1.1.0", "1.0.0"),
  user_agent = NULL) {

  os_base_url <- "https://api.os.uk/features/v1/wfs"

  # TODO implement proper process for key mgmt/user supply via {httr2} in time
  # this will to do for now...
  os_data_key <- Sys.getenv("OS_DATA_KEY")

  # replace UA string with package name once made into public package
  default_ua <- "mapirosa R package https://github.com/francisbarton/mapirosa"
  user_agent <- ifelse(is.null(user_agent), default_ua, user_agent)
  version <- match.arg(version)

  get_capabilities <- function() {
    req <- request(os_base_url)
    resp <- req |>
      req_url_query(service = "wfs") |>
      req_url_query(request = "getcapabilities") |>
      req_url_query(key = os_data_key) |>
      req_perform()

    resp_check_status(resp)

    # convert API response to XML and return an R list object
    resp_body_xml(resp) |>
      xml2::as_list() |>
      purrr::pluck("WFS_Capabilities")
  }

  capabilities <- get_capabilities()
  feature_types <- capabilities |>
    purrr::pluck("FeatureTypeList") |>
    purrr::map("Title") |>
    purrr::list_flatten() |>
    purrr::list_c()

  return_feature_desc <- function() {}

}
