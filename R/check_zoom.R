#' An internal check function to ensure that a valid combination of CRS, map type and zoom level has been chosen
#'
#' @title check_zoom
#' @inheritParams build_basemap
#' @param allow_premium Whether to only access tiles from zoom levels that are within the "OpenData" tiers of the OS API. These vary according to map type and CRS. See https://osdatahub.os.uk/docs/wmts/technicalSpecification for details. Defaults to FALSE. Set to TRUE if you wish to access higher zoom levels within the "Premium" data tiers (chargeable - see https://osdatahub.os.uk/plans)
#' @param chatty Provide a brief info summary of the parameters that will be used. Requires the `usethis` package to be installed. Defaults to FALSE.
#'
#' @return no visible return unless an error is thrown
check_zoom <- function(zoom, type, crs, allow_premium = FALSE, chatty = FALSE) {

  assert_that(crs %in% c(27700, 3857))
  tier <- "Free"

  invalid_msg <- "Invalid combination of CRS / map type / zoom level."

  if (crs == 27700) {
    if (!type == "leisure") {
      if (allow_premium) assert_that(zoom %in% 0:13,
                                     msg = invalid_msg)
      else assert_that(zoom %in% 0:9,
                       msg = invalid_msg)
      if (zoom %in% 10:13) tier <- "Premium"
    } else {
      if (allow_premium) assert_that(zoom %in% 0:9,
                                     msg = invalid_msg)
      else assert_that(zoom %in% 0:5,
                       msg = invalid_msg)
      if (zoom %in% 6:9) tier <- "Premium"
    }
  }

  if (crs == 3857) {
    assert_that(!type == "leisure",
                msg = "Leisure map style is not available in CRS 3857.")
    if (allow_premium) assert_that(zoom %in% 7:20,
                                   msg = invalid_msg)
    else assert_that(zoom %in% 7:16,
                     msg = invalid_msg)
    if (zoom %in% 17:20) tier <- "Premium"
  }

  if (chatty & requireNamespace("usethis")) {
    ui_info(paste("Zoom level:", zoom))
    ui_info(paste("Map type:", stringr::str_to_sentence(type)))
    ui_info(paste("CRS:", crs))
    ui_info(paste("Data tier:", tier))
  }

  invisible(all(!is.null(c(zoom, type, crs, tier))))
}
