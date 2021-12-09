#' Check params are valid
#'
#' @title check_zoom
#' @param zoom zoom level, an integer
#' @param type map type, a string
#' @param crs CRS code (EPSG), an integer, either 27700 or 3857 (string is ok, arg will be coerced to string anyway)
#' @param free_only whether to only access tiles from zoom levels that are within the "OpenData" tiers of the OS API. These vary according to map type and CRS. See https://osdatahub.os.uk/docs/wmts/technicalSpecification for details. Defaults to TRUE. Set to FALSE if you wish to access higher zoom levels within the "Premium" data tiers (chargeable - see https://osdatahub.os.uk/plans)
#' @param chatty provide a brief info summary of the parameters that will be used. Requires the usethis package to be installed. Defaults to FALSE.
#'
#' @importFrom usethis ui_info
#' @return no visible return unless an error is thrown
check_zoom <- function(zoom, type = c("outdoor", "road", "light", "leisure"), crs = c(27700, 3857), free_only = TRUE, chatty = FALSE) {
  zoom <- as.integer(zoom)
  type <- tolower(type)
  type <- match.arg(type)
  crs <- as.character(crs)
  tier <- "Free"
  crs <- match.arg(crs, choices = as.character(c(27700, 3857)))
  if (crs == "27700") {
    if (!type == "leisure") {
      if (!free_only) assertthat::assert_that(zoom %in% 0:13)
      else assertthat::assert_that(zoom %in% 0:9)
      if (zoom %in% 10:13) tier <- "Premium"
    } else {
      if (!free_only) assertthat::assert_that(zoom %in% 0:9)
      else assertthat::assert_that(zoom %in% 0:5)
      if (zoom %in% 6:9) tier <- "Premium"
    }
  } else {
    assertthat::assert_that(!type == "leisure")
    if (!free_only) assertthat::assert_that(zoom %in% 7:20)
    else assertthat::assert_that(zoom %in% 7:16)
    if (zoom %in% 17:20) tier <- "Premium"
  }

  if (chatty & requireNamespace("usethis")) {
    ui_info(paste0("Zoom level: ", zoom))
    ui_info(paste0("Map type: ", type))
    ui_info(paste0("CRS: ", crs))
    ui_info(paste0("Data tier: ", tier))
  }

  invisible(all(!is.null(c(zoom, type, crs, tier))))
}
