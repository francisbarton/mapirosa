#' Internal check for a valid combination of CRS, map type and zoom level
#'
#' @inheritParams build_basemap
#' @returns No visible return unless an error is thrown
check_zoom <- function(
    zoom,
    type = c("outdoor", "road", "light", "leisure"),
    crs = c(27700, 3857),
    allow_premium = FALSE,
    chatty = NULL
    ) {

  tier <- "Free"
  invalid_msg <- "Invalid combination of CRS + map type + zoom level."

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

  # return
  invisible(all(!is.null(c(zoom, type, crs, tier))))
}
