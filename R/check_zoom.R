#' Internal check for a valid combination of CRS, map style and zoom level
#'
#' @inheritParams build_basemap
#' @returns No visible return unless an error is thrown
check_zoom <- function(
    zoom,
    style = c("outdoor", "road", "light", "leisure"),
    crs = c(27700, 3857),
    allow_premium = FALSE
    ) {

  tier <- "Free"

  if (crs == 27700) {

    assert_that(zoom %in% 0:13,
                msg = "Zoom level must be an integer between 0 and 13 (inclusive) when using CRS 27700.")

    if (!style == "leisure") {
      if (!allow_premium) assert_that(zoom %in% 0:9,
                       msg = "Zoom levels 10 to 13 are only available as Premium data. Set `allow_premium` to TRUE to use these levels.")
      if (zoom %in% 10:13) tier <- "Premium"
    }

    if (style == "leisure") {
      assert_that(zoom %in% 0:9,
                  msg = "Zoom levels 10 to 13 are not available in the Leisure style.")
      if (!allow_premium) assert_that(zoom %in% 0:5,
                       msg = "Zoom levels 6 to 9 in the Leisure style are only available as Premium data. Set `allow_premium` to TRUE to use these levels.")
      if (zoom %in% 6:9) tier <- "Premium"
    }
  }

  if (crs == 3857) {

    assert_that(zoom %in% 7:20,
                msg = "Zoom level must be an integer between 7 and 20 (inclusive) when using CRS 27700.")

    assert_that(!style == "leisure",
                msg = "Leisure map style is not available in CRS 3857.")

    if (!allow_premium) assert_that(zoom %in% 7:16,
                     msg = "Zoom levels 17 to 20 are only available as Premium data. Set `allow_premium` to TRUE to use these levels.")
    if (zoom %in% 17:20) tier <- "Premium"
  }

  if (rlang::is_interactive()) {
    ui_info(paste("Zoom level:", zoom))
    ui_info(paste("Map style:", stringr::str_to_sentence(style)))
    ui_info(paste("CRS:", crs))
    ui_info(paste("Data tier:", tier))
  }

  # return
  invisible(all(!is.null(c(zoom, style, crs, tier))))
}
