#' Build a basemap from the Ordnance Survey Maps API
#'
#' @inheritParams generate_png_data
#' @param ... Other details to pass on to generate_png_data()
#'
#' @returns A geospatially-referenced raster... hopefully.
#' @export
#'
#' @examples
#' oxf <- create_bbox("Oxford", 27700)
#' oxford_basemap <- build_basemap(oxf, zoom = 5, style = "road", crs = 27700)
#'
#' tmap::tm_shape(oxford_basemap, raster.downsample = FALSE) +
#'   tmap::tm_rgb(max.value = 1)
build_basemap <- function(bbox, zoom, crs = c(27700, 3857), ...) {

  assert_that(length(bbox) == 4)
  assert_that(all(is.numeric(bbox)))
  assert_that(crs %in% c(27700, 3857))

  raster_data <- generate_png_data(bbox = bbox, zoom = zoom, crs = crs, ...)

  png_data <- raster_data[[1]]
  extents <- raster_data[[2]]

  to_rast <- function(png, extent, crs) {
    out <- png |>
      terra::rast()
    terra::set.ext(out, extent)
    terra::set.crs(out, paste0("epsg:", crs))
    out
  }


  raster_list <- png_data |>
    purrr::map2(extents, \(x, y) to_rast(x, y, crs = crs))
  collection <- terra::sprc(raster_list)


  # return
  terra::merge(collection)
}
