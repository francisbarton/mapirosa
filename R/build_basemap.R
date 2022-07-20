#' Build a basemap from the Ordnance Survey Maps API
#'
#' @title build_basemap
#' @param bbox A bbox object (a length 4 vector with names xmin, ymin, xmax, ymax)
#' @param zoom Zoom level, an integer
#' @param type Map type, a string. One of "Road", "Outdoor", "Light", "Leisure".
#' @param crs CRS code (EPSG), an integer, either 27700 or 3857. 27700 (British National Grid) by default.
#' @param aspect_ratio Ratio of number of tiles returned, x:y. NULL by default, meaning that no adjustment will be made to the basemap made from the bbox. An aspect ratio of 1 means that it will try to return a square basemap. It will do this by widening a "portrait" aspect basemap by adding more columns, or by increasing the number of rows of tiles if the original is "landscape" format. Use with care, as it will add extra tiles, so large adjustments may result in a lot more data being downloaded and a larger basemap image being generated.
#' @param expand_dir A character vector of directions, which dictate in which direction a set of basemap tiles will be stretched, if aspect_ratio is set and causes a change to the original set of tiles. Defaults to c("eastwards", "northwards") which means that new columns of tiles will by default be added to the eastern edge of the basemap, or new rows of tiles will be added to the northern edge of the basemap, if aspect adjustments are made. Use "southwards" or "westwards" to cause the opposite behaviour.
#' @param cache_tiles Instead of converting PNG data from the API into a basemap on the fly, cache data as local PNG files. Not functional yet; defaults to FALSE.
#' @inheritParams check_zoom
#'
#' @return A spatially-referenced raster, hopefully.
#' @export
build_basemap <- function(
    bbox,
    zoom,
    type = c("outdoor", "road", "light", "leisure"),
    crs = 27700,
    squarify = TRUE,
    cache_tiles = FALSE,
    allow_premium = FALSE,
    chatty = FALSE) {

  assert_that(length(bbox) == 4)
  assert_that(all(is.numeric(bbox)))

  type <- tolower(type)

  if (crs == 27700) {
    raster_data <- generate_png_27700(bbox = bbox, zoom = zoom, type = type, squarify = squarify, allow_premium = allow_premium, chatty = chatty)
  }

  if (crs == 3857) {
    raster_data <- generate_png_3857(bbox = bbox, zoom = zoom, type = type, squarify = squarify, allow_premium = allow_premium, chatty = chatty)
  }

  png_data <- raster_data[[1]]
  extents <- raster_data[[2]]

  # Copied from {xfun} - to save a dependency on that pkg
  # https://github.com/yihui/xfun/blob/main/R/paths.R#L403
  # dir_exists <- function(x) utils::file_test('-d', x)
  # dir_create <- function(x, recursive = TRUE, ...) {
  #   dir_exists(x) || dir.create(x, recursive = recursive, ...)
  # }

  # path <- file.path(download_dir, type, zoom)
  #
  # if (xfun::dir_create(path)) {
  #   outfile <- file.path(download_dir, type, zoom, stringr::str_glue("{x}_{y}.png"))
  # }

  # write PNG data to files
  # png::writePNG(target = outfile)

  to_rast <- function(png, extent, crs) {
    png %>%
      terra::rast() %>%
      terra::set.ext(extent) %>%
      terra::set.crs(value = paste0("epsg:", crs))
  }

  if (!cache_tiles) {
    raster_list <- purrr::map2(png_data, extents, to_rast, crs = crs)
    collection <- terra::sprc(raster_list)
    terra::merge(collection)
  }
}

