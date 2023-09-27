#' Build a basemap from the Ordnance Survey Maps API
#'
#' @param bbox A bbox object (a length 4 vector with names xmin, ymin, xmax,
#' ymax)
#' @param zoom Zoom level, an integer. For `crs = 27700`, this must be between
#' 0 and 13. For `crs = 3857`, this must be between 7 and 20. Certain
#' combinations of zoom level and style retrieve tiles from the OS's Premium
#' service, which can generate a financial charge once past a certain level of
#' usage. See the OS Maps API webpages for details.
#' @param style Map style, a string. One of "road", "outdoor", "light",
#' "leisure". Leisure is only available in the 27700 CRS.
#' @param crs CRS code (EPSG), an integer, either `27700` (British National
#' Grid) or `3857` (standard web mapping projected coordinate system eg Google
#' Maps, OSM).
#' @param squarify Whether to add rows/columns to a basemap to make it square.
#' `TRUE` by default. If a bbox covers a set of basemap tiles that is wider
#' than it is high ("landscape"), `squarify` will add row(s) of tiles as
#' necessary to make the basemap square. For a portrait basemap (higher than it
#' is wide), `squarify` retrieves extra columns to make the basemap square.
#' @param squarify_to Where `squarify` adds an odd number of columns or rows,
#' one more will be added to one side than to the opposite side. In case you
#' have a preference which side should receive more, you can stipulate this
#' here. Needs a character vector. This is set to `c("south", "east")` by
#' default. If you know that `squarify` will definitely add columns, say, you
#' can stipulate a single string e.g. `"west"`. If you are not sure whether it
#' will add rows or columns, then enter a vector of length 2, e.g.
#' `c("north", "west")` depending on your preferences. Note that if 5 rows are
#' added, 3 will be added to your preferred direction, and 2 to the other side;
#' this parameter does not force all 5 rows to be added to your preferred side!
#' @param cache_tiles Instead of converting PNG data from the API into a
#' basemap on the fly, cache data as local PNG files. Not functional yet;
#' hence set to `FALSE`.
#' @param allow_premium Whether to only access tiles from zoom levels that are
#' within the "OpenData" tier of the OS API. These vary according to map style
#' and CRS. See the [API Technical Specification](https://osdatahub.os.uk/docs/wmts/technicalSpecification) for details. Defaults to FALSE. Set to TRUE if
#' you wish to access zoom levels within the "Premium" service tier (chargeable
#' - see [https://osdatahub.os.uk/plans](https://osdatahub.os.uk/plans))
#' @param debug Whether to show any errors that were received from the API.
#' This package should handle errors gracefully in general, but if your basemap
#' is not complete then you may wish to turn this on to see what errors there
#' might be.
#'
#' @returns A spatially-referenced raster, hopefully.
#' @export
#'
#' @examples
#' oxf <- create_bbox("Oxford", 27700)
#' oxford_basemap <- build_basemap(oxf, zoom = 5, style = "road", crs = 27700)
#' oxford_basemap
#'
#' tmap::tm_shape(oxford_basemap, raster.downsample = FALSE) +
#'   tmap::tm_rgb(max.value = 1)
#'
build_basemap <- function(
    bbox,
    zoom,
    style = c("outdoor", "road", "light", "leisure"),
    crs = c(27700, 3857),
    squarify = TRUE,
    squarify_to = c("south", "east"),
    cache_tiles = FALSE,
    allow_premium = FALSE,
    debug = FALSE
    ) {



  raster_data <- generate_png_data(
    bbox = bbox,
    zoom = zoom,
    style = style,
    crs = crs,
    squarify = squarify,
    squarify_to = squarify_to,
    allow_premium = allow_premium
  )

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
