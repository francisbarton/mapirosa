tile_logic_27700 <- function(bbox, zoom, tile_size) {

  # Here's the trick! False origins!
  # https://github.com/OrdnanceSurvey/tile-name-derivation
  false_origin_x <- -238375.0
  false_origin_y <- 1376256.0

  west_col <- floor((bbox[["xmin"]] - false_origin_x) / tile_size)
  east_col <- floor((bbox[["xmax"]] - false_origin_x) / tile_size)
  north_row <- floor((false_origin_y - bbox[["ymax"]]) / tile_size)
  south_row <- floor((false_origin_y - bbox[["ymin"]]) / tile_size)

  # return
  c(west_col, east_col, north_row, south_row)
}



tile_logic_3857 <- function(bbox, zoom, equator) {

  # Note that the extent of the 3857 tile matrix is square,
  # ie its north-south height is the same as its east-west width (~ the length
  # of the equator). Hence `equator` can be used in n-s calculations as well.

  # How far (%) across (west to east) the world is a particular easting?
  # x is relative to Greenwich so we have to add it to (equator / 2) first.
  pct_across <- function(x) ((equator / 2) + x) / equator

  # How far (%) down (north to south) the world is a particular northing?
  # x is a northing so we have to subtract it from the distance from the
  # equator to the top of the WGS84 bounds first, thereby turning it into a
  # distance in the southerly direction from the top.
  pct_down <- function(x) ((equator / 2) - x) / equator

  west_col <- floor(pct_across(bbox[["xmin"]]) * 2^zoom)
  east_col <- floor(pct_across(bbox[["xmax"]]) * 2^zoom)
  north_row <- floor(pct_down(bbox[["ymax"]]) * 2^zoom)
  south_row <- floor(pct_down(bbox[["ymin"]]) * 2^zoom)

  # return
  c(west_col, east_col, north_row, south_row)
}
