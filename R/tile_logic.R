tile_logic_27700 <- function(x, zoom, type = c("easting", "northing")) {
  if (type == "easting") {

    # find number of tiles across the BNG area, at this zoom level,
    # ... from my "trial and error" data table (included as internal data
    # in this pkg - see grid_tiles.R)
    number_of_tiles_across <- grid_tiles_27700 %>%
      dplyr::filter(.data$zoom == .env$zoom) %>%
      dplyr::pull(.data$x)

    # How far (%) across (west to east) the BNG is this particular easting?
    # (7e5 is the number of metres wide the BNG is).
    # Value needs to be >= 0 and <= 1.
    # Previous assertion checks on the value of x should ensure this is true.
    pct_across <- x / 7e5

    # what tile number (column number) do we allocate this easting to, then?
    tile_number <- if (pct_across < 1) {
      # In nearly all cases, pct_across will be less than 1, and we will get a
      # valid tile number by multiplying by n and then using floor() to round down.
      # Even if pct_across is zero.
      floor(pct_across * number_of_tiles_across)
    } else {
      # if the easting is right at the eastern edge then floor() won't work,
      # we need to force the return of the easternmost tile column
      # (n-1, as tile numbering starts at zero). TBH this process is slightly
      # overkill; if we asked for the non-existent tile column beyond the
      # eastern edge, we would just get a NULL response and no harm done.
      number_of_tiles_across - 1
    }
  }

  if (type == "northing") {

    # find number of tiles up/down the BNG area, at this zoom level,
    # ... from my "trial and error" data table (included as internal data
    # in this pkg - see grid_tiles.R)
    number_of_tiles_down <- grid_tiles_27700 %>%
      dplyr::filter(.data$zoom == .env$zoom) %>%
      dplyr::pull(.data$y)

    # How far (%) down the BNG is this northing value? Pcts start at the northern
    # edge of the grid (x = 1.3e6, would give 0.000% which is correct.)
    # (1.3e6 is the number of metres high the BNG is).
    # Value needs to be >= 0 and <= 1.
    # Previous assertion checks on the value of x should ensure this is true.
    pct_down <- (1.3e6 - x) / 1.3e6

    # what tile number (row number) do we allocate this northing to, then?
    tile_number <- if (pct_down < 1) {
      # In nearly all cases, pct_down will be less than 1, and we will get a
      # valid tile number by multiplying by n and then using floor() to round down.
      # Even if pct_down is zero.
      floor(pct_down * number_of_tiles_down)
    } else {
      # if the northing is right at the southern edge then floor() won't work,
      # we need to force the return of the southernmost tile row
      # (n-1, as tile numbering starts at zero). TBH this process is slightly
      # overkill; if we asked for the non-existent tile row beyond the
      # southern edge, we would just get a NULL response, and no harm done.
      number_of_tiles_down - 1
    }
  }

  # return
  tile_number
}



tile_logic_3857 <- function(x, zoom, type = c("easting", "northing")) {

  # https://epsg.io/3857
  # 40052753 = length of equator in m
  # 40097932 = width / height of EPSG:3857 map in m
  # To make things work here, we take the mean of these two,
  # and we don't ask any difficult questions about why this works.
  equator <- mean(c(40052753, 40097932))

  if (type == "easting") {
    # How far (%) across (west to east) the world is this particular easting?
    # x is relative to Greenwich so we have to add it to (equator / 2) first.
    pct_across <- ((equator / 2) + x) / equator
    # We can simply floor() the % figure times the number of tile columns
    # at this zoom level. Ignore the vv slight chance that pct_across == 1.
    tile_number <- floor(pct_across * 2^zoom)
  }
  if (type == "northing") {
    # How far (%) down (north to south) the world is this particular easting?
    # x is a northing so we have to subtract it from the distance from the
    # equator to the top of the WGS84 bounds first, thereby turning it into a
    # distance in the southerly direction from the top.
    pct_down <- ((equator / 2) - x) / equator
    # We can simply floor() the % figure times the number of tile rows
    # at this zoom level. Ignore the vv slight chance that pct_down == 1.
    tile_number <- floor(pct_down * 2^zoom)
  }

  # return
  tile_number
}
