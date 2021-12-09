


grid_tiles <- dplyr::tribble(
  ~zoom, ~x, ~y,
  0,      4,     6,
  1,      9,    12,
  2,     19,    24,
  3,     38,    48,
  4,     79,    96,
  5,    158,   197,
  6,    317,   395,
  7,    635,   791,
  8,   1270,  1582,
  9,   2541,  3164,
  10,  5082,  6328,
  11, 10164, 12657,
  12, 20328, 25314,
  13, 40656, 50628
)

# BNG: 700km (E/x) by 1300km (N/y) ie 7*13 = 91 100km^2 squares

# convert easting/northing coordinates to tile x,y at given zoom

zoom <- 8
span_x <- grid_tiles %>%
  dplyr::filter(zoom == zoom) %>%
  dplyr::pull(x)
span_y <- grid_tiles %>%
  dplyr::filter(zoom == zoom) %>%
  dplyr::pull(y)

# test coords/bbox
xmin <- 448000
xmax <- 452000
ymin <- 248000
ymax <- 252000

xmin_tile <- floor(span_x*xmin/700000)
xmax_tile <- floor(span_x*xmax/700000)
# y tile numbers start from NW but BNG coords start from SW
ymin_tile <- floor(span_y*(700000 - ymin)/700000)
ymax_tile <- floor(span_y*(700000 - ymax)/700000)

# expand_grid(x = seq(xmin_tile, xmax_tile), y = seq(ymin_tile, ymax_tile)) %>%
#   # pwalk(os_map_tiles) # this (pwalk) was for writing *files*
#   purrr::pmap(os_map_tiles, zoom = zoom)  # sending PNG data instead
