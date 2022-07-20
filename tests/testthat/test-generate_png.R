equator <- 40052753 # length of equator in m
equator <- 40097932 # width / height of EPSG:3857 map in m

zoom <- 12

xmin_tile <- tile_logic_3857(nails_bbox_3857[["xmin"]], zoom, "longitude")
xmax_tile <- tile_logic_3857(nails_bbox_3857[["xmax"]], zoom, "longitude")
ymin_tile <- tile_logic_3857(nails_bbox_3857[["ymin"]], zoom, "latitude")
ymax_tile <- tile_logic_3857(nails_bbox_3857[["ymax"]], zoom, "latitude")


grid <- tidyr::expand_grid(x = seq(xmin_tile, xmax_tile), y = seq(ymin_tile, ymax_tile))

tile_side <- equator / 2^zoom

extents <- grid %>%
  dplyr::mutate(xmin = (.env$tile_side * .data$x) - (.env$equator / 2)) %>%
  dplyr::mutate(xmax = (.env$tile_side * (.data$x + 1)) - (.env$equator / 2)) %>%
  dplyr::mutate(ymin = (.env$equator / 2) - (.env$tile_side * (.data$y + 1))) %>%
  dplyr::mutate(ymax = (.env$equator / 2) - (.env$tile_side * .data$y)) %>%
  dplyr::select(.data$xmin:.data$ymax) %>%
  dplyr::rowwise() %>%
  dplyr::group_split() %>%
  purrr::map(as.vector) %>%
  purrr::map(unlist)



nailsworth_27700 <- osmdata::getbb("Nailsworth", limit = 1, format_out = "sf_polygon") |>
  sf::st_transform(crs = 27700)
nails_bbox_27700 <- structure(c(xmin = 382036, ymin = 198166,
                                xmax = 386280, ymax = 201156))

"check extents 1" %>%
  test_that({
    expect_equal(nrow(grid), length(extents))
    expect_length(extents[[1]], 4)
  })

"check extents 2" %>%
  test_that({
    expect_equal(extents[[1]][["xmax"]], extents[[1]][["xmin"]] + tile_side)
    expect_equal(extents[[2]][["ymax"]], extents[[2]][["ymin"]] + tile_side)
  })

"tile width" %>%
  test_that({
    zoom <- 4

    tile_side <- grid_tiles_27700 %>%
      dplyr::filter(.data$zoom == .env$zoom) %>%
      dplyr::pull(.data$res) %>%
      `*`(256)

    expect_equal(tile_side, 14336)
  })

"tile logix" %>%
  test_that({
    zoom <- 4
    tile_side <- 14336
    xmin_tile <- tile_logic_27700(nails_bbox_27700[["xmin"]], zoom, "easting")
    xmax_tile <- tile_logic_27700(nails_bbox_27700[["xmax"]], zoom, "easting")
    ymin_tile <- tile_logic_27700(nails_bbox_27700[["ymin"]], zoom, "northing")
    ymax_tile <- tile_logic_27700(nails_bbox_27700[["ymax"]], zoom, "northing")

    grid <- tidyr::expand_grid(x = seq(xmin_tile, xmax_tile), y = seq(ymin_tile, ymax_tile))

    png_data <- grid %>%
      purrr::pmap(read_png, zoom = zoom, type = "road", crs = 27700)

    expect_equal(nrow(grid), length(png_data))
  })


# test process stepwise

crs <- 3857
zoom <- 12
type <- "road"
nailsworth_3857 <- osmdata::getbb("Nailsworth", limit = 1, format_out = "sf_polygon") |>
  sf::st_transform(crs = 3857)

# nails_bbox_3857 <- nailsworth_3857 |>
#   sf::st_bbox()
nails_bbox_3857 <- structure(c(xmin = -251732, ymin = 6742840,
                               xmax = -244891, ymax = 6747653))

xmin_tile <- tile_logic_3857(nails_bbox_3857[["xmin"]], zoom, "longitude")
xmax_tile <- tile_logic_3857(nails_bbox_3857[["xmax"]], zoom, "longitude")
ymin_tile <- tile_logic_3857(nails_bbox_3857[["ymin"]], zoom, "latitude")
ymax_tile <- tile_logic_3857(nails_bbox_3857[["ymax"]], zoom, "latitude")

sqt <- squarify(xmin_tile, xmax_tile, ymin_tile, ymax_tile)

grid <- tidyr::expand_grid(x = seq(sqt[1], sqt[2]), y = seq(sqt[3], sqt[4]))

raster_data <- generate_png_3857(bbox = nails_bbox_3857, zoom = 12, type = "road")

png_data <- raster_data[[1]]
extents <- raster_data[[2]]


to_rast <- function(png, extent, crs) {
  png %>%
    terra::rast() %>%
    terra::set.ext(extent) %>%
    terra::set.crs(value = paste0("epsg:", crs))
}

raster_list <- purrr::map2(png_data, extents, to_rast, crs = crs)
collection <- terra::sprc(raster_list)
expect_s4_class(collection, "SpatRasterCollection")

merged <- terra::merge(collection)

tmap::tmap_mode("view")
tmap::tm_shape(merged, raster.downsample = FALSE) +
  tmap::tm_rgb(max.value = 1) +
  tmap::tm_shape(nailsworth_3857) +
  tmap::tm_polygons(border.col = "orange", lwd = 2, alpha = 0.3)
