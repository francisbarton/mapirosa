# nailsworth_27700 <- osmdata::getbb(
#   "Nailsworth",
#   limit = 1,
#   format_out = "sf_polygon") |>
#   sf::st_transform(crs = 27700)
# nailsworth_3857 <- osmdata::getbb(
#   "Nailsworth",
#   limit = 1,
#   format_out = "sf_polygon") |>
#   sf::st_transform(crs = 3857)


# nails_bbox_27700 <- nailsworth_27700 |>
#   sf::st_bbox()


"initial" |>
  test_that({

  nails_bbox_27700 <- structure(c(
    xmin = 382036.196778455, ymin = 198166.20158993,
    xmax = 386279.599388408, ymax = 201155.90450143
  ))

  nails_bbox_3857 <- osmdata::getbb(
    "Nailsworth",
    limit = 1,
    format_out = "sf_polygon") |>
    sf::st_transform(crs = 3857) |>
    sf::st_bbox()

  raster_data <- generate_png_data(
    bbox = nails_bbox_3857,
    zoom = 12,
    style = "road",
    crs = 3857,
    squarify = FALSE
  )

  png_data <- raster_data[[1]]
  extents <- raster_data[[2]]

  expect_equal(length(png_data), length(extents))

  raster_1 <- png_data[[1]] |>
    terra::rast()

  terra::set.ext(raster_1, extents[[1]])
  terra::set.crs(raster_1, paste0("epsg:", 3857))

  expect_s4_class(raster_1, "SpatRaster")

  ext_1 <- terra::ext(raster_1) |>
    as.vector()
  expect_equal(ext_1, extents[[1]], ignore_attr = TRUE)
  })

"combine" |>
  test_that({

  nails_bbox_3857 <- osmdata::getbb(
    "Nailsworth",
    limit = 1,
    format_out = "sf_polygon") |>
    sf::st_transform(crs = 3857) |>
    sf::st_bbox()

  raster_data <- generate_png_data(
    bbox = nails_bbox_3857,
    zoom = 12,
    style = "road",
    crs = 3857,
    squarify = FALSE
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

  raster_list <- purrr::map2(png_data, extents, to_rast, crs = 3857)

  collection <- terra::sprc(raster_list)
  expect_s4_class(collection, "SpatRasterCollection")

  merged <- terra::merge(collection)
  terra::writeRaster(merged, filename = "collection.tiff", overwrite = TRUE)
})
