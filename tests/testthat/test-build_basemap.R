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
    squarify = FALSE,
    chatty = FALSE)
    
  png_data <- raster_data[[1]]
  extents <- raster_data[[2]]

  expect_equal(length(png_data), length(extents))

  raster_1 <- png_data[[1]] |>
    terra::rast() |>
    terra::set.ext(extents[[1]]) |>
    terra::set.crs(paste0("epsg:", crs))

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
    squarify = FALSE,
    chatty = FALSE)
    
  png_data <- raster_data[[1]]
  extents <- raster_data[[2]]
  
  to_rast <- function(png, extent, crs) {
    png |>
      terra::rast() |>
      terra::set.ext(extent) |>
      terra::set.crs(paste0("epsg:", crs))
  }

  raster_list <- purrr::map2(png_data, extents, to_rast, crs = crs)

  collection <- terra::sprc(raster_list)
  expect_s4_class(collection, "SpatRasterCollection")

  merged <- terra::merge(collection)
  terra::writeRaster(merged, filename = "collection.tiff", overwrite = TRUE)
})
