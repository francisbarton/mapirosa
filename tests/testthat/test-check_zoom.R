# zero is fine for 27700
testthat::expect_true(
  check_zoom(0, "light", 27700)
)
# 0:6 are not OK for 3857
testthat::expect_error(
  check_zoom(6, "light", 3857)
)
# but 7:16 are fine
testthat::expect_true(
  check_zoom(7, "light", 3857)
)
testthat::expect_true(
  check_zoom(16, "light", 3857)
)
# 20 is not OK wih 3857...
testthat::expect_error(
  check_zoom(20, "light", 3857)
)
# ... unless free_only is turned off
testthat::expect_true(
  check_zoom(20, "light", 3857, free_only = FALSE)
)

# the return should be invisible (and also TRUE)
testthat::expect_invisible(
  check_zoom(7, "light", 3857)
)
# leisure is not compatible with 3857
testthat::expect_error(
  check_zoom(7, "leisure", 3857)
)
# the other three are fine though
testthat::expect_true(
  check_zoom(7, "outdoor", 3857)
)

# with 27700 and leisure, 6 is not compatible with free_only
testthat::expect_error(
  check_zoom(6, "leisure", 27700)
)
testthat::expect_true(
  check_zoom(6, "leisure", 27700, free_only = FALSE)
)

# LIGHT should be converted to lower case and therefore be OK
testthat::expect_true(
  check_zoom(6, "LIGHT", 27700)
)


# 3856 is not a valid crs code
testthat::expect_error(
  check_zoom(6, "light", 3856)
)
# streets is not a valid map type
testthat::expect_error(
  check_zoom(6, "streets", 3857)
)

# what happens if you don't specify a type or a crs?
testthat::expect_true(
  check_zoom(9)
)
testthat::expect_true(
  check_zoom(10, crs = 3857)
)
testthat::expect_true(
  check_zoom(9, type = "leisure", free_only = FALSE)
)
testthat::expect_error(
  check_zoom(9, type = "leisure")
)
testthat::expect_error(
  check_zoom(10)
)
