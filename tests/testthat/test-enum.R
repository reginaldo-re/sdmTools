test_that("It creates an enum.", {
  a_enum <- create_enum(dummy, cell_id, x_centroid, y_centroid)

  expect_true(
    ((a_enum %>% enum_as_vector() == c("dummy", "cell_id", "x_centroid", "y_centroid")) %>% all())
  )
})

test_that("It tries to create an invalid enum.", {
  expect_error(create_enum(dummy, cell_id, x_centroid, y_centroid, dummy))
})

test_that("It tries to convert to vector a non enumeration object.", {
  expect_error(c("aaa") %>% enum_as_vector())
})


test_that("It creates ATTR_CONTROL_NAMES global constant.", {
  expect_true(
    ((ATTR_CONTROL_NAMES %>% enum_as_vector() == c("dummy", "cell_id", "x_centroid", "y_centroid")) %>% all())
  )
})

test_that("It creates RASTER_FORMATS_EXT global constant.", {
  expect_true(
    ((RASTER_FORMATS_EXT %>% enum_as_vector() == c("grd", "asc", "sdat", "rst", "nc", "tif", "envi", "bil", "img")) %>% all())
  )
})

