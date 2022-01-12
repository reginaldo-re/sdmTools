test_that("Trying to create an invalid objects SDM_area.", {
  expect_warning("123" %>% areas_gt(1))
})

test_that("Removing a single area from a SpatialPolygons study area.", {
  new_SPDF <- SP %>%
    as("SpatialPolygonsDataFrame")

  new_SPDF@data <- list(
    area=c(
      SP[1] %>% gArea(),
      SP[2] %>% gArea(),
      SP[3] %>% gArea()
    )
  ) %>%
    as.data.frame()

  new_area <- new_SPDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    areas_gt(0.25, new_name = "Test area")

  expect_equal(new_area$study_area$area %>% sum() %>% round(2), 0.98)
  expect_equal(new_area$study_area %>% gArea() %>% round(2), 1.09)
  expect_string(new_area$name, "Test area")
})

test_that("Removing no areas from a SpatialPolygons study area.", {
  new_SPDF <- SP %>%
    as("SpatialPolygonsDataFrame")

  new_SPDF@data <- list(
    area=c(
      SP[1] %>% gArea(),
      SP[2] %>% gArea(),
      SP[3] %>% gArea()
    )
  ) %>%
    as.data.frame()

  new_area <- new_SPDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    areas_gt(0.1)

  expect_equal(new_area$study_area$area %>% sum() %>% round(2), 1.23)
  expect_equal(new_area$study_area %>% gArea() %>% round(2), 1.34)
})

test_that("Removing all areas from a SpatialPolygons study area.", {
  new_area <- SP %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    areas_gt(20)

  expect_error(gArea(new_area$study_area))
})

test_that("Removing no areas from study area using SpatialPolygonsDataframe.", {
  new_area <- SP %>%
    as("SpatialPolygonsDataFrame") %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    areas_gt(0.1)

  expect_equal(new_area$study_area %>% gArea() %>% round(2), 1.34)
})
