test_that("Removing a single area from a SpatialPolygons study area.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

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
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir) %>%
    keep_areas_gt(
      lower_bound = 0.25,
      new_name = "Test area",
      dir_path = a_dir
    )

  expect_equal(new_area$study_area$area %>% sum() %>% round(2), 0.98)
  expect_equal(new_area$study_area %>% gArea() %>% round(2), 0.98)
  expect_string(new_area$name, "Test area")

  a_dir %>%
    dir_delete()
})

test_that("Removing no areas from a SpatialPolygons study area.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

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
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    keep_areas_gt(
      lower_bound = 0.1,
      new_name = "Test area"
    )

  expect_equal(new_area$study_area$area %>% sum() %>% round(2), 1.23)
  expect_equal(new_area$study_area %>% gArea() %>% round(2), 1.23)

  a_dir %>%
    dir_delete()
})

test_that("Removing all areas from a SpatialPolygons study area.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

  expect_error(
    new_area <- SP %>%
      sdm_area(
        sdm_area_name = "Test area",
        epsg_code = "EPSG:6933",
        resolution = 50000,
        dir_path = a_dir
      ) %>%
      keep_areas_gt(
        lower_bound= 20
      )
  )

  a_dir %>%
    dir_delete()
})

test_that("Removing no areas from study area using SpatialPolygonsDataframe.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

  new_area <- SP %>%
    as("SpatialPolygonsDataFrame") %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    keep_areas_gt(0.1)

  expect_equal(new_area$study_area %>% gArea() %>% round(2), 1.23)

  a_dir %>%
    dir_delete()
})
