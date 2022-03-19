test_that("Making a grid over study area (SpatialPolygonsDataframe) removing all variables.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  gridded_area <- SPDF %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid(
      var_names = list(),
      new_name = "New name",
      dir_path = a_dir
    )


  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_true(gridded_area$gridded)
  expect_true(((gridded_area$study_area@data %>% names()) == c("cell_id", "x_centroid", "y_centroid")) %>% all())
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "New name")
  checkmate::expect_file_exists(a_dir %>% fs::path("New name.gpkg"))

  a_dir %>%
    fs::dir_delete()
})

test_that("Making a grid over study area (SpatialPolygonsDataframe) with all variables.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  gridded_area <- SPDF %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid(
      new_name = "New name",
      dir_path = a_dir
    )

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_equal(gridded_area$study_area@data$geocodigo %>% mean() %>% round(2), 11.79)
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "New name")
  checkmate::expect_file_exists(a_dir %>% fs::path("New name.gpkg"))

  a_dir %>%
    fs::dir_delete()
})


test_that("Making a grid over study area (SpatialPolygonsDataframe) with one variable.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  gridded_area <- SPDF %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid(
      var_names = list("geocodigo"),
      dir_path = a_dir
    )

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_equal(gridded_area$study_area@data$geocodigo %>% mean() %>% round(2), 11.79)
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "Test area")
  checkmate::expect_file_exists(a_dir %>% fs::path("Test area.gpkg"))

  a_dir %>%
    fs::dir_delete()
})


test_that("Making a grid over study area (SpatialPolygons).", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  gridded_area <- SPDF %>%
    as("SpatialPolygons") %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "Test area")
  checkmate::expect_file_exists(a_dir %>% fs::path("Test area.gpkg"))


  a_dir %>%
    fs::dir_delete()
})

test_that("Making a grid over study area (SpatialLinesDataframe) removing all variables.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  gridded_area <- SLDF %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid(var_names = list())

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_true(((gridded_area$study_area@data %>% names()) == list("cell_id", "x_centroid", "y_centroid")) %>% all())
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "Test area")
  checkmate::expect_file_exists(a_dir %>% fs::path("Test area.gpkg"))


  a_dir %>%
    fs::dir_delete()
})

test_that("Making a grid over study area (SpatialLinesDataframe) with all variables.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  gridded_area <- SLDF %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_equal(gridded_area$study_area@data$LENGTH_ %>% mean() %>% round(2), 3.79)
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "Test area")
  checkmate::expect_file_exists(a_dir %>% fs::path("Test area.gpkg"))

  a_dir %>%
    fs::dir_delete()
})

test_that("Making a grid over study area (SpatialLinesDataframe) with one variable.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  gridded_area <- SLDF %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid(
      var_names = list("Length", "xxx", "Main_ri")
    )

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_equal(gridded_area$study_area@data$Length %>% mean() %>% round(2), 3.79)
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "Test area")
  checkmate::expect_file_exists(a_dir %>% fs::path("Test area.gpkg"))

  a_dir %>%
    fs::dir_delete()
})


test_that("Making a grid over study area (SpatialLinesDataframe) with no data", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  sl1 = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
  crs(sl1) <- CRS("EPSG:6933")
  new_area <- sl1 %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid()

  expect_s3_class(new_area, "SDM_area")
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "Test area")
  checkmate::expect_file_exists(a_dir %>% fs::path("Test area.gpkg"))

  a_dir %>%
    fs::dir_delete()
})


test_that("Making a grid over SDM_area.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  gridded_area <- SPDF %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  checkmate::expect_string(gridded_area$sdm_area_name, fixed = "Test area")
  checkmate::expect_file_exists(a_dir %>% fs::path("Test area.gpkg"))

  a_dir %>%
    fs::dir_delete()
})

test_that("Trying to make a grid over an already gridded SDM_area object.", {
  expect_warning(a_sdm_area_gridded_area %>% make_grid())
})
