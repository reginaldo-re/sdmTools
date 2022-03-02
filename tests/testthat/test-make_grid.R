test_that("Making a grid over study area (SpatialPolygonsDataframe) removing all variables.", {
  gridded_area <- SPDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid(var_names = list(), new_name = T)

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_true(((gridded_area$study_area@data %>% names()) == c("cell_id", "x_centroid", "y_centroid")) %>% all())
  expect_string(gridded_area$name, "Test area_grid")
})

test_that("Making a grid over study area (SpatialPolygonsDataframe) with all variables.", {
  gridded_area <- SPDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid(new_name = "test_area_grid")

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_equal(gridded_area$study_area@data$geocodigo %>% mean() %>% round(2), 12.10)
  expect_string(gridded_area$name, "test_area_grid")
})


test_that("Making a grid over study area (SpatialPolygonsDataframe) with one variable.", {
  gridded_area <- SPDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid(var_names = list("geocodigo"))

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_equal(gridded_area$study_area@data$geocodigo %>% mean() %>% round(2), 12.10)
})


test_that("Making a grid over study area (SpatialPolygons).", {
  gridded_area <- SPDF %>%
    as("SpatialPolygons") %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
})

test_that("Making a grid over study area (SpatialLinesDataframe) removing all variables.", {
  gridded_area <- SLDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid(var_names = list())

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_true(((gridded_area$study_area@data %>% names()) == list("cell_id", "x_centroid", "y_centroid")) %>% all())
})

test_that("Making a grid over study area (SpatialLinesDataframe) with all variables.", {
  gridded_area <- SLDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_equal(gridded_area$study_area@data$LENGTH_ %>% mean() %>% round(2), 3.79)
})

test_that("Making a grid over study area (SpatialLinesDataframe) with one variable.", {
  gridded_area <- SLDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid(var_names = list("Length", "xxx", "Main_ri"))

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_equal(gridded_area$study_area@data$Length %>% mean() %>% round(2), 3.79)
})


test_that("Making a grid over study area (SpatialLinesDataframe) with no data", {
  sl1 = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
  crs(sl1) <- CRS("EPSG:6933")
  new_area <- sl1 %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_s3_class(new_area, "SDM_area")
})


test_that("Making a grid over SDM_area.", {
  gridded_area <- SPDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
})

test_that("Trying to make a grid over an already gridded SDM_area object.", {
  expect_warning(a_sdm_area_gridded_area %>% make_grid())
})
