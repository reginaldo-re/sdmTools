test_that("Trying to create an invalid objects SDM_area.", {
  expect_error(NULL %>% sdm_area("Test area"))
  expect_error(NULL %>% sdm_area("Test area", NULL))
  expect_error(NULL %>% sdm_area("Test area", NULL, NULL))
  expect_error(NULL %>% sdm_area("Test area", NULL, c(1,1)))
  expect_error(NULL %>% sdm_area("Test area", "EPSG:6933", c(1,1)))

  expect_error(SP %>% sdm_area("Test area"))
  expect_error(SP %>% sdm_area("Test area", "EPSG:XXXX"))
  expect_error(SP %>% sdm_area("Test area", "EPSG:6933"))
  expect_error(SP %>% sdm_area("Test area", "EPSG:6933", c(1)))
  expect_error(SP %>% sdm_area("Test area", "EPSG:6933", c(0,2)))
  expect_error(SP %>% sdm_area("Test area", "EPSG:XXXX", c(0,2)))
  expect_error(SP %>% sdm_area(a_res= c(0,2)))

  expect_error("123" %>% sdm_area("Test area", "EPSG:6933", c(0,2)))
  expect_error("zzzzz" %>% sdm_area("Test area", "EPSG:6933", c(50000, 50000)))
  expect_error(".gpkg" %>% sdm_area("Test area", "EPSG:6933", c(50000, 50000)))
})


test_that("Creating a study area from SpatialLines.", {
  sl1 = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
  crs(sl1) <- CRS("EPSG:6933")
  new_area <- sl1 %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000))

  expect_equal(new_area$name, "Test area")
  expect_equal(new_area$epsg, "EPSG:6933")
  expect_equal(new_area$crs, raster::crs("EPSG:6933"))
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
  #expect_error(new_area %>% areas_gt(10000))
})

test_that("Trying to remove an small area from a SpatialLines study area.", {
  sl1 = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
  crs(sl1) <- CRS("EPSG:6933")
  new_area <- sl1 %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000))

  expect_error(new_area %>% areas_gt(10000))
})

test_that("Creating a study area from SpatialPolygons.", {
  new_area <- SP %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000))

  expect_equal(new_area$name, "Test area")
  expect_equal(new_area$epsg, "EPSG:6933")
  expect_equal(new_area$crs, raster::crs("EPSG:6933"))
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Creating a study area from SpatialLinesDataFrame file keeping the original CRS.", {
  new_area <- system.file("vect_files/hydro_uper_prpy.gpkg", package="sdmTools") %>%
    sdm_area(name = "Test area", a_res = c(50000, 50000))

  expect_equal(new_area$name, "Test area")
  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Creating a study area from SpatialPolygonsDataFrame file keeping the original CRS.", {
  new_area <- system.file("vect_files/brasil_uf.gpkg", package="sdmTools") %>%
    sdm_area(name = "Test area", a_res = c(50000, 50000))

  expect_equal(new_area$name, "Test area")
  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Creating a study area from SpatialLinesDataFrame file.", {
  new_area <- system.file("vect_files/hydro_uper_prpy.gpkg", package="sdmTools") %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000))

  expect_equal(new_area$name, "Test area")
  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Creating a study area from SpatialPolygonsDataFrame file.", {
  new_area <- system.file("vect_files/brasil_uf.gpkg", package="sdmTools") %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000))

  expect_equal(new_area$name, "Test area")
  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})


test_that("Creating a study area from a gridded SpatialPolygonsDataFrame file.", {
  new_area <- system.file("vect_files/brasil_uf.gpkg", package="sdmTools") %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  new_area <- new_area$study_area %>%
    sdm_area("Test area")

  expect_equal(new_area$name, "Test area")
  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_true(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Creating a study area from SpatialPolygonsDataFrame path.", {
  new_area <- "brasil" %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000))

  expect_equal(new_area$name, "Test area")
  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})



test_that("Trying to get resolution from a not gridded SDM_area.", {
  expect_null(NULL %>% .get_resolution())
})

test_that("Get file name.", {
  file_name_tmp <- "brasil" %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    .guess_file_name()

  expect_string(file_name_tmp, "test_area_50000_epsg_6933.gpkg")
})

test_that("Repair an area with invalid CRS.", {
  expect_error(
    P %>%
      list() %>%
      SpatialPolygons() %>%
      .repair_area()
  )
})





