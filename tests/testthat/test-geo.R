main <-  cbind(
  c(0, 0, 1, 1),
  c(0, 1, 1, 0)
)
secondary <- cbind(
  c(1, 1.3, 1.3, 1),
  c(1, 1.0, 0.7, 0.7)
)
hole <- main/3 + 1/3
island = cbind(
  c(1.05, 1.05, 1.55, 1.55),
  c(0, .5, .5, 0)
)

P <- Polygons(
  ID = 1,
  list(
    Polygon(main),
    Polygon(hole, hole = TRUE),
    Polygon(island),
    Polygon(secondary)
  )
)

SP = SpatialPolygons(list(P))
crs(SP) <- CRS("EPSG:6933")

SPDF <- rgdal::readOGR(system.file("brasil_uf.gpkg", package="sdmTools"), layer = "brasil_uf", verbose = F)
SLDF <- rgdal::readOGR(system.file("hydro_uper_prpy.gpkg", package="sdmTools"), layer = "hydro_uper_prpy", verbose = F)

a_sdm_area <- SPDF %>%
  sdm_area("EPSG:6933", c(50000, 50000))

a_sdm_area_gridded_area <- a_sdm_area %>%
  make_grid()


test_that("Creating a study area from SpatialLines.", {
  sl1 = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
  crs(sl1) <- CRS("EPSG:6933")
  new_area <- sl1 %>%
    sdm_area("EPSG:6933", c(50000, 50000))

  expect_equal(new_area$crs, "EPSG:6933")
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
  #expect_error(new_area %>% areas_gt(10000))
})

test_that("Creating a study area from SpatialPolygons.", {
  new_area <- SP %>%
    sdm_area("EPSG:6933", c(50000, 50000))

  expect_equal(new_area$crs, "EPSG:6933")
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})


test_that("Creating a study area from SpatialLinesDataFrame file keeping the original CRS.", {
  new_area <- system.file("hydro_uper_prpy.gpkg", package="sdmTools") %>%
    sdm_area(a_res=c(50000, 50000))

  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Creating a study area from SpatialPolygonsDataFrame file keeping the original CRS.", {
  new_area <- system.file("brasil_uf.gpkg", package="sdmTools") %>%
    sdm_area(a_res= c(50000, 50000))

  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Creating a study area from SpatialLinesDataFrame file.", {
  new_area <- system.file("hydro_uper_prpy.gpkg", package="sdmTools") %>%
    sdm_area("EPSG:6933", c(50000, 50000))

  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Creating a study area from SpatialPolygonsDataFrame file.", {
  new_area <- system.file("brasil_uf.gpkg", package="sdmTools") %>%
    sdm_area("EPSG:6933", c(50000, 50000))

  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_false(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})


test_that("Creating a study area from a gridded SpatialPolygonsDataFrame", {
  new_area <- system.file("brasil_uf.gpkg", package="sdmTools") %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  new_area <- new_area$study_area %>%
    sdm_area()

  expect_false(new_area$crs %>% is.na())
  expect_equal(new_area$resolution, c(50000, 50000))
  expect_true(new_area$gridded)
  expect_s3_class(new_area, "SDM_area")
})

test_that("Trying to create an invalid objects SDM_area from a SpatialPolygon object.", {
  expect_error(NULL %>% sdm_area())
  expect_error(NULL %>% sdm_area(NULL))
  expect_error(NULL %>% sdm_area(NULL, NULL))
  expect_error(NULL %>% sdm_area(NULL, c(1,1)))
  expect_error(NULL %>% sdm_area("EPSG:6933", c(1,1)))

  expect_error(SP %>% sdm_area())
  expect_error(SP %>% sdm_area("EPSG:XXXX"))
  expect_error(SP %>% sdm_area("EPSG:6933"))
  expect_error(SP %>% sdm_area("EPSG:6933", c(1)))
  expect_error(SP %>% sdm_area("EPSG:6933", c(0,2)))
  expect_error(SP %>% sdm_area("EPSG:XXXX", c(0,2)))
  expect_error(SP %>% sdm_area(a_res= c(0,2)))

  expect_error("123" %>% sdm_area("EPSG:6933", c(0,2)))
})


test_that("Trying to remove an small area from a SpatialLines study area.", {
  sl1 = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
  crs(sl1) <- CRS("EPSG:6933")
  new_area <- sl1 %>%
    sdm_area("EPSG:6933", c(50000, 50000))

  expect_error(new_area %>% areas_gt(10000))
})

test_that("Removing a single area from a SpatialPolygons study area.", {
  new_area <- SP %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    areas_gt(0.25)
  expect_equal(new_area$study_area %>% gArea() %>% round(2), 1.09)
})

test_that("Removing no areas from a SpatialPolygons study area.", {
  new_area <- SP %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    areas_gt(0.1)

  expect_equal(new_area$study_area %>% gArea() %>% round(2), 1.34)
})

test_that("Removing all areas from a SpatialPolygons study area.", {
  new_area <- SP %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    areas_gt(20)

  expect_error(gArea(new_area$study_area))
})

test_that("Removing no areas from study area using SpatialPolygonsDataframe.", {
  new_area <- SP %>%
    as("SpatialPolygonsDataFrame") %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    areas_gt(0.1)

  expect_equal(new_area$study_area %>% gArea() %>% round(2), 1.34)
})

test_that("Making a grid over study area (SpatialPolygonsDataframe) removing all variables.", {
  gridded_area <- SPDF %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid(var_names = list())

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_true(((gridded_area$study_area@data %>% names()) == c("cell_id", "x_centroid", "y_centroid")) %>% all())
})

test_that("Making a grid over study area (SpatialPolygonsDataframe) with all variables.", {
  gridded_area <- SPDF %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_equal(gridded_area$study_area@data$geocodigo %>% mean() %>% round(2), 12.10)
})

test_that("Making a grid over study area (SpatialPolygonsDataframe) with one variable.", {
  gridded_area <- SPDF %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid(var_names = list("geocodigo"))

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
  expect_equal(gridded_area$study_area@data$geocodigo %>% mean() %>% round(2), 12.10)
})


test_that("Making a grid over study area (SpatialPolygons).", {
  gridded_area <- SPDF %>%
    as("SpatialPolygons") %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
})

test_that("Making a grid over study area (SpatialLinesDataframe) removing all variables.", {
  gridded_area <- SLDF %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid(var_names = list())

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_true(((gridded_area$study_area@data %>% names()) == list("cell_id", "x_centroid", "y_centroid")) %>% all())
})

test_that("Making a grid over study area (SpatialLinesDataframe) with all variables.", {
  gridded_area <- SLDF %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_equal(gridded_area$study_area@data$LENGTH_ %>% mean() %>% round(2), 3.79)
})

test_that("Making a grid over study area (SpatialLinesDataframe) with one variable.", {
  gridded_area <- SLDF %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid(var_names = list("Length", "xxx", "Main_ri"))

  expect_equal(gridded_area$study_area %>% nrow(), 351)
  expect_equal(gridded_area$study_area@data$Length %>% mean() %>% round(2), 3.79)
})


test_that("Making a grid over study area (SpatialLinesDataframe) with no data", {
  sl1 = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
  crs(sl1) <- CRS("EPSG:6933")
  new_area <- sl1 %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_s3_class(new_area, "SDM_area")
})


test_that("Making a grid over SDM_area.", {
  gridded_area <- SPDF %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    make_grid()

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
})


test_that("Merge raster over SDM_area with all unnamed raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(system.file("rasters", package="sdmTools"))

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$wc2.0_bio_5m_02 %>% mean() %>% round(2), 11.08)
})

test_that("Merge raster over non gridded SDM_area with all unnamed raster variables.", {
  gridded_area <- SPDF %>%
    sdm_area("EPSG:6933", c(50000, 50000)) %>%
    merge_area(system.file("rasters", package="sdmTools"))

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$wc2.0_bio_5m_02 %>% mean() %>% round(2), 11.08)
})


test_that("Merge raster over SDM_area with one named raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(system.file("rasters/wc2.0_bio_5m_01.tif", package="sdmTools"))

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
})


test_that("Merge raster over SDM_area with all named raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(system.file("rasters", package="sdmTools"), var_names = list("bio_5m_01", "bio_5m_02"))

  expect_equal(gridded_area$study_area$bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$bio_5m_02 %>% mean() %>% round(2), 11.08)
})

test_that("Merge raster over SDM_area with only one named raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(system.file("rasters", package="sdmTools"), var_names = list("bio_5m_01"))

  expect_equal(gridded_area$study_area$bio_5m_01 %>% mean() %>% round(2), 24.37)
})

test_that("Merge raster over SDM_area with none raster variables.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(system.file("rasters", package="sdmTools"), var_names = list()))
})

test_that("Merge raster over SDM_area with invalid area source.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(system.file("rast", package="sdmTools"), var_names = list()))
})

test_that("Merge raster over SDM_area with wrong raster variables names.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(system.file("rasters", package="sdmTools"), list("wrong_name")),
    "At least one variable name is ambiguous. Try to use more specific variable names."
  )
})


test_that("Ploat an area map from a study area.", {
  a_ggplot<- a_sdm_area %>% area_geomap()

  expect_s3_class(a_ggplot, "ggplot")
})

test_that("Ploat an grid map from a gridded study area.", {
  a_ggplot<- a_sdm_area %>% grid_geomap(a_sdm_area_gridded_area)

  expect_s3_class(a_ggplot, "ggplot")
})
