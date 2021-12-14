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

test_that("Lower bound less than or equal zero.", {
  expect_warning(areas_gt(NULL, 0), "Nothing to do, the type of an_area must be: SpatialPolygons, SpatialPolygonsDataFrame, or SDM_area.")
})

test_that("Only null parameters.", {
  expect_warning(areas_gt(NULL, NULL), "Nothing to do, the type of an_area must be: SpatialPolygons, SpatialPolygonsDataFrame, or SDM_area.")
})

test_that("Study area is not a vector polygon.", {
  sl1 = SpatialLines(list(Lines(Line(cbind(c(2,4,4,1,2),c(2,3,5,4,2))), "sp")))
  expect_warning(areas_gt(sl1, 10000), "Nothing to do, the type of an_area must be: SpatialPolygons, SpatialPolygonsDataFrame, or SDM_area.")
})

test_that("Removing a single area from projected study area.", {
  new_area <- areas_gt(SP, 0.25)
  expect_equal(new_area %>% gArea() %>% round(2), 0.980)
})

test_that("Removing no areas from projected study area.", {
  new_area <- areas_gt(SP, 0.1)
  expect_equal(new_area %>% gArea() %>% round(2), 1.23)
})

test_that("Removing all areas from projected study area.", {
  new_area <- areas_gt(SP, 20)
  expect_error(gArea(new_area))
})


test_that("Removing a single area from sdm_area.", {
  new_sdm_area <- sdm_area(SP)
  new_sdm_area <- areas_gt(new_sdm_area, 0.25)

  expect_s3_class(new_sdm_area, "SDM_area")
  expect_equal(new_sdm_area$study_area %>% gArea() %>% round(2), 1.09)
})

test_that("Lower bound less than or equal zero on SDM_area.", {
  new_sdm_area <- sdm_area(SP)
  expect_error(areas_gt(new_sdm_area, 0))
})

test_that("Creating a valid SDM_area.", {
new_sdm_area <- sdm_area(SP)
  expect_s3_class(new_sdm_area, "SDM_area")
})

test_that("Trying to create an invalid SDM_area.", {
  expect_error(sdm_area(NULL), "Study area must be in a vectorized format!")
})


test_that("Removing no areas from study area using SpatialPolygonsDataframe.", {
  new_area <- areas_gt(SP %>% as("SpatialPolygonsDataFrame"), 0.1)
  expect_equal(new_area %>% gArea() %>% round(2), 1.23)
})

test_that("Making a grid over study area (SpatialPolygonsDataframe) removing all variables.", {
  gridded_area <- SPDF %>%
    make_grid(cell_width = 50000, cell_height = 50000, var_names = list(), centroid=T)

  expect_equal(gridded_area %>% nrow(), 3634)
  expect_true(((gridded_area@data %>% names()) == c("cell_id", "x_centroid", "y_centroid")) %>% all())
})

test_that("Making a grid over study area (SpatialPolygonsDataframe) with all variables.", {
  gridded_area <- SPDF %>%
    make_grid(cell_width = 50000, cell_height = 50000, centroid=T)

  expect_equal(gridded_area %>% nrow(), 3634)
  expect_equal(gridded_area@data$geocodigo %>% mean() %>% round(2), 12.10)
})

test_that("Making a grid over study area (SpatialPolygonsDataframe) with one variable.", {
  gridded_area <- SPDF %>%
    make_grid(cell_width = 50000, cell_height = 50000, var_names = c("geocodigo"), centroid=T)

  expect_equal(gridded_area %>% nrow(), 3634)
  expect_equal(gridded_area@data$geocodigo %>% mean() %>% round(2), 12.10)
})


test_that("Making a grid over study area (SpatialPolygons).", {
  gridded_area <- SPDF %>%
    as("SpatialPolygons") %>%
    make_grid(cell_width = 50000, cell_height = 50000, centroid=T)

  expect_equal(gridded_area %>% nrow(), 3634)
})

test_that("Making a grid over study area (SpatialLinesDataframe) removing all variables.", {
  gridded_area <- SLDF %>%
    make_grid(cell_width = 50000, cell_height = 50000, var_names = list(), centroid=T)

  expect_equal(gridded_area %>% nrow(), 351)
  expect_true(((gridded_area@data %>% names()) == c("cell_id", "x_centroid", "y_centroid")) %>% all())
})

test_that("Making a grid over study area (SpatialLinesDataframe) with all variables.", {
  gridded_area <- SLDF %>%
    make_grid(cell_width = 50000, cell_height = 50000, centroid=T)

  expect_equal(gridded_area %>% nrow(), 351)
  expect_equal(gridded_area@data$LENGTH_ %>% mean() %>% round(2), 3.79)
})

test_that("Making a grid over study area (SpatialLinesDataframe) with one variable.", {
  gridded_area <- SLDF %>%
    make_grid(cell_width = 50000, cell_height = 50000, var_names = c("Length", "xxx", "Main_ri"), centroid=T)

  expect_equal(gridded_area %>% nrow(), 351)
  expect_equal(gridded_area@data$Length %>% mean() %>% round(2), 3.79)
})


test_that("Making a grid over study area (SpatialLines).", {
  gridded_area <- SLDF %>%
    as("SpatialLines") %>%
    make_grid(cell_width = 50000, cell_height = 50000, centroid=T)

  expect_equal(gridded_area %>% nrow(), 351)
})


test_that("Making a grid over SDM_area.", {
  new_sdm_area <- sdm_area(SPDF)

  gridded_area <- new_sdm_area %>%
    make_grid(cell_width = 50000, cell_height = 50000, centroid=T)

  expect_equal(gridded_area$study_area %>% nrow(), 3634)
})


test_that("Merge raster over SDM_area", {
  new_sdm_area <- sdm_area(SPDF)

  gridded_area <- new_sdm_area %>%
    make_grid(cell_width = 50000, cell_height = 50000, centroid=T)

  gridded_area <- gridded_area %>%
    merge_area(system.file("rasters", package="sdmTools"), cell_width = 50000, cell_height = 50000)

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$wc2.0_bio_5m_02 %>% mean() %>% round(2), 11.08)
})
