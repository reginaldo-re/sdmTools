test_that("Merge raster over SDM_area with all unnamed raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(new_name = T, system.file("rast_files", package="sdmTools"))

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$wc2.0_bio_5m_02 %>% mean() %>% round(2), 11.08)
})


test_that("Merge raster over non gridded SDM_area with all unnamed raster variables.", {
  gridded_area <- SPDF %>%
    sdm_area("Test area", "EPSG:6933", c(50000, 50000)) %>%
    merge_area(new_name = "Test", system.file("rast_files", package="sdmTools"))

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$wc2.0_bio_5m_02 %>% mean() %>% round(2), 11.08)
})


test_that("Merge raster over SDM_area with one named raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(system.file("rast_files/wc2.0_bio_5m_01.tif", package="sdmTools"))

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
})


test_that("Merge raster over SDM_area with all named raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(system.file("rast_files", package="sdmTools"), var_names = list("bio_5m_01", "bio_5m_02"))

  expect_equal(gridded_area$study_area$bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$bio_5m_02 %>% mean() %>% round(2), 11.08)
})

test_that("Merge raster over SDM_area with only one named raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(system.file("rast_files", package="sdmTools"), var_names = list("bio_5m_01"))

  expect_equal(gridded_area$study_area$bio_5m_01 %>% mean() %>% round(2), 24.37)
})

test_that("Merge raster over SDM_area with none raster variables.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(system.file("rast_files", package="sdmTools"), var_names = list()))
})

test_that("Merge raster over SDM_area with invalid area source.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(system.file("rast", package="sdmTools"), var_names = list()))
})

test_that("Merge raster over SDM_area with wrong raster variables names.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(system.file("rast_files", package="sdmTools"), list("wrong_name")),
    "At least one variable name is ambiguous. Try to use more specific variable names."
  )
})
