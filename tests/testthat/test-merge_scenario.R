test_that("Merge non valid scenarios over SDM_area.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_scenario(system.file("rast_files", package="sdmTools"), new_path = "tmp")
  )
})

test_that("Merge non valid new_path to merged scenario over SDM_area.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_scenario(system.file("rast_files", package="sdmTools"))
  )
})


test_that("Merge scenarios over SDM_area with all unnamed raster variables.", {
  a_scenario <- system.file("rast_files", package="sdmTools") %>%
    sdm_scenario()

  gridded_area <- a_sdm_area_gridded_area %>%
    merge_scenario(a_scenario, new_path = "/tmp")

  expect_true(gridded_area)
})
