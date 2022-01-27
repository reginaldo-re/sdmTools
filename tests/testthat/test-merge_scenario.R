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


# test_that("Merge scenarios over SDM_area with all unnamed raster variables.", {
#   withr::with_dir(
#     a_dir <- tempdir(),
#     {
#       a_dir <- a_dir %>% fs::path("scenarios_folder")
#       if (a_dir %>% fs::is_dir()){
#         a_dir %>%
#           fs::dir_delete()
#         a_dir %>%
#           fs::dir_create()
#       }
#
#       "inner_raster1" %>%
#         fs::dir_create()
#
#       system.file("rast_files", package="sdmTools") %>%
#         fs::dir_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)
#
#       "inner_raster2" %>%
#         fs::dir_create()
#
#       system.file("rast_files", package="sdmTools") %>%
#         fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)
#
#       a_scenario <- a_dir %>%
#         sdm_scenario()
#
#       gridded_area <- a_sdm_area_gridded_area %>%
#         merge_scenario(a_scenario, new_path = "/tmp")
#
#       expect_true(gridded_area)
#     }
#   )
# })
