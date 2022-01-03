test_that("Saving a SDM_area to gpkg file.", {
  withr::with_dir(a_dir <- tempdir(),
    {
      a_sdm_area %>%
      save_gpkg(file_path = a_dir)

      expect_true(fs::is_file(fs::path(a_dir, "test_area_50000_epsg_6933.gpkg")))
    }
  )
})
