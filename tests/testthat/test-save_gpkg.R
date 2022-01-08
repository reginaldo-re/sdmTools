test_that("Saving a SDM_area to gpkg file.", {
  withr::with_dir(a_dir <- tempdir(),
    {
      a_sdm_area %>%
      save_gpkg(file_path = a_dir)

      expect_true(fs::is_file(fs::path(a_dir, "test_area_50000_epsg_6933.gpkg")))
    }
  )
})


test_that("Saving SPDF to gpkg file.", {
  withr::with_file(a_file <- tempfile(),
                  {
                    SPDF %>%
                      save_gpkg(file_name = a_file)

                    expect_true(fs::is_file(fs::path(fs::path_dir(a_file), fs::path_file(a_file))))
                  }
  )
})
