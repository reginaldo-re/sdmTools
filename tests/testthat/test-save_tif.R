test_that("Saving a SDM_area to tif file guessing file name.", {
  withr::with_dir(a_dir <- tempdir(),
    {
      a_sdm_area %>%
        save_tif(file_path = a_dir)

      expect_true(
        fs::is_file(
          fs::path(a_dir) %>%
            fs::path("test_area_50000_epsg_6933") %>%
            fs::path("geometriaaproximada.tif")
          )
        )
      expect_true(
        fs::is_file(
          fs::path(a_dir) %>%
            fs::path("test_area_50000_epsg_6933") %>%
            fs::path("sigla.tif")
        )
      )
      expect_true(
        fs::is_file(
          fs::path(a_dir) %>%
            fs::path("test_area_50000_epsg_6933") %>%
            fs::path("geocodigo.tif")
        )
      )
      expect_true(
        fs::is_file(
          fs::path(a_dir) %>%
          fs::path("test_area_50000_epsg_6933") %>%
            fs::path("nome.tif")
        )
      )
    }
  )
})


test_that("Saving a SDM_area to tif file giving a file name.", {
  withr::with_dir(a_dir <- tempdir(),
    {
      a_sdm_area %>%
        save_tif("test", a_dir)

      expect_true(
        fs::is_file(
          fs::path(a_dir) %>%
          fs::path("test") %>%
            fs::path("geometriaaproximada.tif")
        )
      )
      expect_true(
        fs::is_file(
          fs::path(a_dir) %>%
            fs::path("test") %>%
            fs::path("sigla.tif")
        )
      )
      expect_true(
        fs::is_file(
          fs::path(a_dir) %>%
            fs::path("test") %>%
            fs::path("geocodigo.tif")
        )
      )
      expect_true(
        fs::is_file(
          fs::path(a_dir) %>%
            fs::path("test") %>%
            fs::path("nome.tif")
        )
      )
    }
  )
})

