test_that("Scenario folder is invalid: contains files and folders.", {
  withr::with_dir(
    a_dir <- tempdir(),
    {
      a_dir <- a_dir %>% fs::path("scenarios_folder")
      if (a_dir %>% fs::is_dir()){
        a_dir %>%
          fs::dir_delete()
        a_dir %>%
          fs::dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir, overwrite = T)

      a_dir %>%
        fs::path("inner_folder") %>%
        fs::dir_create()

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(
          a_dir %>%
            fs::path("inner_folder"),
          overwrite = T
        )

      expect_error(
        tmp_scenario <- a_dir %>%
          sdm_scenario()
      )
    }
  )
})

test_that("Scenario folder is invalid: contains non raster files", {
  withr::with_dir(
    a_dir <- tempdir(),
    {
      a_dir <- a_dir %>% fs::path("scenarios_folder")
      if (a_dir %>% fs::is_dir()){
        a_dir %>%
          fs::dir_delete()
        a_dir %>%
          fs::dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir, overwrite = T)

     system.file("vect_files", package="sdmTools") %>%
       fs::dir_copy(a_dir, overwrite = T)

      expect_error(
        tmp_scenario <- a_dir %>%
          sdm_scenario()
      )
    }
  )
})

test_that("Scenario folder containing a single raster.", {
  withr::with_dir(
    a_dir <- tempdir(),
    {
      a_dir <- a_dir %>% fs::path("scenarios_folder")
      if (a_dir %>% fs::is_dir()){
        a_dir %>%
          fs::dir_delete()
        a_dir %>%
          fs::dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir, overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario()

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir)
      checkmate::expect_names(
        tmp_scenario$content %>% names(),
        identical.to = c(
          "wc2.0_bio_5m_01.tif",
          "wc2.0_bio_5m_02.tif"
        )
      )
    }
  )
})

test_that("Scenario folder containing an hierarchy scenarios.", {
  withr::with_dir(
    a_dir <- tempdir(),
    {
      a_dir <- a_dir %>% fs::path("scenarios_folder")
      if (a_dir %>% fs::is_dir()){
        a_dir %>%
          fs::dir_delete()
        a_dir %>%
          fs::dir_create()
      }

      "inner_raster1" %>%
        fs::dir_create()

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)

      "inner_raster2" %>%
        fs::dir_create()

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario()

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir)
      checkmate::expect_int(tmp_scenario$content %>% length(), lower = 2, upper = 2)
    }
  )
})


test_that("Scenario folder with only one named raster variables", {
  withr::with_dir(
    a_dir <- tempdir(),
    {
      a_dir <- a_dir %>% fs::path("scenarios_folder")
      if (a_dir %>% fs::is_dir()){
        a_dir %>%
          fs::dir_delete()
        a_dir %>%
          fs::dir_create()
      }

      "inner_raster1" %>%
        fs::dir_create()

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)

      "inner_raster2" %>%
        fs::dir_create()

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario(var_names = list("bio_5m_01"))

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(
          tmp_scenario$content %>% magrittr::extract2("inner_raster1"),
          fixed = "wc2.0_bio_5m_01.tif"
        )
    }
  )
})


test_that("Scenario folder with no named raster variables", {
  withr::with_dir(
    a_dir <- tempdir(),
    {
      a_dir <- a_dir %>% fs::path("scenarios_folder")
      if (a_dir %>% fs::is_dir()){
        a_dir %>%
          fs::dir_delete()
        a_dir %>%
          fs::dir_create()
      }

      "inner_raster1" %>%
        fs::dir_create()

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)

      "inner_raster2" %>%
        fs::dir_create()

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)

      expect_error(
        a_dir %>%
          sdm_scenario(var_names = list())
      )
    }
  )
})
