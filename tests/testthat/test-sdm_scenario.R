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

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_folder"), overwrite = T)

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
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir %>% fs::path_dir())
      expect_true((tmp_scenario$content %>% fs::path_file() ==  c("wc2.0_bio_5m_01.tif","wc2.0_bio_5m_02.tif")) %>% all())
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

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster3") %>% fs::path("inner_inner_raster1"), overwrite = T)


      tmp_scenario <- a_dir %>%
        sdm_scenario()

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir %>% fs::path_dir())
      checkmate::expect_int(tmp_scenario$content %>% length(), lower = 3, upper = 3)
      checkmate::expect_int(tmp_scenario$content$`scenarios_folder/inner_raster3` %>% length(), lower = 1, upper = 1)
    }
  )
})


test_that("Scenario folder containing an hierarchy vector scenarios.", {
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

      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect1"), overwrite = T)

      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect2"), overwrite = T)

      a_dir %>%
        fs::path("inner_vect3") %>%
        fs::path("inner_inner_vect1") %>%
        fs::dir_create()

      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect3") %>% fs::path("inner_inner_vect1"), overwrite = T)


      tmp_scenario <- a_dir %>%
        sdm_scenario()

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir %>% fs::path_dir())
      checkmate::expect_int(tmp_scenario$content %>% length(), lower = 3, upper = 3)
      checkmate::expect_int(tmp_scenario$content$`scenarios_folder/inner_vect3` %>% length(), lower = 1, upper = 1)
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

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario(var_names = list("bio_5m_01"))

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(
          tmp_scenario$content %>% magrittr::extract2("scenarios_folder/inner_raster1") %>% fs::path_file(),
          fixed = "wc2.0_bio_5m_01.tif"
        )
    }
  )
})

test_that("Scenario folder containing an hierarchy vector scenarios and single variable.", {
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

      a_dir %>%
        fs::path("inner_vect1") %>%
        fs::dir_create()

      system.file("vect_files", package="sdmTools") %>%
        fs::path("brasil_uf.gpkg") %>%
        fs::file_copy(a_dir %>% fs::path("inner_vect1"), overwrite = T)


      a_dir %>%
        fs::path("inner_vect2") %>%
        fs::dir_create()

      system.file("vect_files", package="sdmTools") %>%
        fs::path("brasil_uf.gpkg") %>%
        fs::file_copy(a_dir %>% fs::path("inner_vect2"), overwrite = T)

      a_dir %>%
        fs::path("inner_vect3") %>%
        fs::path("inner_inner_vect1") %>%
        fs::dir_create()

      system.file("vect_files", package="sdmTools") %>%
        fs::path("brasil_uf.gpkg") %>%
        fs::file_copy(a_dir %>% fs::path("inner_vect3") %>% fs::path("inner_inner_vect1"), overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario(var_names = list("geocodigo"))

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir %>% fs::path_dir())
      checkmate::expect_int(tmp_scenario$content %>% length(), lower = 3, upper = 3)
    }
  )
})


test_that("Scenario folder containing an hierarchy vector scenarios and invalid variables.", {
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


      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect1"), overwrite = T)

      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect2"), overwrite = T)

      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect3") %>% fs::path("inner_inner_vect1"), overwrite = T)

      expect_error(
        tmp_scenario <- a_dir %>%
          sdm_scenario(var_names = list("geocodigo", "invalid_variable"))
      )
    }
  )
})


