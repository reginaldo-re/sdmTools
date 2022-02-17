test_that("Merge non valid scenarios over SDM_area.", {
  expect_error(
    a_sdm_area %>%
      merge_scenario(system.file("rast_files", package="sdmTools"), new_path = "tmp")
  )
})


test_that("Merge scenario folder containing a single raster.", {
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
        sdm_scenario(list("bio_5m_01", "bio_5m_02"))

      a_sdm_area_gridded_area %>%
        merge_scenario(to_merge_scenario = tmp_scenario, new_path = tempdir() %>% fs::path("new_path"))

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir %>% fs::path_dir())
      expect_true((tmp_scenario$content %>% fs::path_file() ==  c("wc2.0_bio_5m_01.tif","wc2.0_bio_5m_02.tif")) %>% all())
      checkmate::expect_file_exists(
        tempdir() %>%
          fs::path("new_path") %>%
          fs::path("scenarios_folder_50000_epsg_6933.gpkg")
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

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster1"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster2"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_raster3") %>% fs::path("inner_inner_raster1"), overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario(list("bio_5m_01", "bio_5m_02"))

      a_sdm_area_gridded_area %>%
        merge_scenario(to_merge_scenario = tmp_scenario, new_path = tempdir() %>% fs::path("new_path"))

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir %>% fs::path_dir())
      checkmate::expect_int(tmp_scenario$content %>% length(), lower = 3, upper = 3)
      checkmate::expect_int(tmp_scenario$content$`scenarios_folder/inner_raster3` %>% length(), lower = 1, upper = 1)
      checkmate::expect_directory_exists(
        a_dir %>%
          fs::path_dir() %>%
          fs::path("new_path") %>%
          fs::path("inner_raster1")
      )
      checkmate::expect_directory_exists(
        a_dir %>%
          fs::path_dir() %>%
          fs::path("new_path") %>%
          fs::path("inner_raster2")
      )
      checkmate::expect_directory_exists(
        a_dir %>%
          fs::path_dir() %>%
          fs::path("new_path") %>%
          fs::path("inner_raster3")
      )
      checkmate::expect_directory_exists(
        a_dir %>%
          fs::path_dir() %>%
          fs::path("new_path") %>%
          fs::path("inner_raster3") %>%
          fs::path("inner_inner_raster1")
      )
      checkmate::expect_file_exists(
        a_dir %>%
          fs::path_dir() %>%
          fs::path("new_path") %>%
          fs::path("inner_raster1") %>%
          fs::path("inner_raster_1_50000_epsg_6933.gpkg")
      )
      checkmate::expect_file_exists(
        a_dir %>%
          fs::path_dir() %>%
          fs::path("new_path") %>%
          fs::path("inner_raster2") %>%
          fs::path("inner_raster_2_50000_epsg_6933.gpkg")
      )
      checkmate::expect_file_exists(
        a_dir %>%
          fs::path_dir() %>%
          fs::path("new_path") %>%
          fs::path("inner_raster3") %>%
          fs::path("inner_inner_raster1") %>%
          fs::path("inner_inner_raster_1_50000_epsg_6933.gpkg")
      )
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

      a_dir %>%
        fs::path("inner_vect1") %>%
        fs::dir_create()

      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect1"), overwrite = T)

      a_dir %>%
        fs::path("inner_vect1") %>%
        fs::dir_create()

      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect2"), overwrite = T)

      a_dir %>%
        fs::path("inner_vect3") %>%
        fs::path("inner_inner_vect1") %>%
        fs::dir_create()

      system.file("vect_files", package="sdmTools") %>%
        fs::dir_copy(a_dir %>% fs::path("inner_vect3") %>% fs::path("inner_inner_vect1"), overwrite = T)


      tmp_scenario <- a_dir %>%
        sdm_scenario(list("bio_5m_01", "bio_5m_02"))

      a_sdm_area_gridded_area %>%
        merge_scenario(to_merge_scenario = tmp_scenario, new_path = tempdir() %>% fs::path("new_path"))

      checkmate::expect_string(tmp_scenario$name, fixed = "scenarios_folder")
      checkmate::expect_string(tmp_scenario$path, fixed = a_dir %>% fs::path_dir())
      checkmate::expect_int(tmp_scenario$content %>% length(), lower = 3, upper = 3)
      checkmate::expect_int(tmp_scenario$content$`scenarios_folder/inner_vect3` %>% length(), lower = 1, upper = 1)
    }
  )
})



