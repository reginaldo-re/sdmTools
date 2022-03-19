test_that("Scenario folder is invalid: contains a non hierarchy of files and folders.", {
  a_dir <- tempdir()

  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir, overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_folder"), overwrite = T)

      expect_error(
        tmp_scenario <- a_dir %>%
          sdm_scenario()
      )
    }
  )
})

test_that("Scenario folder is invalid: contains a mix of non raster and vect files.", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir, overwrite = T)

     system.file("vect_files", package="sdmTools") %>%
       dir_copy(a_dir, overwrite = T)

      expect_error(
        tmp_scenario <- a_dir %>%
          sdm_scenario()
      )
    }
  )
})


test_that("Scenario folder containing a single raster but no variable names.", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir, overwrite = T)

      expect_error(
        tmp_scenario <- a_dir %>%
          sdm_scenario()
      )

    }
  )
})

test_that("Scenario folder with one invalid named raster variables.", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_raster1"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_raster2"), overwrite = T)

      expect_error(
        tmp_scenario <- a_dir %>%
          sdm_scenario(var_names = list("bio_5m_01", "invalid_name"))
      )
    }
  )
})


test_that("Scenario folder containing a single raster.", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_raster1"), overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario(var_names = list("bio_5m_01","bio_5m_02"))

      expect_string(tmp_scenario$sdm_scenario_name, fixed = "scenarios_folder")
      expect_string(tmp_scenario$dir_path, fixed = a_dir %>% path_dir())
      expect_true((tmp_scenario$content %>% unlist() %>% path_file() ==  c("wc2.0_bio_5m_01.tif","wc2.0_bio_5m_02.tif")) %>% all())
    }
  )
})

test_that("Scenario folder containing a correct hierarchy of raster files.", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_raster1"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_raster2"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_raster3") %>% path("inner_inner_raster1"), overwrite = T)


      tmp_scenario <- a_dir %>%
        sdm_scenario(list("bio_5m_01","bio_5m_02"))

      expect_string(tmp_scenario$sdm_scenario_name, fixed = "scenarios_folder")
      expect_string(tmp_scenario$dir_path, fixed = a_dir %>% path_dir())
      expect_int(tmp_scenario$content %>% length(), lower = 3, upper = 3)
      expect_int(tmp_scenario$content$`scenarios_folder/inner_raster3` %>% length(), lower = 1, upper = 1)
    }
  )
})



test_that("Scenario folder with only one named raster variables", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_raster1"), overwrite = T)

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir %>% path("inner_raster2"), overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario(var_names = list("bio_5m_01"))

      expect_string(tmp_scenario$sdm_scenario_name, fixed = "scenarios_folder")
      expect_string(
          tmp_scenario$content %>% extract2("scenarios_folder/inner_raster1") %>% path_file(),
          fixed = "wc2.0_bio_5m_01.tif"
      )
    }
  )
})


test_that("Scenario folder containing a hierarchy vector scenarios.", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      a_dir %>%
        path("inner_vect1")  %>%
        dir_create()

      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect1"), overwrite = T)

      a_dir %>%
        path("inner_vect2")  %>%
        dir_create()
      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect2"), overwrite = T)

      a_dir %>%
        path("inner_vect3") %>%
        path("inner_inner_vect1") %>%
        dir_create()

      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect3") %>% path("inner_inner_vect1"), overwrite = T)


      tmp_scenario <- a_dir %>%
        sdm_scenario(var_names = list("geocodigo", "nome"))

      expect_string(tmp_scenario$sdm_scenario_name, fixed = "scenarios_folder")
      expect_string(tmp_scenario$dir_path, fixed = a_dir %>% path_dir())
      expect_int(tmp_scenario$content %>% length(), lower = 3, upper = 3)
      expect_int(tmp_scenario$content$`scenarios_folder/inner_vect3` %>% length(), lower = 1, upper = 1)
    }
  )
})

test_that("Scenario folder containing a hierarchy vector scenarios and single variable.", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      a_dir %>%
        path("inner_vect1") %>%
        dir_create()
      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect1"), overwrite = T)
      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect1") %>% path("brasil_uf2.gpkg"), overwrite = T)


      a_dir %>%
        path("inner_vect2") %>%
        dir_create()
      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect2"), overwrite = T)


      a_dir %>%
        path("inner_vect3") %>%
        path("inner_inner_vect1") %>%
        dir_create()
      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect3") %>% path("inner_inner_vect1"), overwrite = T)

      tmp_scenario <- a_dir %>%
        sdm_scenario(var_names = list("geocodigo"))

      expect_string(tmp_scenario$sdm_scenario_name, fixed = "scenarios_folder")
      expect_string(tmp_scenario$dir_path, fixed = a_dir %>% path_dir())
      expect_int(tmp_scenario$content[["scenarios_folder/inner_vect1"]] %>% length(), lower = 2, upper = 2)
    }
  )
})


test_that("Scenario folder containing a hierarchy vector scenarios and invalid variables.", {
  a_dir <- tempdir()
  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      a_dir %>%
        path("inner_vect1") %>%
        dir_create()
      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect1"), overwrite = T)

      a_dir %>%
        path("inner_vect2") %>%
        dir_create()
      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect2"), overwrite = T)

      a_dir %>%
        path("inner_vect3") %>%
        path("inner_inner_vect1") %>%
        dir_create()
      system.file("vect_files", package="sdmTools") %>%
        path("brasil_uf.gpkg") %>%
        file_copy(a_dir %>% path("inner_vect3") %>% path("inner_inner_vect1"), overwrite = T)

      expect_error(
        tmp_scenario <- a_dir %>%
          sdm_scenario(var_names = list("geocodigo", "invalid_variable"))
      )
    }
  )
})

test_that("Scenario folder is invalid: contains a non hierarchy of files and folders.", {
  a_dir <- tempdir()

  with_tempdir(
    tmpdir = a_dir,
    clean = T,
    code = {
      a_dir <- a_dir %>% path("scenarios_folder")
      if (a_dir %>% is_dir()){
        a_dir %>%
          dir_delete()
        a_dir %>%
          dir_create()
      }

      system.file("rast_files", package="sdmTools") %>%
        dir_copy(a_dir, overwrite = T)

      a_dir %>%
        path("empty_folder") %>%
        dir_create()

      expect_error(
        .find_scenario_rast_files(a_dir, "")
      )
    }
  )
})


