test_that("Merge scenario folder containing a single raster.", {
  scenario_dir <- tempdir() %>% path("scenarios_folder")
  new_sdm_area_dir <- tempdir() %>% path("new_sdm_area_dir")

  if (scenario_dir %>% dir_exists()){
    scenario_dir %>%
      dir_delete()
    scenario_dir %>%
      dir_create()
  }

  system.file("rast_files", package="sdmTools") %>%
    dir_copy(scenario_dir %>% path("inner_raster1"), overwrite = T)

  merged_area <- a_sdm_area %>%
    merge_scenario(
      to_merge_scenario = scenario_dir,
      var_names = list("bio_5m_01","bio_5m_02"),
      dir_path = new_sdm_area_dir
    )

  expect_equal(merged_area$study_area %>% nrow(), 3629)
   expect_file_exists(
    new_sdm_area_dir %>%
      path("Test area.gpkg")
  )
  expect_file_exists(
    new_sdm_area_dir %>%
      path("scenarios/inner_raster1.gpkg")
  )
  expect_true(new_sdm_area_dir %>% dir_ls(recurse = T, type = "file") %>% length == 2)

  shp_tmp <- new_sdm_area_dir %>%
    path("scenarios/inner_raster1.gpkg") %>%
    readOGR(verbose = F)

  expect_equal(merged_area$study_area %>% nrow(), shp_tmp %>% nrow())

  scenario_dir %>%
    dir_delete()

  new_sdm_area_dir %>%
    dir_delete()
})

test_that("Trying to merge an already merged scenario folder.", {
  scenario_dir <- tempdir() %>% path("scenarios_folder")
  new_sdm_area_dir <- tempdir() %>% path("new_sdm_area_dir")

  if (scenario_dir %>% dir_exists()){
    scenario_dir %>%
      dir_delete()
    scenario_dir %>%
      dir_create()
  }

  system.file("rast_files", package="sdmTools") %>%
    dir_copy(scenario_dir %>% path("inner_raster1"), overwrite = T)

  tmp_scenario <- scenario_dir %>%
    sdm_scenario(var_names = list("bio_5m_01","bio_5m_02"))

  merged_area <- a_sdm_area_gridded_area %>%
    merge_scenario(
      to_merge_scenario = tmp_scenario,
      var_names = list("bio_5m_01","bio_5m_02"),
      dir_path = new_sdm_area_dir
    )

  expect_error(
    merged_area <- merged_area %>%
      merge_scenario(
        to_merge_scenario = tmp_scenario,
        var_names = list("bio_5m_01","bio_5m_02"),
        dir_path = new_sdm_area_dir
      )
  )

  scenario_dir %>%
    dir_delete()

  new_sdm_area_dir %>%
    dir_delete()
})


test_that("Scenario folder containing an hierarchy scenarios.", {
  scenario_dir <- tempdir() %>% path("scenarios_folder")
  new_sdm_area_dir <- tempdir() %>% path("new_sdm_area_dir")

  if (scenario_dir %>% is_dir()){
    scenario_dir %>%
      dir_delete()
    scenario_dir %>%
      dir_create()
  }

  system.file("rast_files", package="sdmTools") %>%
    dir_copy(scenario_dir %>% path("inner_raster1") %>% path("inner_inner_raster1"), overwrite = T)

  system.file("rast_files", package="sdmTools") %>%
    dir_copy(scenario_dir %>% path("inner_raster2") %>% path("inner_inner_raster1"), overwrite = T)

  system.file("rast_files", package="sdmTools") %>%
    dir_copy(scenario_dir %>% path("inner_raster3") %>% path("inner_inner_raster1"), overwrite = T)

  system.file("rast_files", package="sdmTools") %>%
    dir_copy(scenario_dir %>% path("inner_raster3") %>% path("inner_inner_raster2"), overwrite = T)

  tmp_scenario <- scenario_dir %>%
    sdm_scenario(list("bio_5m_01", "bio_5m_02"))

  merged_area <- a_sdm_area_gridded_area %>%
    merge_scenario(
      to_merge_scenario = tmp_scenario,
      var_names = list("bio_5m_01", "bio_5m_02"),
      dir_path = new_sdm_area_dir
    )

  expect_equal(merged_area$study_area %>% nrow(), 3629)
  expect_string(tmp_scenario$sdm_scenario_name, fixed = "scenarios_folder")
  expect_string(tmp_scenario$dir_path, fixed = scenario_dir %>% path_dir())
  expect_int(tmp_scenario$content %>% length(), lower = 3, upper = 3)
  expect_int(tmp_scenario$content$`scenarios_folder/inner_raster3` %>% length(), lower = 2, upper = 2)
  expect_file_exists(
    new_sdm_area_dir %>%
      path("scenarios") %>%
      path("inner_raster1") %>%
      path("inner_inner_raster1.gpkg")
  )
  expect_file_exists(
    new_sdm_area_dir %>%
      path("scenarios") %>%
      path("inner_raster2") %>%
      path("inner_inner_raster1.gpkg")
  )
  expect_file_exists(
    new_sdm_area_dir %>%
      path("scenarios") %>%
      path("inner_raster3") %>%
      path("inner_inner_raster1.gpkg")
  )
  expect_file_exists(
    new_sdm_area_dir %>%
      path("scenarios") %>%
      path("inner_raster3") %>%
      path("inner_inner_raster2.gpkg")
  )

  scenario_dir %>%
    dir_delete()

  new_sdm_area_dir %>%
    dir_delete()
})


test_that("Scenario folder containing an hierarchy vector scenarios.", {
  scenario_dir <- tempdir() %>% path("scenarios_folder")
  new_sdm_area_dir <- tempdir() %>% path("new_sdm_area_dir")


  if (scenario_dir %>% is_dir()){
    scenario_dir %>%
      dir_delete()
    scenario_dir %>%
      dir_create()
  }

  scenario_dir %>%
    path("inner_vect1")  %>%
    dir_create()

  system.file("vect_files", package="sdmTools") %>%
    path("hydro_uper_prpy.gpkg") %>%
    file_copy(scenario_dir %>% path("inner_vect1"), overwrite = T)

  scenario_dir %>%
    path("inner_vect2")  %>%
    dir_create()

  system.file("vect_files", package="sdmTools") %>%
    path("hydro_uper_prpy.gpkg") %>%
    file_copy(scenario_dir %>% path("inner_vect2"), overwrite = T)

  tmp_scenario <- scenario_dir %>%
    sdm_scenario(var_names = list("length_", "dist_dn"))

  merged_area <- a_sdm_area_gridded_area %>%
    merge_scenario(
      to_merge_scenario = tmp_scenario,
      var_names = list("length_", "dist_dn"),
      dir_path = new_sdm_area_dir)

  expect_string(tmp_scenario$sdm_scenario_name, fixed = "scenarios_folder")
  expect_string(tmp_scenario$dir_path, fixed = scenario_dir %>% path_dir())
  expect_equal(merged_area$study_area %>% nrow(), 351)
  expect_int(tmp_scenario$content %>% length(), lower = 2, upper = 2)

  expect_file_exists(
    new_sdm_area_dir %>%
      path("scenarios") %>%
      path("inner_vect1") %>%
      path("hydro_uper_prpy.gpkg")
  )
  expect_file_exists(
    new_sdm_area_dir %>%
      path("scenarios") %>%
      path("inner_vect2") %>%
      path("hydro_uper_prpy.gpkg")
  )

  scenario_dir %>%
    dir_delete()

  new_sdm_area_dir %>%
    dir_delete()
})


