test_that("Merge raster over SDM_area with all unnamed raster variables.", {
  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(
      to_merge_area = system.file("rast_files", package="sdmTools")
    )

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$wc2.0_bio_5m_02 %>% mean() %>% round(2), 11.08)

  expect_string(gridded_area$sdm_area_name, fixed = "Test area")
  expect_file_exists(a_sdm_area_gridded_area$dir_path %>% path("Test area.gpkg"))

  spdf_tmp <- gridded_area$dir_path %>%
    path("Test area.gpkg") %>% readOGR(verbose = F)

  expect_equal(gridded_area$study_area@polygons %>% length(), spdf_tmp@polygons %>% length())
  expect_true((gridded_area$study_area@data %>% as_tibble() == spdf_tmp@data %>% as_tibble()) %>% all())
})


test_that("Merge raster over non gridded SDM_area with all unnamed raster variables.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

  gridded_area <- SPDF %>%
    sdm_area(
      sdm_area_name = "Test area",
      epsg_code = "EPSG:6933",
      resolution = 50000,
      dir_path = a_dir
    ) %>%
    merge_area(
      to_merge_area = system.file("rast_files", package="sdmTools"),
      new_name = "Test"
    )

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$wc2.0_bio_5m_02 %>% mean() %>% round(2), 11.08)

  spdf_tmp <- a_dir %>%
    path("Test.gpkg") %>% readOGR(verbose = F)

  expect_equal(gridded_area$study_area@polygons %>% length(), spdf_tmp@polygons %>% length())
  expect_true((gridded_area$study_area@data %>% as_tibble() == spdf_tmp@data %>% as_tibble()) %>% all())

  a_dir %>%
    dir_delete()
})


test_that("Merge raster over SDM_area with one named raster variables.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(
      to_merge_area = system.file("rast_files/wc2.0_bio_5m_01.tif", package="sdmTools"),
      dir_path = a_dir
    )

  expect_equal(gridded_area$study_area$wc2.0_bio_5m_01 %>% mean() %>% round(2), 24.37)

  spdf_tmp <- a_dir %>%
    path("Test area.gpkg") %>% readOGR(verbose = F)

  expect_equal(gridded_area$study_area@polygons %>% length(), spdf_tmp@polygons %>% length())
  expect_true((gridded_area$study_area@data %>% as_tibble() == spdf_tmp@data %>% as_tibble()) %>% all())


  a_dir %>%
    dir_delete()
})


test_that("Merge raster over SDM_area with all named raster variables.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(
      to_merge_area = system.file("rast_files", package="sdmTools"),
      var_names = list("bio_5m_01", "bio_5m_02"),
      dir_path = a_dir
    )

  expect_equal(gridded_area$study_area$bio_5m_01 %>% mean() %>% round(2), 24.37)
  expect_equal(gridded_area$study_area$bio_5m_02 %>% mean() %>% round(2), 11.08)

  spdf_tmp <- a_dir %>%
    path("Test area.gpkg") %>% readOGR(verbose = F)

  expect_equal(gridded_area$study_area@polygons %>% length(), spdf_tmp@polygons %>% length())
  expect_true((gridded_area$study_area@data %>% as_tibble() == spdf_tmp@data %>% as_tibble()) %>% all())

  a_dir %>%
    dir_delete()
})

test_that("Merge raster over SDM_area with only one named raster variables.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

  gridded_area <- a_sdm_area_gridded_area %>%
    merge_area(
      to_merge_area = system.file("rast_files", package="sdmTools"),
      var_names = list("bio_5m_01"),
      dir_path = a_dir
    )

  expect_equal(gridded_area$study_area$bio_5m_01 %>% mean() %>% round(2), 24.37)

  spdf_tmp <- a_dir %>%
    path("Test area.gpkg") %>% readOGR(verbose = F)

  expect_equal(gridded_area$study_area@polygons %>% length(), spdf_tmp@polygons %>% length())
  expect_true((gridded_area$study_area@data %>% as_tibble() == spdf_tmp@data %>% as_tibble()) %>% all())


  a_dir %>%
    dir_delete()
})

test_that("Merge raster over SDM_area with none raster variables.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(
        to_merge_area = system.file("rast_files", package="sdmTools"),
        var_names = list()
      )
    )
})

test_that("Merge raster over SDM_area with invalid area source.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(
        to_merge_area = system.file("rast", package="sdmTools"),
        var_names = list()
      )
    )
})

test_that("Merge raster over SDM_area with wrong raster variables names.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      merge_area(
        to_merge_area = system.file("rast_files", package="sdmTools"),
        list("invalid_name")
      )
  )
})


test_that("Merge raster over SDM_area containing a non null scenario.", {
  expect_error(
    a_sdm_area_gridded_area %>%
      inset("scenarios", "") %>%
      merge_area(
        to_merge_area = system.file("rast_files", package="sdmTools"),
        list("invalid_name")
      )
  )
})
