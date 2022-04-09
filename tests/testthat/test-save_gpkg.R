test_that("Saving a SDM_area to gpkg file using the same dir path.", {
  new_sdm_area <- a_sdm_area %>%
    save_gpkg()

  expect_true(is_file(paste0(path(a_sdm_area$dir_path, a_sdm_area$sdm_area_name), ".gpkg")))
  expect_character(compare(new_sdm_area, a_sdm_area), fixed = "No differences")
})


test_that("Saving a SDM_area to gpkg file.", {
  a_dir <- tempdir() %>% path(stri_rand_strings(1,6))

  new_sdm_area <- a_sdm_area %>%
    save_gpkg(
      sdm_area_name = "New filename.gpkg",
      dir_path = a_dir
    )

  expect_true(is_file(paste0(path(a_dir, "New filename"), ".gpkg")))
  expect_length(compare(new_sdm_area, a_sdm_area), 2)

  a_dir %>%
    dir_delete()
})


test_that("Saving SPDF to gpkg file.", {
  a_dir <- tempdir() %>% path(stri_rand_strings(1,6))

  new_SPDF <- SPDF %>%
    save_gpkg(
      sdm_area_name = "New filename",
      dir_path = a_dir
    )

  expect_true(is_file(path(a_dir, "New filename.gpkg")))
  expect_length(compare(new_SPDF, a_sdm_area$study_area), 0)
  a_dir %>%
    dir_delete()
})


test_that("Saving SLDF to gpkg file.", {
  a_dir <- tempdir() %>% path(stri_rand_strings(1,6))
  new_SLDF <- SLDF %>%
    save_gpkg(
      sdm_area_name = "New filename",
      dir_path = a_dir
    )

  expect_true(is_file(path(a_dir, "New filename.gpkg")))
  expect_length(compare(new_SLDF, SLDF), 0)
  a_dir %>%
    dir_delete()
})

test_that("Saving SP to gpkg file.", {
  a_dir <- tempdir() %>% path(stri_rand_strings(1,6))
  new_SP <- SP %>%
    save_gpkg(
      sdm_area_name = "New filename",
      dir_path = a_dir
    )

  expect_true(is_file(path(a_dir, "New filename.gpkg")))
  expect_class(new_SP, "SpatialPolygonsDataFrame")
  a_dir %>%
    dir_delete()
})


test_that("Saving SL to gpkg file.", {
  a_dir <- tempdir() %>% path(stri_rand_strings(1,6))
  new_SL <- SL %>%
    save_gpkg(
      sdm_area_name = "New filename",
      dir_path = a_dir
    )

  expect_true(is_file(path(a_dir, "New filename.gpkg")))
  expect_class(new_SL, "SpatialLinesDataFrame")
  a_dir %>%
    dir_delete()
})

test_that("Trying to save SPDF to gpkg file with invalid arguments.", {
  expect_error(
    SPDF %>%
      save_gpkg()
  )
})

test_that("Saving SPDF to gpkg file with no crs argument.", {
  a_dir <- tempdir() %>% path(stri_rand_strings(1,6))
  a_dir %>%
    dir_create()

  new_SPDF <- SPDF %>%
    .sp_save_gpkg(
      sdm_area_name = "New filename",
      dir_path = a_dir
    )

  expect_true(is_file(path(a_dir, "New filename.gpkg")))
  expect_length(compare(new_SPDF, SPDF), 0)
  a_dir %>%
    dir_delete()
})


test_that("Saving SPDF to gpkg file with a different crs than SPDF.", {
  a_dir <- tempdir() %>% path(stri_rand_strings(1,6))
  a_dir %>%
    dir_create()

  new_SPDF <- SPDF %>%
    .sp_save_gpkg(
      sdm_area_name = "New filename",
      dir_path = a_dir,
      crs = crs("EPSG:5880")
    )

  expect_true(is_file(path(a_dir, "New filename.gpkg")))
  expect_true(compareCRS(new_SPDF, crs("EPSG:5880")))
  a_dir %>%
    dir_delete()
})

