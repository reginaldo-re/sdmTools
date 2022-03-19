test_that("Saving a SDM_area to gpkg file using the same dir path.", {
  a_dir <- tempdir() %>% fs::path(stringi::stri_rand_strings(1,6))
  new_sdm_area <- a_sdm_area %>%
    save_gpkg()

  expect_true(fs::is_file(paste0(fs::path(a_sdm_area$dir_path, a_sdm_area$sdm_area_name), ".gpkg")))
  checkmate::expect_character(waldo::compare(new_sdm_area, a_sdm_area), fixed = "No differences")
})


test_that("Saving a SDM_area to gpkg file.", {
  a_dir <- tempdir() %>% fs::path(stringi::stri_rand_strings(1,6))
  a_dir %>%
    fs::dir_create()

  new_sdm_area <- a_sdm_area %>%
    save_gpkg(new_name = "New filename", dir_path = a_dir)

  expect_true(fs::is_file(paste0(fs::path(a_dir, "New filename"), ".gpkg")))
  expect_length(waldo::compare(new_sdm_area, a_sdm_area), 2)

  a_dir %>%
    fs::dir_delete()
})


test_that("Saving SPDF to gpkg file.", {
  a_dir <- tempdir() %>% fs::path(stringi::stri_rand_strings(1,6))

  new_SPDF <- SPDF %>%
    save_gpkg(new_name = "New filename", dir_path = a_dir)

  expect_true(fs::is_file(fs::path(a_dir, "New filename.gpkg")))
  expect_length(waldo::compare(new_sdm_area, a_sdm_area), 2)
  a_dir %>%
    fs::dir_delete()
})


test_that("Saving SLDF to gpkg file.", {
  a_dir <- tempdir() %>% fs::path(stringi::stri_rand_strings(1,6))
  new_SLDF <- SLDF %>%
    save_gpkg(new_name = "New filename", dir_path = a_dir)
  expect_true(fs::is_file(fs::path(a_dir, "New filename.gpkg")))
  expect_length(waldo::compare(new_SLDF, SLDF), 18)
  a_dir %>%
    fs::dir_delete()
})

test_that("Saving SP to gpkg file.", {
  a_dir <- tempdir() %>% fs::path(stringi::stri_rand_strings(1,6))
  new_SP <- SP %>%
    save_gpkg(new_name = "New filename", dir_path = a_dir)
  expect_true(fs::is_file(fs::path(a_dir, "New filename.gpkg")))
  checkmate::expect_class(new_SP, "SpatialPolygonsDataFrame")
  a_dir %>%
    fs::dir_delete()
})


test_that("Saving SL to gpkg file.", {
  a_dir <- tempdir() %>% fs::path(stringi::stri_rand_strings(1,6))
  new_SL <- SL %>%
    save_gpkg(new_name = "New filename", dir_path = a_dir)
  expect_true(fs::is_file(fs::path(a_dir, "New filename.gpkg")))
  checkmate::expect_class(new_SL, "SpatialLinesDataFrame")
  a_dir %>%
    fs::dir_delete()
})

test_that("Trying to save SPDF to gpkg file with invalid arguments.", {
  expect_error(
    SPDF %>%
      save_gpkg()
  )
})

test_that("Saving SPDF to gpkg file with no crs argument.", {
  a_dir <- tempdir() %>% fs::path(stringi::stri_rand_strings(1,6))
  a_dir %>%
    fs::dir_create()

  new_SPDF <- SPDF %>%
    .sp_save_gpkg(
      new_name = "New filename",
      dir_path = a_dir
    )

  expect_true(fs::is_file(fs::path(a_dir, "New filename.gpkg")))
  expect_length(waldo::compare(new_sdm_area, a_sdm_area), 2)
  a_dir %>%
    fs::dir_delete()
})


test_that("Saving SPDF to gpkg file with a different crs than SPDF.", {
  a_dir <- tempdir() %>% fs::path(stringi::stri_rand_strings(1,6))
  a_dir %>%
    fs::dir_create()

  new_SPDF <- SPDF %>%
    .sp_save_gpkg(
      new_name = "New filename",
      dir_path = a_dir,
      crs = raster::crs("EPSG:5880")
    )

  expect_true(fs::is_file(fs::path(a_dir, "New filename.gpkg")))
  expect_length(waldo::compare(new_sdm_area, a_sdm_area), 2)
  a_dir %>%
    fs::dir_delete()
})

