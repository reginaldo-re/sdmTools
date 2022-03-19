test_that("Saving a SDM_area to a tif file.", {
  new_sdm_area <- a_sdm_area %>%
    save_tif()

  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("geometriaaproximada.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("sigla.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("geocodigo.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("nome.tif")
  )

  expect_character(compare(new_sdm_area, a_sdm_area), fixed = "No differences")
})

test_that("Saving a SDM_area to a tif file giving a different dir_path.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

  a_dir %>%
    dir_create()

  new_sdm_area <- a_sdm_area %>%
    save_tif(dir_path = a_dir)

  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("geometriaaproximada.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("sigla.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("geocodigo.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("nome.tif")
  )
  expect_length(compare(new_sdm_area, a_sdm_area), 1)

  a_dir %>%
    dir_delete()
})

test_that("Saving a SDM_area to a tif file giving a different dir_path and a different file name.", {
  a_dir <- tempdir() %>%
    path(stri_rand_strings(1,6))

  new_sdm_area <- a_sdm_area %>%
    save_tif("test", a_dir)

  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("geometriaaproximada.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("sigla.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("geocodigo.tif")
  )
  expect_file_exists(
    path(new_sdm_area$dir_path) %>%
      path(new_sdm_area$sdm_area_name) %>%
      path("nome.tif")
  )
  expect_length(compare(new_sdm_area, a_sdm_area), 2)

  a_dir %>%
    dir_delete()
})

