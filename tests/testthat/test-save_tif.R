test_that("Saving a SDM_area to a tif file.", {
  new_sdm_area <- a_sdm_area %>%
    save_tif()

  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("geometriaaproximada.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("sigla.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("geocodigo.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("nome.tif")
  )

  checkmate::expect_character(waldo::compare(new_sdm_area, a_sdm_area), fixed = "No differences")
})

test_that("Saving a SDM_area to a tif file giving a different dir_path.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  a_dir %>%
    fs::dir_create()

  new_sdm_area <- a_sdm_area %>%
    save_tif(dir_path = a_dir)

  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("geometriaaproximada.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("sigla.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("geocodigo.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("nome.tif")
  )
  expect_length(waldo::compare(new_sdm_area, a_sdm_area), 1)

  a_dir %>%
    fs::dir_delete()
})

test_that("Saving a SDM_area to a tif file giving a different dir_path and a different file name.", {
  a_dir <- tempdir() %>%
    fs::path(stringi::stri_rand_strings(1,6))

  new_sdm_area <- a_sdm_area %>%
    save_tif("test", a_dir)

  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("geometriaaproximada.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("sigla.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("geocodigo.tif")
  )
  checkmate::expect_file_exists(
    fs::path(new_sdm_area$dir_path) %>%
      fs::path(new_sdm_area$sdm_area_name) %>%
      fs::path("nome.tif")
  )
  expect_length(waldo::compare(new_sdm_area, a_sdm_area), 2)

  a_dir %>%
    fs::dir_delete()
})

