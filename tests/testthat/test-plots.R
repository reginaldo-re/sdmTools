test_that("Plot an area map from a study area.", {
  a_ggplot<- a_sdm_area %>% area_geomap()

  expect_s3_class(a_ggplot, "ggplot")
})

test_that("Ploat an area map from a gridded study area.", {
  a_ggplot<- a_sdm_area_gridded_area %>% area_geomap(title = "New title")

  expect_s3_class(a_ggplot, "ggplot")
})

test_that("Ploat an grid map from a gridded study area.", {
  a_ggplot<- a_sdm_area %>% grid_geomap(a_sdm_area_gridded_area)

  expect_s3_class(a_ggplot, "ggplot")
})
