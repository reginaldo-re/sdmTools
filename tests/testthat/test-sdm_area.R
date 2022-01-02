test_that("Trying to create an invalid objects SDM_area.", {
  expect_error(NULL %>% sdm_area("Test area"))
  expect_error(NULL %>% sdm_area("Test area", NULL))
  expect_error(NULL %>% sdm_area("Test area", NULL, NULL))
  expect_error(NULL %>% sdm_area("Test area", NULL, c(1,1)))
  expect_error(NULL %>% sdm_area("Test area", "EPSG:6933", c(1,1)))

  expect_error(SP %>% sdm_area("Test area"))
  expect_error(SP %>% sdm_area("Test area", "EPSG:XXXX"))
  expect_error(SP %>% sdm_area("Test area", "EPSG:6933"))
  expect_error(SP %>% sdm_area("Test area", "EPSG:6933", c(1)))
  expect_error(SP %>% sdm_area("Test area", "EPSG:6933", c(0,2)))
  expect_error(SP %>% sdm_area("Test area", "EPSG:XXXX", c(0,2)))
  expect_error(SP %>% sdm_area(a_res= c(0,2)))

  expect_error("123" %>% sdm_area("Test area", "EPSG:6933", c(0,2)))
  expect_error("zzzzz" %>% sdm_area("Test area", "EPSG:6933", c(50000, 50000)))
  expect_error(".gpkg" %>% sdm_area("Test area", "EPSG:6933", c(50000, 50000)))
})



