test_that("Convert a SDM_area data to dataframe.", {
  n_rows <- a_sdm_area_gridded_area %>%
    sdm_tidy() %>%
    nrow()

  expect_equal(n_rows, 18170)
})


test_that("Convert a SDM_area data to dataframe using a region.", {
  a_df <- a_sdm_area_gridded_area %>%
    sdm_tidy(region = "cell_id")

  expect_equal(a_df %>% nrow(), 18170)
  expect_equal(a_df %>% ncol(), 13)
})

test_that("Convert a SPDF data to dataframe using a region.", {
  a_df <- SPDF %>%
    sdm_tidy(region = "cell_id")

  expect_equal(a_df %>% nrow(), 689648)
  expect_equal(a_df %>% ncol(), 7)
})
