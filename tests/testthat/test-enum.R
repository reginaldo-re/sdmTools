test_that("It creates an enum.", {
  a_enum <- create_enum(dummy, cell_id, x_centroid, y_centroid)

  expect_true(
    ((a_enum %>% enum_as_vector() == c("dummy", "cell_id", "x_centroid", "y_centroid")) %>% all())
  )
})

test_that("It tries to create an invalid enum", {
  expect_error(create_enum(dummy, cell_id, x_centroid, y_centroid, dummy))
})

