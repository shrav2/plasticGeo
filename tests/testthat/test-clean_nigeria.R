test_that("clean_nigeria returns a tibble with the correct columns", {
  result <- clean_nigeria()

  expect_s3_class(result, "tbl_df")
  expect_true("Neighborhood" %in% names(result))
  expect_true("plastic_type" %in% names(result))
  expect_true("Count" %in% names(result))
  expect_false(any(stringr::str_detect(result$plastic_type, "^SUM_")))
  expect_true(is.numeric(result$Count))

})
