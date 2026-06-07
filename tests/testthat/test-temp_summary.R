test_that("temp_summary works for continent summary", {
  result <- temp_summary("continent")
  expect_s3_class(result, "gt_tbl")
})
test_that("temp_summary works for temperature bin summary", {
  result <- temp_summary("temp_bin")
  expect_s3_class(result, "gt_tbl")
})

