test_that("load_plastic_data & load_nigeria_data returns a data frame", {

  data <- load_plastic_data()

  expect_s3_class(data, "data.frame")
  expect_gt(nrow(data), 0)
  expect_gt(ncol(data), 0)

  data2 <- load_nigeria_data()

  expect_s3_class(data2, "data.frame")
  expect_gt(nrow(data2), 0)
  expect_gt(ncol(data2), 0)
})
