test_that("analyze_plastic returns a gt table with regression results for log-transformed data", {

  result <- analyze_plastic()
  table_data <- result[["_data"]]

  expect_s3_class(result, "gt_tbl")
  expect_equal(nrow(table_data), 2)

  expect_named(
    table_data,
    c("term", "estimate", "std.error", "statistic", "p.value")
  )

  expect_true(is.numeric(table_data$estimate))

  expect_equal(round(table_data$estimate, 2), c(6.21, 0.05))
  expect_equal(round(table_data$statistic, 2), c(9.16, 1.36))
  expect_equal(round(table_data$p.value, 3), c(0.000, 0.177))
})


test_that("analyze_log_plastic returns a gt table with regression results for raw data", {

  result <- analyze_plastic(log_transform = FALSE)
  table_data <- result[["_data"]]

  expect_s3_class(result, "gt_tbl")
  expect_equal(nrow(table_data), 2)

  expect_named(
    table_data,
    c("term", "estimate", "std.error", "statistic", "p.value")
  )

  expect_true(is.numeric(table_data$estimate))

  expect_equal(round(table_data$estimate, 2), c(-1535.62, 536.43))
  expect_equal(round(table_data$statistic, 2), c(-0.29, 2.04))
  expect_equal(round(table_data$p.value, 3), c(0.771, 0.044))
})

