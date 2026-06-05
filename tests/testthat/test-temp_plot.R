test_that("plot_temp_plastic_hex creates a hexbin plot", {
  result <- plot_temp_plastic_hex()

  expect_s3_class(result, "ggplot")
  expect_equal(result$labels$x, "Mean Annual Temperature")
  expect_equal(result$labels$y, "Log Total Plastic Collected")
  expect_equal(result$labels$fill, "Number of Countries")
})
