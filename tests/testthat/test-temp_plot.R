test_that("plot_temp_plastic_hex creates a hexbin plot", {
  data <- tibble::tibble(
    Country = c("Nigeria", "Ghana", "Kenya", "Brazil", "India"),
    AnnualMeanTemp = c(27, 26, 24, 22, 25),
    grand_total = c(150000, 50000, 30000, 70000, 40000)
  )

  result <- plot_temp_plastic_hex(data)

  expect_s3_class(result, "ggplot")
  expect_equal(result$labels$x, "Mean Annual Temperature")
  expect_equal(result$labels$y, "Log Total Plastic Collected")
  expect_equal(result$labels$fill, "Number of Countries")
})
