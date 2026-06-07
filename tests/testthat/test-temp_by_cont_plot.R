test_that("temp_by_cont_plot creates a scatterplot", {
  result <- temp_by_cont_plot()

  expect_s3_class(result, "ggplot")
  expect_equal(length(result$layers), 2)
  expect_equal(result$labels$title, "Temperature vs Plastic Collection (Log Scale)")
  expect_equal(result$labels$x, "Mean Annual Temperature (°C)")
  expect_equal(result$labels$y, "Total Plastic Collected (Log Scale)")
  expect_equal(result$labels$colour, "Continent")

})

