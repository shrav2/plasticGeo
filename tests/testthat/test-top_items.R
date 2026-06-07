test_that("top_plastic_items returns top n plastic types for a neighborhood", {
  result <- top_plastic_items("Agege", 3)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_named(result, c("Plastic Type", "Count"))
  expect_true(is.numeric(result$Count))
  expect_equal(result$Count, sort(result$Count, decreasing = TRUE))

  expect_equal(result$`Plastic Type`, c(
    "Plastic Bottles",
    "Wrappers & Labels",
    "Food Containers"
  ))
  expect_equal(
    result$Count,
    c(1458, 533, 430)
    )

  expect_error(top_plastic_items("Not a neighborhood", 3))
  expect_error(top_plastic_items("Agege", 11))
  expect_error(top_plastic_items("Agege", 2.5))
  expect_error(top_plastic_items(c("Agege", "Apapa"), 3))

})
