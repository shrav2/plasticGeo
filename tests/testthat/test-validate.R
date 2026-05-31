test_that("checks if neighborhood input is valid", {

  expect_true(validate_neighborhood("Agege"))
  expect_true(validate_neighborhood("Port-Harcourt"))

  expect_error(
    validate_neighborhood(c("Agege", "Port-Harcourt")),
    "`neighborhood` must be one character string."
  )

  expect_error(
    validate_neighborhood("Invalid Neighborhood"),
    "neighborhood` must be a neighborhood in the Nigeria data."
  )

})
