test_that("tbl_uvregression.mids works with mids objects", {
  # Load necessary libraries
  library(gtsummary)
  library(mice)
  library(testthat)

  # Create a sample dataset with missing values
  set.seed(123)
  data <- data.frame(
    outcome = rbinom(100, 1, 0.5),
    predictor1 = rnorm(100),
    predictor2 = rnorm(100)
  )
  data$predictor2[sample(1:100, 20)] <- NA  # Introduce missing values

  # Create mids object using mice
  imputed_data <- mice(data, m = 5, maxit = 5, seed = 123, printFlag = FALSE)

  # Run tbl_uvregression on mids object
  tbl <- tbl_uvregression(
    imputed_data,
    method = glm,
    y = outcome ~ .,
    exponentiate = TRUE
  )

  # Tests
  expect_s3_class(tbl, "tbl_uvregression")         # Check correct class
  expect_true("tbl_summary" %in% class(tbl))       # Check tbl_summary is part of the class
  expect_named(tbl$table_body,                     # Ensure table has expected columns
               c("variable", "estimate", "ci", "p.value"))

  # Test pool_and_tidy_mice integration
  expect_true(any(grepl("pooled", tolower(names(tbl)))))  # Confirm pooled results

  # Check that no error occurs when printing
  expect_no_error(as_gt(tbl))

  # Additional edge case checks
  # Ensure it works with a single variable
  tbl_single <- tbl_uvregression(
    imputed_data,
    method = glm,
    y = outcome ~ predictor1,
    exponentiate = TRUE
  )
  expect_s3_class(tbl_single, "tbl_uvregression")

  # # Test invalid inputs
  # expect_error(tbl_uvregression(data, method = glm, y = outcome ~ .),
  #              "mids object expected")  # Check proper error message
})
