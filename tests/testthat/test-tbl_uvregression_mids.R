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
  imputed_data <- mice::mice(data, m = 5, maxit = 5, seed = 123, printFlag = FALSE)

  # Run tbl_uvregression on mids object
  tbl <- tbl_uvregression(
    imputed_data,
    method = glm,
    y = outcome,
    exponentiate = TRUE
  )

  # Tests
  expect_true("tbl_uvregression" %in% class(tbl))       # Check tbl_uvregression is part of the class
  # expect_s3_class(tbl, "tbl_uvregression")         # Check correct class
  expect_true("gtsummary" %in% class(tbl))       # Check tbl_summary is part of the class
  expect_named(tbl$table_body,                     # Ensure table has expected columns
               c('tbl_id1', 'variable', 'var_label', 'var_type', 'reference_row', 'row_type', 'header_row', 'coefficients_type', 'coefficients_label', 'label', 'term', 'var_class', 'var_nlevels', 'contrasts', 'contrasts_type', 'estimate', 'std.error', 'statistic', 'b', 'df', 'dfcom', 'fmi', 'lambda', 'm', 'riv', 'ubar', 'n', 'ci', 'conf.low', 'conf.high', 'p.value'),
               ignore.order = FALSE,
               ignore.case = FALSE)

  # Check that no error occurs when printing
  expect_no_error(as_gt(tbl))

  # Additional edge case checks
  # Ensure it works with a single variable
  tbl_single <- tbl_uvregression(
    imputed_data,
    method = glm,
    y = outcome,
    include = predictor1,
    exponentiate = TRUE
  )
  expect_true("tbl_uvregression" %in% class(tbl_single))       # Check tbl_uvregression is part of the class
  # expect_s3_class(tbl_single, "tbl_uvregression")

  # # Test invalid inputs
  # expect_error(tbl_uvregression(data, method = glm, y = outcome ~ .),
  #              "mids object expected")  # Check proper error message
})
