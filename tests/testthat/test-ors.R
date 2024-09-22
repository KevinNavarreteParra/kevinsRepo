test_that("general ors tests", {

  # Example data
  data <- data.frame(y = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
                     x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
  # Fit a logistic regression model
  model <- glm(y ~ x, data = data, family = binomial)
  # Calculate odds ratios and confidence intervals
  result <- ors(model)

  # check if result has 3 columns
  expect_equal(ncol(result), 3)

  # check if result has 2 rows
  expect_equal(nrow(result), 2)

  # check if result has column names
  expect_equal(colnames(result), c("OR", "2.5 %", "97.5 %"))

  # check if result has row names
  expect_equal(rownames(result), c("(Intercept)", "x"))

  # check if result has correct values

  expect_equal(result[1, 2], exp(confint(model)[1, 1]))
  expect_equal(result[2, 2], exp(confint(model)[2, 1]))

  expect_equal(result[1, 3], exp(confint(model)[1, 2]))
  expect_equal(result[2, 3], exp(confint(model)[2, 2]))

})
