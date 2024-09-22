test_that("logit_diag function works", {

  # Create a data frame
  d <- data.frame(y = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
                  x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
                  x2 = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))

  # Fit a logistic regression model
  model <- glm(y ~ x1 + x2, data = d, family = binomial)

  # Run the logit_diag function
  logit_diag(model, "model", 1)

  # Check that the objects were created in the global environment
  expect_true(exists("model.ors1"))
  expect_true(exists("model.aod1"))
  expect_true(exists("model.rsq1"))
  expect_true(exists("model.hos1"))
  expect_true(exists("model.class1"))
  expect_true(exists("model.tab_acc1"))

  # Check that the objects are of the correct class
  expect_true(inherits(get("model.ors1"), "matrix"))
  expect_true(inherits(get("model.aod1"), "matrix"))
  expect_true(inherits(get("model.rsq1"), "numeric"))
  expect_true(inherits(get("model.hos1"), "list"))
  expect_true(inherits(get("model.class1"), "table"))
  expect_true(inherits(get("model.tab_acc1"), "data.frame"))

  # Check that the objects are not empty
  expect_gt(nrow(get("model.ors1")), 0)
  expect_gt(nrow(get("model.aod1")), 0)
  expect_gt(get("model.rsq1"), 0)
  expect_gt(length(get("model.hos1")), 0)
  expect_gt(sum(get("model.class1")), 0)
  expect_gt(nrow(get("model.tab_acc1")), 0)
})
