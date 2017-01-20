context("Linear Regression")

test_that("tf_linear_regression() produces similar fits to lm()", {

  tf_model <- tf_linear_regression(mpg ~ drat, data = mtcars)
  rs_model <- lm(mpg ~ drat, data = mtcars)

  tf_coef <- coef(tf_model)
  rs_coef <- coef(rs_model)

  # TODO: the values are close-ish, but not as close as one might expect?
  expect_true(
    all(abs(tf_coef - rs_coef) < 0.5),
    "R and TensorFlow produce similar linear model fits"
  )
})
