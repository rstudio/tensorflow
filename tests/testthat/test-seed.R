test_that("use_session_with_seed works", {
  skip_if_no_tensorflow()

  f <- function() {
    library(keras)
    use_session_with_seed(seed = 1)
    model <- keras_model_sequential() %>%
      layer_dense(units = 1)
    predict(model, matrix(1, ncol = 1))
  }

  run1 <- callr::r(f)
  run2 <- callr::r(f)

  expect_equal(run1, run2)
})
