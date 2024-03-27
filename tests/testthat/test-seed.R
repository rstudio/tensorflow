# skip("use_session_with seed doesn't work with TF >= 2.3")
if (tf_version() < "2.3")
test_that("use_session_with_seed works", {
  skip_if_no_tensorflow()


  f <- function() {
    library(keras3)
    use_session_with_seed(seed = 1)
    model <- keras_model_sequential() %>%
      layer_dense(units = 1)
    predict(model, matrix(1, ncol = 1))
  }

  run1 <- callr::r(f)
  run2 <- callr::r(f)

  expect_equal(run1, run2)
})

test_that("set_random_seed", {

  skip_if_no_tensorflow()

  if (tf_version() < "2.0")
    skip("set_random_seed only works for TF >= 2.0")

  f <- function() {
    library(keras3)
    tensorflow::set_random_seed(seed = 1)
    model <- keras_model_sequential() %>%
      layer_dense(units = 1)
    predict(model, matrix(1, ncol = 1))
  }

  run1 <- callr::r(f)
  run2 <- callr::r(f)

  expect_equal(run1, run2)
})
