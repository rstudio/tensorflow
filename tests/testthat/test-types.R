source("utils.R")

test_that("TensorShapes are converted to lists", {

  skip_if_no_tensorflow()

  x <- tf$constant(10, shape = shape(5,10))
  expect_identical(x$shape[2], list(10L))
  expect_identical(x$shape[[2]], 10L)

  y <- tf$TensorShape(shape(5L, 10L))
  expect_s3_class(y, "tensorflow.python.framework.tensor_shape.TensorShape")
  expect_identical(y[2], list(10L))
  expect_identical(y[[2]], 10L)
})
