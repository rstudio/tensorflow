
test_that("TensorShapes are not converted to lists", {

  skip_if_no_tensorflow()

  x <- tf$constant(10, shape = shape(5,10))
  expect_true(x$shape[2] == shape(10))
  expect_identical(x$shape[[2]], 10L)

  y <- tf$TensorShape(shape(5L, 10L))
  expect_s3_class(y, "tensorflow.python.framework.tensor_shape.TensorShape")
  expect_true(y[2] == shape(10))
  expect_identical(y[[2]], 10L)
})
