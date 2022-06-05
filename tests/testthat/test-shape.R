test_that("shape() works", {


  skip_if_no_tensorflow()


  expect_tensor_shape <- function(x, dims) {
    expect_s3_class(x, "tensorflow.python.framework.tensor_shape.TensorShape")
    if (missing(dims))
      return()

    if (is.null(dims)) {
      expect_null(x$rank)
      expect_null(x$dims)
    } else {
      expect_identical(as.list(x$as_list()), dims)
    }
  }

  x <- shape(1, NA, 2, NULL, 3)
  expect_tensor_shape(x, list(1L, NULL, 2L, NULL, 3L))

  expect_identical(as.list(x), list(1L, NULL, 2L, NULL, 3L))

  expect_true(shape()          == tf$TensorShape(list()))
  expect_true(shape(dims = NULL) == tf$TensorShape(NULL))


  # --- construct ---
  expect_tensor_shape(shape()                , list())
  expect_tensor_shape(shape(NULL)            , list(NULL))
  expect_tensor_shape(shape(dims = NULL)     , NULL)
  expect_tensor_shape(shape(3, 4)            , list(3L, 4L))
  expect_tensor_shape(shape(NA, 4)           , list(NULL, 4L))
  expect_tensor_shape(shape(dims = c(NA, 4)) , list(NULL, 4L))

  # --- inspect ---
  expect_identical(length(shape(dims = NULL)), NA_integer_, )
  expect_identical(length(shape(1, 2, 3, NA)), 4L)


  # ---convert ---
  x <- shape(dims = list(3L, 5L))
  expect_identical(as.list(x)   , list(3L, 5L))
  expect_identical(as.integer(x), c(3L, 5L))
  expect_identical(as.numeric(x), c(3, 5))
  expect_identical(as.double(x) , c(3, 5))

  x <- shape(NA, 3)
  expect_identical(as.list(x), list(NULL, 3L))
  expect_identical(as.integer(x), c(NA, 3L))
  expect_identical(as.double(x),  c(NA, 3))

  x2 <- as_tensor(shape(NA, 3))
  expect_equal(x2$numpy(), array(c(-1L, 3L)))
  expect_identical(x2$dtype$name, "int32")
  expect_identical(as.list(x2$shape), list(2L))

  x <- shape(dims = NULL)
  expect_error(as.list(x))
  expect_error(as.numeric(x))
  expect_error(as_tensor(x))

  x <- shape(NA, 3)
  # as_tensor() converts undefined dims to -1
  expect_identical(as.integer(as_tensor(x)), c(-1L, 3L))
  # can round trips shape -> tensor -> shape
  expect_tensor_shape(shape(dims = as_tensor(x)), list(NULL, 3L))


  # --- compare ---
  # Fully known shapes return TRUE if and only if each element is equal
  expect_true(shape(3, 4) == shape(3, 4)) # TRUE
  expect_false(shape(3, 4) == shape(4, 4)) # FALSE

  # Partially-known shapes always return FALSE
  if (tf_version() >= "2.9")
    expect_true(shape(NA, 4) == shape(NA, 4))
  else
    expect_false(shape(NA, 4) == shape(NA, 4))

  expect_false(shape(NA, 4) == shape(3, 4))

  # Two unknown shapes, return TRUE
  expect_true(shape(dims = NULL) == shape(dims = NULL))

  # Comparing an unknown shape to a partially or fully defined shape returns FALSE
  expect_false(shape(dims = NULL) == shape(NULL))
  expect_false(shape(dims = NULL) == shape(4))

  if(tf_version() < "2.9") {
  # in 2.9, != is just negation of ==
  # prior versions: != is mostly the inverse of ==, with one difference:
  # it raises an error when comparing a fully unknown shapes
  expect_error(shape(dims = NULL) != shape(dims = NULL)) # ValueError: The inequality of unknown TensorShapes is undefined.
  expect_error(shape(dims = NULL) != shape())            # ValueError: The inequality of unknown TensorShapes is undefined.
  }


  # --- extract or replace ---
  # regular R-list semantics for `[`, `[[`, `[<-`, `[[<-`
  x <- shape(1, 2, 3)
  expect_tensor_shape(x[1], list(1L))
  expect_identical(x[[1]], 1L)

  x_slice <- x[2:3]
  expect_tensor_shape(x_slice, list(2L, 3L))
  expect_true(x_slice == c(2, 3))
  expect_true(x_slice == x[-1])

  x <- shape(1, 2, 3)
  x[1] <- 11
  expect_tensor_shape(x, list(11L, 2L, 3L))
  expect_true(x == c(11, 2, 3))

  x[1] <- shape(22)
  expect_tensor_shape(x, list(22L, 2L, 3L))
  expect_true(x == c(22, 2, 3))

  x[1] <- list(33)
  expect_tensor_shape(x, list(33L, 2L, 3L))
  expect_true(x == c(33, 2, 3))

  x[[1]] <- 44
  expect_true(x == c(44, 2, 3))
  x[1:2] <- c(NA, 99)
  expect_identical(as.numeric(x), c(NA, 99, 3))
  x[1:2] <- shape(33, 44)
  expect_tensor_shape(x, list(33L, 44L, 3L))
  expect_identical(as.numeric(x), c(33, 44, 3))

  # --- concatenate ---
  x <-
    c(shape(1), shape(2, 3), shape(4, NA)) # TensorShape([1, 2, 3, 4, None])
  expect_identical(as.list(x), list(1L, 2L, 3L, 4L, NULL))

  # --- merge ---
  x <- merge(shape(NA, 2),
             shape(1 , 2)) # TensorShape([1, 2])
  expect_tensor_shape(x, list(1L, 2L))
  expect_true(x == c(1, 2))

  expect_error(merge(shape(2, 2),
                     shape(1, 2))) # ValueError: Shapes (2, 2) and (1, 2) are not compatible


  expect_output(print(shape(3)), "TensorShape([3])", fixed = TRUE)
  expect_output(print(shape(3, NA)), "TensorShape([3, None])", fixed = TRUE)
  expect_output(print(shape(3, NULL)), "TensorShape([3, None])", fixed = TRUE)

  expect_equal(format(shape(3)), "(3)")
  expect_equal(format(shape(3, NA)), "(3, NA)")
  expect_equal(format(shape(3, NULL)), "(3, NA)")

  # shape() can accept tf.TensorShapes, and flatten them
  expect_equal(as.list(shape(shape(3))), list(3L))
  expect_equal(as.list(shape(shape(3, 4))), list(3L, 4L))
  expect_equal(as.list(shape(shape(3, 4), 5)), list(3L, 4L, 5L))
  expect_equal(as.list(shape(NA, shape(3, 4), 5)), list(NULL, 3L, 4L, 5L))

})
