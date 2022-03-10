test_that("as_tensor works", {

  skip_if_no_tensorflow()

  test_is_tensor <- function(x, dtype=NULL) {
    expect("tensorflow.tensor" %in% class(x),
           paste("Wrong S3 class, expected 'tensorflow.tensor', actual", class(x)))

    failure_message <- sprintf(
      "wrong type attributes. expected '%s', encountered '%s'", dtype, x$dtype)
    if(is.character(dtype))
      expect(x$dtype[[dtype]], failure_message)
    else if(!is.null(dtype))
      expect(x$dtype == tf$as_dtype(dtype), failure_message)
  }

  test_is_tensor(as_tensor(3), 'is_floating')
  test_is_tensor(as_tensor(3L), tf$int32)
  test_is_tensor(as_tensor("foo"), tf$string)
  test_is_tensor(as_tensor(TRUE), tf$bool)
  test_is_tensor(as_tensor(1+1i), 'is_complex')

  test_is_tensor(as_tensor(3L, tf$int32)   , tf$int32)
  test_is_tensor(as_tensor(3L, tf$int64)   , tf$int64)
  test_is_tensor(as_tensor(3L, tf$float32) , tf$float32)
  test_is_tensor(as_tensor(3L, tf$float64) , tf$float64)
  test_is_tensor(as_tensor(3L, tf$int8)    , tf$int8)

  test_is_tensor(as_tensor(3.0, tf$float32), tf$float32)
  test_is_tensor(as_tensor(3.0, tf$float64), tf$float64)
  test_is_tensor(as_tensor(3.0, tf$int32)  , tf$int32)
  test_is_tensor(as_tensor(3.0, tf$int64)  , tf$int64)
  test_is_tensor(as_tensor(3.0, tf$int8)   , tf$int8)

  # currently scalars -> float32; arrays -> float64
  test_is_tensor(as_tensor(arr(3))       , 'is_floating')
  test_is_tensor(as_tensor(arr(3, 3))    , 'is_floating')
  test_is_tensor(as_tensor(arr(3, 3, 3)) , 'is_floating')

  x <- tf$constant(3)
  test_is_tensor(as_tensor(x, tf$int32), tf$int32)
  test_is_tensor(as_tensor(x, tf$int64), tf$int64)

  shps <- list(c(-1, 4),
               shape(-1, 4),
               c(NA, 4),
               list(NULL, 4),
               list(3, 4),
               as_tensor(c(-1L, 4L)))
  for (shp in shps) {
    x <- as_tensor(1:12, shape = shp)
    expect_identical(dim(x), c(3L, 4L))
  }


  # can call tf$fill() to expand scalars
  expect_identical(
    tf$convert_to_tensor(array(0, c(3,4)))$numpy(),
    as_tensor(0, shape = c(3, 4))$numpy()
  )

  expect_identical(
    tf$zeros(shape(3,4))$numpy(),
    as_tensor(0, shape = c(3, 4))$numpy()
  )

  # atomic vectors are converted to python as R arrays
  x <- as_tensor(1:6, shape = c(2, 3))
  expect_equal(x$dtype$name, "int32") # a list of ints would convert to int64


  i <- as_tensor(seq(4))
  j <- as_tensor(rep(3L, 4))
  r <- as_tensor(i >= j, dtype = "int32")
  expect_equal(r$dtype$name, "int32") # casting bool to int doesn't do a saturate cast

  r <- as_tensor(i-1, "bool")
  expect_equal(as.logical(r), c(FALSE, TRUE, TRUE, TRUE))

  x <- as_tensor(c(-1, 0, 1, 254, 255, 256), "uint8")
  expect_equal(as.integer(x), as.integer(c(0,0, 1, 254, 255, 255)))

  x <- as_tensor(c(-1, 0, 1, 254, 255, 256))
  x <-  as_tensor(x, "uint8")
  expect_equal(as.integer(x), as.integer(c(0,0, 1, 254, 255, 255)))

  # tf.fill works with a scalar tensor
  x <- as_tensor(as_tensor(3), shape = 32)
  expect_equal(dim(x), 32L)
  expect_equal(as.numeric(x), rep(3, 32))

  # supplied shape can be a tensor
  x <- as_tensor(as_tensor(3), shape = as_tensor(32L))
  expect_equal(dim(x), 32L)
  expect_equal(as.numeric(x), rep(3, 32))

})


test_that("conversion of tf.string dtype tensors", {

  xx <- list(array("foo"),
             array(c("foo", "bar", "baz")),
             array(as.character(1:12), c(3, 4)),
             array(as.character(1:12), c(2, 3, 2)))

  for (x in xx)
    expect_identical(x, as.array(as_tensor(x)))

  for (x in xx)
    expect_identical(as.character(x),
                     as.character(as_tensor(x)))

  })
