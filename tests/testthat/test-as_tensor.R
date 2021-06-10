test_that("as_tensor works", {

  test_is_tensor <- function(x, dtype) {
    expect_s3_class(x, "tensorflow.tensor")
    if(is.character(dtype))
      expect_true(x$dtype[[dtype]])
    else if(!is.null(dtype))
      expect_true(x$dtype == tf$as_dtype(dtype))
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

})
