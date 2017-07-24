context("Imperative")

test_that("Imperative style TensorFlow works", {

  skip_if_no_tensorflow()

  list_a <- list(list(7), list(6))
  list_b <- list(list(6, 7))
  list_c <- list(list(28), list(24))
  list_d <- list(list(42, 49), list(36, 42))
  
  # convert == FALSE
  for(new_step in c(TRUE, FALSE)) {
    res <- tf_imperative({
      a_const <- tf$constant(list_a)
      b_const <- tf$constant(list_b)
      c_const <- a_const * 4
      d_const <- tf$matmul(a_const, b_const)
      expect_equal(c_const, tf$constant(list_c))
      expect_equal(d_const, tf$constant(list_d))
      d_const
    }, new_step = new_step, convert = FALSE)
    expect_true(inherits(res, "tensorflow.python.framework.ops.Tensor"))
  }

  # convert == TRUE
  for(new_step in c(TRUE, FALSE)) {
    res <- tf_imperative({
      a_const <- tf$constant(list_a)
      b_const <- tf$constant(list_b)
      c_const <- a_const * 4
      d_const <- tf$matmul(a_const, b_const)
      expect_equal(c_const, tf$constant(list_c))
      expect_equal(d_const, tf$constant(list_d))
      d_const
    }, new_step = new_step, convert = TRUE)
    expect_equal(res, matrix(unlist(list_d), nrow = 2, byrow = TRUE))
  }
  
  # convert returned list of R objects and tensors to R objects
  for(new_step in c(TRUE, FALSE)) {
    res <- tf_imperative({
      a_const <- tf$constant(list_a)
      b_const <- tf$constant(list_b)
      c_const <- a_const * 4
      d_const <- tf$matmul(a_const, b_const)
      expect_equal(c_const, tf$constant(list_c))
      expect_equal(d_const, tf$constant(list_d))
      list(d1 = d_const, d2 = d_const, d3 = 1)
    }, new_step = new_step, convert = TRUE)
    expect_equal(
      res,
      list(
        d1 = matrix(unlist(list_d), nrow = 2, byrow = TRUE),
        d2 = matrix(unlist(list_d), nrow = 2, byrow = TRUE), 
        d3 = 1)
      )
  }
  
})
