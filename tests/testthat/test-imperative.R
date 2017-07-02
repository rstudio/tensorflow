context("Imperative")

test_that("Imperative style TensorFlow works", {

  list_a <- list(list(7), list(6))
  list_b <- list(list(6, 7))
  list_c <- list(list(28), list(24))
  list_d <- list(list(42, 49), list(36, 42))
  
  # convert == FALSE
  for(new_step in c(TRUE, FALSE)) {
    with_imperative({
      a_const <- tf$constant(list_a)
      b_const <- tf$constant(list_b)
      c_const <- a_const * 4
      d_const <- tf$matmul(a_const, b_const)
      expect_equal(c_const, tf$constant(list_c))
      expect_equal(d_const, tf$constant(list_d))
    }, new_step = new_step, convert = FALSE, env = environment())
    expect_false(inherits(a_const, "matrix"))
    expect_false(inherits(b_const, "matrix"))
    expect_false(inherits(c_const, "matrix"))
    expect_false(inherits(d_const, "matrix"))
  }

  # convert == TRUE
  for(new_step in c(TRUE, FALSE)) {
    with_imperative({
      a_const <- tf$constant(list_a)
      b_const <- tf$constant(list_b)
      c_const <- a_const * 4
      d_const <- tf$matmul(a_const, b_const)
      expect_equal(c_const, tf$constant(list_c))
      expect_equal(d_const, tf$constant(list_d))
    }, new_step = new_step, convert = TRUE, env = environment())
    expect_equal(a_const, matrix(unlist(list_a), nrow = 2))
    expect_equal(b_const, matrix(unlist(list_b), nrow = 1))
    expect_equal(c_const, matrix(unlist(list_c), nrow = 2))
    expect_equal(d_const, matrix(unlist(list_d), nrow = 2, byrow = TRUE))
  }
  
})
