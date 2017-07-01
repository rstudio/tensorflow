context("Imperative")

test_that("Imperative style TensorFlow works", {

  list_a <- list(list(7), list(6))
  list_b <- list(list(6, 7))
  list_c <- list(list(28), list(24))
  list_d <- list(list(42, 49), list(36, 42))

  with_imperative({
    a <- tf$constant(list_a)
    b <- tf$constant(list_b)
    c <- a * 4
    d <- tf$matmul(a, b)
    expect_equal(c, tf$constant(list_c))
    expect_equal(d, tf$constant(list_d))
  }, TRUE)

  with_imperative({
    a <- tf$constant(list_a)
    b <- tf$constant(list_b)
    c <- a * 4
    d <- tf$matmul(a, b)
    expect_equal(c, tf$constant(list_c))
    expect_equal(d, tf$constant(list_d))
  }, FALSE)
})