context("generic methods")

source("utils.R")

as_tensor <- function(...) tf$convert_to_tensor(...)

expect_near <- function(..., tol = 1e-5) expect_equal(..., tolerance = tol)

test_that("log with supplied base works", {

  skip_if_no_tensorflow()

  r <- array(as.double(1:20))
  t <- as_tensor(r, dtype = tf$float32)

  expect_near(r, grab(   log( as_tensor(exp(r)))))
  expect_near(r, grab(log2(  as_tensor(2 ^ r)) ))
  expect_near(r, grab(log10( as_tensor(10 ^ r)) ))

  expect_near(r, grab(   log( exp(t))))
  expect_near(r, grab(  log2(  2 ^ t )))
  expect_near(r, grab( log10( 10 ^ t )))

  # log() dispatches correctly without trying to change base
  expect_identical(grab(tf$math$log(t)), grab(log(t)))

  expect_near(log(r), grab(log(t)))
  expect_near(log(r, base = 3), grab(log(t, base = 3)))

})

test_that("sinpi dispatches correctly", {

  skip_if_no_tensorflow()

  r <- array(seq(0, 4, length.out = 100))
  t <- as_tensor(r, dtype = tf$float32)

  expect_near(sinpi(r), grab( sinpi(t) ))
  expect_near(cospi(r), grab( cospi(t) ))
  expect_near(tanpi(r), grab( tanpi(t) ))

})




