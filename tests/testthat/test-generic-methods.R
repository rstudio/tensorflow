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

test_generic <- function(name, fun, x, y = NULL) {
  test_that(paste("Generic", name, "works"), {

    skip_if_no_tensorflow()

    if (is.null(y)) {
      out_r <- fun(x)
      out_tf <- fun(tf$constant(x))
    } else {
      out_r <- fun(x, y)
      out_tf <- fun(tf$constant(x), tf$constant(y))
    }

    if (!tf$executing_eagerly() && inherits(out_tf, "tensorflow.tensor")) {

      if (tf_version() >= "1.14")
        sess <- tf$compat$v1$Session()
      else
        sess <- tf$Session()


      out_tf <- sess$run(out_tf)
    }

    if (inherits(out_tf, "python.builtin.object"))
      out_tf <- out_tf$numpy()

    expect_equal(out_tf, out_r)
  })
}

tensor_generics <- list(
  dim,
  length
)

for (fun in tensor_generics) {
  test_generic(deparse(fun), fun, array(1000, dim = c(1,2,3)))
  test_generic(deparse(fun), fun, array(1000, dim = c(1)))
  test_generic(deparse(fun), fun, matrix(1:100))
}


test_that("lenght works", {
  skip_if_no_tensorflow()
  expect_identical(length(1L), length(tf$constant(1L)))
})


logical_generics <- list(
  `==`,
  `!=`,
  `<`,
  `<=`,
  `>`,
  `>=`
)

for (fun in logical_generics) {
  test_generic(
    deparse(fun), fun,
    x = array(runif(1000), dim = c(1,2,3)),
    y = array(runif(1000), dim = c(1,2,3))
  )

  test_generic(
    deparse(fun), fun,
    x = array(1, dim = c(1,2,3)),
    y = array(1, dim = c(1,2,3))
  )
}

bool_operators <- list(
  `&`,
  `|`
)

for (fun in bool_operators) {
  test_generic(deparse(fun), fun, TRUE, FALSE)
  test_generic(deparse(fun), fun, TRUE, FALSE)
}

test_generic("!", `!`, TRUE)
test_generic("!", `!`, FALSE)

complex_generics <- list(
  Re,
  Im,
  Conj,
  Arg,
  Mod
)

for (fun in complex_generics) {
  test_generic(deparse(fun), fun, 1 + 2i)
}

binary_generics <- list(
  `+`,
  `-`,
  `*`,
  `/`,
  `%/%`,
  `%%`,
  `^`
)

for (fun in binary_generics) {
  test_generic(
    deparse(fun), fun,
    x = array(runif(1000), dim = c(1,2,3)),
    y = array(runif(1000), dim = c(1,2,3))
  )

  test_generic(
    deparse(fun), fun,
    x = array(1, dim = c(1,2,3)),
    y = array(1, dim = c(1,2,3))
  )
}

generics <- list(
  abs,
  sign,
  sqrt,
  floor,
  ceiling,
  round,
  exp,
  log,
  log1p,
  log2,
  log10,
  cos,
  sin,
  tan,
  sinpi,
  cospi,
  tanpi,
  acos,
  asin,
  atan,
  lgamma,
  digamma
)

for (fun in generics) {
  test_generic(
    deparse(fun), fun,
    x = array(runif(1000), dim = c(1,2,3))
  )

  test_generic(
    deparse(fun), fun,
    x = array(1, dim = c(1,2,3))
  )
}







