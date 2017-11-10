context("extract syntax")

source("utils.R")

grab <- function (x) {
  # evaluate tf object x on the graph
  sess <- tf$Session()
  sess$run(x)
}

arr <- function (...) {
  # create an array with the specified dimensions, and fill it with consecutive
  # increasing integers
  dims <- unlist(list(...))
  array(1:prod(dims), dim = dims)
}

randn <- function (...) {
  dim <- c(...)
  array(rnorm(prod(dim)), dim = dim)
}

# check a simple (one-object) expression produces the same result when done on
# an R array, and when done on a tensor, with results ported back to R
# e.g. check_expr(a[1:3], swap = "a")
check_expr <- function (expr, name = "x") {

  call <- substitute(expr)
  r_out <- as.array(eval(expr))

  # swap the array for a constant, run, and convert back to an array
  obj <- get(name, parent.frame())
  swapsies <- list(tf$constant(obj))
  names(swapsies) <- name
  tf_out <- with(swapsies, grab(eval(call)))

  # check it's very very similar
  expect_identical(r_out, tf_out)

}

# capture previous r-like extraction method, set to default, and return later
old_extract_method <- options("tensorflow.one_based_extract")
options(tensorflow.one_based_extract = NULL)


# test indexing for unknown dimensions

test_that('extract works for unknown dimensions', {

  skip_if_no_tensorflow()

  # the output should retain the missing dimension
  x <- tf$placeholder(tf$float64, shape(NULL, 10))
  y1 <- x[, 1]
  y2 <- x[, 1, drop = FALSE]

  expect_identical(dim(y1), list(NULL))
  expect_identical(dim(y2), list(NULL, 1L))

  # expected values with 5 rows
  x_vals <- matrix(rnorm(50), 5, 10)
  y1_exp <- as.array(x_vals[, 1])
  y2_exp <- as.array(x_vals[, 1, drop = FALSE])

  # get observed in values for these
  sess <- tf$Session()
  y1_obs <- sess$run(y1,
                     feed_dict = dict(x = x_vals))
  y2_obs <- sess$run(y2,
                     feed_dict = dict(x = x_vals))

  expect_identical(y1_obs, y1_exp)
  expect_identical(y2_obs, y2_exp)

})

test_that("scalar indexing works", {

  skip_if_no_tensorflow()

  # set up arrays
  x1_ <- arr(3)
  x2_ <- arr(3, 3)
  x3_ <- arr(3, 3, 3)

  # cast to Tensors
  x1 <- tf$constant(x1_)
  x2 <- tf$constant(x2_)
  x3 <- tf$constant(x3_)

  # extract as arrays
  y1_ <- x1_[1]
  y2_ <- x2_[1, 2]
  y3_ <- x3_[1, 2, 3]

  # extract as Tensors
  y1 <- x1[1]
  y2 <- x2[1, 2]
  y3 <- x3[1, 2, 3]

  # they should be equivalent
  expect_equal(y1_, grab(y1))
  expect_equal(y2_, grab(y2))
  expect_equal(y3_, grab(y3))

})

# tests for 0-based indexing
options(tensorflow.one_based_extract = FALSE)


test_that("vector indexing works", {
  skip_if_no_tensorflow()

  # set up arrays
  x1_ <- arr(3)
  x2_ <- arr(3, 3)

  # cast to Tensors
  x1 <- tf$constant(x1_)
  x2 <- tf$constant(x2_)

  # extract as arrays
  y1_ <- x1_[2:3]
  y2_ <- x2_[2:3, 1]

  # extract as Tensors
  y1 <- x1[1:2]
  y2 <- x2[1:2, 0]

  # these should be equivalent (need to coerce R version back to arrays)
  expect_equal(y1_, grab(y1))
  expect_equal(array(y2_), grab(y2))

})

test_that("blank indices retain all elements", {
  skip_if_no_tensorflow()

  # set up arrays
  x1_ <- arr(3)
  x2_ <- arr(3, 3)
  x3_ <- arr(3, 3, 3)
  x4_ <- arr(3, 3, 3, 3)

  # cast to Tensors
  x1 <- tf$constant(x1_)
  x2 <- tf$constant(x2_)
  x3 <- tf$constant(x3_)
  x4 <- tf$constant(x4_)

  # extract as arrays
  y1_ <- x1_[]
  y2_a <- x2_[2:3, ]
  y2_b <- x2_[, 1:2]
  y3_a <- x3_[2:3, 1, ]
  y3_b <- x3_[2:3, , 1]
  y4_ <- x4_[2:3, 1, , 2:3]

  # extract as Tensors
  y1 <- x1[]
  y2a <- x2[1:2, ]  # j missing
  y2b <- x2[, 0:1]
  y3a <- x3[1:2, 0, ]
  y3b <- x3[1:2, , 0]
  y4 <- x4[1:2, 0, , 1:2]

  # these should be equivalent
  expect_equal(y1_, grab(y1))
  expect_equal(y2_a, grab(y2a))
  expect_equal(y2_b, grab(y2b))  #
  expect_equal(y3_a, grab(y3a))
  expect_equal(y3_b, grab(y3b))  #
  expect_equal(y4_, grab(y4))

})

test_that("indexing works within functions", {
  skip_if_no_tensorflow()

  # set up arrays
  x1_ <- arr(3)
  x2_ <- arr(3, 3)
  x3_ <- arr(3, 3, 3)

  # cast to Tensors
  x1 <- tf$constant(x1_)
  x2 <- tf$constant(x2_)
  x3 <- tf$constant(x3_)

  # set up functions
  sub1 <- function (x, a)
    x[a - 1]
  sub2 <- function (x, a, b)
    x[a - 1, b - 1]
  sub3 <- function (x, b, c)
    x[, b - 1, c - 1]  # skip first element

  # extract as arrays
  y1_ <- x1_[1:3]
  y2_ <- x2_[, 1:2]
  y3_a <- x3_[, 1:2, ]
  y3_b <- x3_[, , 1]

  # extract as Tensors
  y1 <- sub1(x1, 1:3)
  y2 <- sub2(x2, 1:3, 1:2)
  y3a <- sub3(x3, 1:2, 1:3)
  y3b <- sub3(x3, 1:3, 1)

  # these should be equivalent
  expect_equal(y1_, grab(y1))
  expect_equal(y2_, grab(y2))
  expect_equal(y3_a, grab(y3a))
  expect_equal(y3_b, grab(y3b))

})


test_that("indexing works with variables", {
  skip_if_no_tensorflow()

  expect_ok <- function (expr) {
    expect_is(expr, "tensorflow.python.framework.ops.Tensor")
  }

  # set up tensors
  x1 <- tf$constant(arr(3))
  x2 <- tf$constant(arr(3, 3))
  x3 <- tf$constant(arr(3, 3, 3))

  # extract with index (these shouldn't error)
  index <- 2
  expect_ok(x1[index])  # i
  expect_ok(x2[, index])  # j
  expect_ok(x3[, , index])  # dots

})

test_that("negative and decreasing indexing errors", {
  skip_if_no_tensorflow()

  # set up Tensors
  x1 <- tf$constant(arr(3))
  x2 <- tf$constant(arr(3, 3))

  # extract with negative indices
  expect_error(x1[-1],
               'negative indexing of Tensors is not currently supported')
  expect_error(x2[1:-2, ],
               'negative indexing of Tensors is not currently supported')
  # extract with decreasing indices
  expect_error(x1[3:2],
               'decreasing indexing of Tensors is not currently supported')
  expect_error(x2[2:1, ],
               'decreasing indexing of Tensors is not currently supported')

})

test_that("incorrect number of indices errors", {
  skip_if_no_tensorflow()

  # set up Tensor
  x <- tf$constant(arr(3, 3, 3))

  # too many
  expect_error(x[1:2, 2, 0:2, 3],
               'incorrect number of dimensions')
  expect_error(x[1:2, 2, 0:2, 3, , ],
               'incorrect number of dimensions')
  expect_error(x[1:2, 2, 0:2, 3, , drop = TRUE],
               'incorrect number of dimensions')
  # too few
  expect_error(x[],
               'incorrect number of dimensions')
  expect_error(x[1:2, ],
               'incorrect number of dimensions')
  expect_error(x[1:2, 2],
               'incorrect number of dimensions')

})

test_that("silly indices error", {
  skip_if_no_tensorflow()

  # set up Tensor
  x <- tf$constant(arr(3, 3, 3))

  # these should all error and notify the user of the failing index
  expect_error(x[1:2, NULL, 2],
               'invalid index - must be numeric and finite')
  expect_error(x[1:2, NA, 2],
               'invalid index - must be numeric and finite')
  expect_error(x[1:2, Inf, 2],
               'invalid index - must be numeric and finite')
  expect_error(x[1:2, 'apple', 2],
               'invalid index - must be numeric and finite')
  expect_error(x[1:2, mean, 2],
               'invalid index - must be numeric and finite')
})

test_that("passing non-vector indices errors", {
  skip_if_no_tensorflow()

  # set up Tensor
  x1 <- tf$constant(arr(3, 3))
  x2 <- tf$constant(arr(3, 3, 3))

  # block indices
  block_idx_1 <- rbind(c(1, 2), c(0, 1))
  block_idx_2 <- rbind(c(1, 2, 1), c(0, 1, 2))

  # indexing with matrices should fail
  expect_error(x1[block_idx_1],
               'only vector indexing of Tensors is currently supported')
  expect_error(x2[block_idx_2],
               'only vector indexing of Tensors is currently supported')

})

test_that("undefined extensions extract", {
  skip_if_no_tensorflow()

  # thanks to @dfalbel https://github.com/rstudio/tensorflow/issues/139
  x <- tf$placeholder(tf$int16, shape = list(NULL, 1L))
  sub <- x[, 0L]

  # also check it returns the correct dimensions to R
  x_ <- matrix(seq_len(3), ncol = 1)
  sess <- tf$Session()
  result <- sess$run(sub, dict(x = x_))
  expectation <- array(x_[, 1, drop = TRUE])
  expect_equal(result, expectation)

})

options(tensorflow.one_based_extract = NULL)

test_that("dim(), length(), nrow(), and ncol() work on tensors", {

  skip_if_no_tensorflow()

  a_matrix <- matrix(rnorm(100), ncol = 2)
  a_tensor <- tf$constant(a_matrix)
  expect_equal(dim(a_matrix), dim(a_tensor))
  expect_equal(length(a_matrix), length(a_tensor))
  expect_equal(nrow(a_matrix), nrow(a_tensor))
  expect_equal(ncol(a_matrix), ncol(a_tensor))

})

# test warnings for extraction that looks like it might be 0-based

test_that('extract warns when indices look 0-based', {

  skip_if_no_tensorflow()

  x <- tf$constant(matrix(0, 2, 2))
  i0 <- 0:1
  i1 <- 1:2

  # explicit 0-indexing shouldn't warn
  options(tensorflow.one_based_extract = FALSE)
  expect_silent(x[i0, i0])

  # explicit 1-indexing shouldn't warn
  options(tensorflow.one_based_extract = TRUE)
  expect_silent(x[i0, i0])

  # default 1-indexing should warn only if there's a zero in there
  options(tensorflow.one_based_extract = NULL)
  expect_silent(x[i1, i1])
  expect_warning(x[i0, i0],
                 "It looks like you might be using 0-based indexing")

})

# reset user's extract method
options(tensorflow.one_based_extract = old_extract_method)
