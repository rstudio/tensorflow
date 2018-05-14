context("extract syntax")

source("utils.R")

.SESS <- tf$Session()

grab <- function(x) .SESS$run(x)


null_out_all_extract_opts <- function() {
  opts <- options()
  opts[grepl("^tensorflow[.]extract", names(opts))] <- list(NULL)
  options(opts)
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

reset_warnings <- function() {
  e <- tensorflow:::warned_about
  e$negative_indices <- FALSE
  e$tensors_passed_asis <- FALSE
}


# capture previous r-like extraction method, set to default, and return later
# old_extract_method <- options("tensorflow.extract.one_based")
# options(tensorflow.extract.one_based = NULL)
# options(tensorflow.extract.style = 'R')


# test indexing for unknown dimensions

test_that('extract works for unknown dimensions', {

  skip_if_no_tensorflow()

  oopt <- options(tensorflow.extract.style = "R")

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

  options(oopt)
})

test_that("scalar indexing works", {

  skip_if_no_tensorflow()
  oopt <- options(tensorflow.extract.style = "R")
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

  options(oopt)
})

# tests for 0-based indexing

# options(tensorflow.extract.one_based = FALSE)

test_that("vector indexing works", {
  skip_if_no_tensorflow()

  oopt <- options(tensorflow.extract.one_based = FALSE)
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

  options(oopt)
})

test_that("blank indices retain all elements", {
  skip_if_no_tensorflow()

  oopt <- options(tensorflow.extract.one_based = FALSE)

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

  options(oopt)
})

test_that("indexing works within functions", {
  skip_if_no_tensorflow()

    # tensorflow.extract.style = "python",
  oopt <- options(tensorflow.extract.one_based = FALSE)

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

  options(oopt)
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

test_that("indexing with negative sequences errors", {
  skip_if_no_tensorflow()

  oopt <- options(tensorflow.extract.style = "R")
  # set up Tensors
  x1 <- tf$constant(arr(3))
  x2 <- tf$constant(arr(3, 3))

  # extract with negative indices (where : is not the top level call)
  expect_error(x1[-(1:2)], 'positive')
  expect_error(x2[-(1:2), ], 'positive')

  options(oopt)
})

test_that("incorrect number of indices errors", {
  skip_if_no_tensorflow()

  # set up Tensor
  x <- tf$constant(arr(3, 3, 3))
  # options(tensorflow.extract.one_based = TRUE)
  # too many
  expect_error(x[1:2, 2, 1:2, 3],
               'Incorrect number of dimensions')
  expect_error(x[1:2, 2, 1:2, 3, , ],
               'Incorrect number of dimensions')
  expect_error(x[1:2, 2, 1:2, 3, , drop = TRUE],
               'Incorrect number of dimensions')
  # too few
  expect_warning(x[],
               'Incorrect number of dimensions')
  expect_warning(x[1:2, ],
               'Incorrect number of dimensions')
  expect_warning(x[1:2, 2],
               'Incorrect number of dimensions')

})

test_that("silly indices error", {
  skip_if_no_tensorflow()

  # set up Tensor
  x <- tf$constant(arr(3, 3, 3))

  # these should all error and notify the user of the failing index
  expect_error(x[1:2, NA, 2], 'NA')
  expect_error(x[1:2, Inf, 2], 'Inf')
  expect_error(x[1:2, 'apple', 2], 'character')
  expect_error(x[1:2, mean, 2], 'function')
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
               'not currently supported')
  expect_error(x2[block_idx_2],
               'not currently supported')

})

test_that("undefined extensions extract", {
  skip_if_no_tensorflow()
  oopt <- options(tensorflow.extract.style = 'python')

  # thanks to @dfalbel https://github.com/rstudio/tensorflow/issues/139
  x <- tf$placeholder(tf$int16, shape = list(NULL, 1L))
  sub <- x[, 0L]

  # also check it returns the correct dimensions to R
  x_ <- matrix(seq_len(3), ncol = 1)
  sess <- tf$Session()
  result <- sess$run(sub, dict(x = x_))
  expectation <- array(x_[, 1, drop = TRUE])
  expect_equal(result, expectation)

  options(oopt)

})


test_that("dim(), length(), nrow(), and ncol() work on tensors", {

  skip_if_no_tensorflow()

  a_matrix <- matrix(rnorm(100), ncol = 2)
  a_tensor <- tf$constant(a_matrix)
  expect_equal(dim(a_matrix), dim(a_tensor))
  expect_equal(length(a_matrix), length(a_tensor))
  expect_equal(nrow(a_matrix), nrow(a_tensor))
  expect_equal(ncol(a_matrix), ncol(a_tensor))

})



test_that("all_dims()", {

  skip_if_no_tensorflow()

  x1.r <- arr(3)
  x2.r <- arr(3, 3)
  x3.r <- arr(3, 3, 3)
  x4.r <- arr(3, 3, 3, 3)

  x1.t <- tf$constant(x1.r)
  x2.t <- tf$constant(x2.r)
  x3.t <- tf$constant(x3.r)
  x4.t <- tf$constant(x4.r)

  expect_equal(grab( x1.t[all_dims()] ), x1.r[])

  options(tensorflow.extract.one_based = TRUE)
  # as.array() because tf returns 1d arrays, not bare atomic vectors
  expect_equal(grab( x2.t[all_dims()]    ), as.array( x2.r[,]  ))
  expect_equal(grab( x2.t[1, all_dims()]  ), as.array( x2.r[1,] ))
  expect_equal(grab( x2.t[ all_dims(), 1] ), as.array( x2.r[,1] ))

  expect_equal(grab( x3.t[all_dims()]       ), as.array( x3.r[,,]   ))
  expect_equal(grab( x3.t[1, all_dims()]    ), as.array( x3.r[1,,]  ))
  expect_equal(grab( x3.t[1, 1, all_dims()] ), as.array( x3.r[1,1,] ))
  expect_equal(grab( x3.t[1, all_dims(), 1] ), as.array( x3.r[1,,1] ))
  expect_equal(grab( x3.t[all_dims(), 1]    ), as.array( x3.r[,,1]  ))
  expect_equal(grab( x3.t[all_dims(), 1, 1] ), as.array( x3.r[,1,1] ))

  expect_equal(grab( x4.t[all_dims()]       ), as.array( x4.r[,,,]   ))
  expect_equal(grab( x4.t[1, all_dims()]    ), as.array( x4.r[1,,,]  ))
  expect_equal(grab( x4.t[1, 1, all_dims()] ), as.array( x4.r[1,1,,] ))
  expect_equal(grab( x4.t[1, all_dims(), 1] ), as.array( x4.r[1,,,1] ))
  expect_equal(grab( x4.t[all_dims(), 1]    ), as.array( x4.r[,,,1]  ))
  expect_equal(grab( x4.t[all_dims(), 1, 1] ), as.array( x4.r[,,1,1] ))

})


test_that("negative-integers work python style", {

  skip_if_no_tensorflow()
  options(tensorflow.extract.warn_negatives_pythonic = FALSE)
  # options(tensorflow.warn_negative_extract_is_python_style = FALSE)

  x1.r <- arr(4)
  x2.r <- arr(4, 4)

  x1.t <- tf$constant(x1.r)
  x2.t <- tf$constant(x2.r)

  options(tensorflow.extract.one_based = TRUE)
  expect_equal(grab( x1.t[-1] ),     x1.r[4]    )
  expect_equal(grab( x1.t[-2] ),     x1.r[3]    )
  expect_equal(grab( x2.t[-2, -2] ), x2.r[3, 3] )
  expect_equal(grab( x2.t[-1, ] ), as.array( x2.r[4,] ))

  options(tensorflow.extract.one_based = FALSE)
  # same as above
  expect_equal(grab( x1.t[-1] ),     x1.r[4]    )
  expect_equal(grab( x1.t[-2] ),     x1.r[3]    )
  expect_equal(grab( x2.t[-2, -2] ), x2.r[3, 3] )
  expect_equal(grab( x2.t[-1, ] ), as.array( x2.r[4,] ))

  null_out_all_extract_opts()
})


test_that("python-style strided slice", {

  skip_if_no_tensorflow()
  oopts <- options()
  options(tensorflow.extract.warn_negatives_pythonic = FALSE)

  x.r <- arr(20, 2) # 2nd dim to keep R from dropping (since tf always returns 1d array)
  x.t <- tf$constant(x.r)

  options(tensorflow.extract.style = "R")

  expect_equal(grab( x.t[ `5:`          ,] ), x.r[ 5:20,])
  expect_equal(grab( x.t[ `5:NULL`      ,] ), x.r[ 5:20,])
  expect_equal(grab( x.t[  5:NULL       ,] ), x.r[ 5:20,])
  expect_equal(grab( x.t[ `5:NULL:`     ,] ), x.r[ 5:20,])
  expect_equal(grab( x.t[  5:NULL:NULL  ,] ), x.r[ 5:20,])
  expect_equal(grab( x.t[ `5:NULL:NULL` ,] ), x.r[ 5:20,])

  expect_equal(grab( x.t[ `5::` ,] ), x.r[ 5:20,])
  expect_equal(grab( x.t[ `:5:` ,] ), x.r[ 1:5,])
  expect_equal(grab( x.t[ `:5`  ,] ), x.r[ 1:5,])
  expect_equal(grab( x.t[ `2:5` ,] ), x.r[ 2:5,])
  expect_equal(grab( x.t[ 2:5   ,] ), x.r[ 2:5,])

  expect_equal(grab( x.t[ `::2` ,]       ), x.r[ seq.int(1, 20, by = 2) ,])
  expect_equal(grab( x.t[ NULL:NULL:2 ,] ), x.r[ seq.int(1, 20, by = 2) ,])

  # non syntantic names or function calls can work too
  `_idx` <- 1
  expect_equal(grab( x.t[ `_idx`:(identity(5)+1L),]), x.r[ 1:6, ] )


  expect_equal(grab( x.t[ `2:6:2`,]), x.r[ seq.int(2, 6, 2) ,])
  expect_equal(grab( x.t[  2:6:2 ,]), x.r[ seq.int(2, 6, 2) ,])

  # decreasing indexes work
  expect_equal(grab( x.t[ `6:2:-2`,]), x.r[ seq.int(6, 2, -2) ,])
  expect_equal(grab( x.t[  6:2:-2 ,]), x.r[ seq.int(6, 2, -2) ,])

  # sign of step gets automatically inverted on decreasing indexes
  expect_equal(grab( x.t[ `6:2:2` ,]), x.r[ seq.int(6, 2, -2) ,])
  expect_equal(grab( x.t[  6:2:2  ,]), x.r[ seq.int(6, 2, -2) ,])
  expect_equal(grab( x.t[  6:2    ,]),   x.r[ 6:2 ,])
  expect_equal(grab( x.t[  6:2:1 ,]),   x.r[ 6:2  ,])
  expect_equal(grab( x.t[  6:2:-1 ,]),   x.r[ 6:2 ,])


  options(tensorflow.extract.style = "python")
  # options set to match python
  # helper to actually test in python
  test_in_python <- (function() {
    # main <- reticulate::import_main()
    reticulate::py_run_string(paste(
      "import numpy as np",
      "x = np.array(range(1, 41))",
      "x.shape = (2, 20)",
      "x = x.transpose()", sep = "\n"))
    function(chr) {
      reticulate::py_eval(chr)
    }
  })()


  expect_equal(grab( x.t[ 2:5,] ), test_in_python("x[2:5,]"))
  expect_equal(grab( x.t[ 2:-5 ,] ), test_in_python("x[ 2:-5 ,]"))
  expect_equal(grab( x.t[ 2:5:2 ,] ), test_in_python("x[ 2:5:2 ,]"))
  expect_equal(grab( x.t[ -2:-5:-1 ,] ), test_in_python("x[ -2:-5:-1 ,]"))
  expect_equal(grab( x.t[ 5:2:-1 ,] ), test_in_python("x[ 5:2:-1 ,]"))
  expect_equal(grab( x.t[ 5:2:-2 ,] ), test_in_python("x[ 5:2:-2 ,]"))


  # indexing with tensors
  expect_equal(grab( x.t[tf$constant(2L),] ), as.array(x.r[3,]))
  expect_equal(grab( x.t[tf$constant(2L):tf$constant(5L),] ), x.r[3:5,])

  # expect warning that no translation on tensors performed
  null_out_all_extract_opts()
  expect_warning(grab( x.t[tf$constant(2L),] ), "ignored")

  # warn only once
  expect_silent(grab( x.t[tf$constant(2L),] ))

  # warn in slice syntax too
  reset_warnings()
  null_out_all_extract_opts()
  expect_warning(grab( x.t[tf$constant(2L):tf$constant(5L),] ), "ignored")

  reset_warnings()
  options(tensorflow.extract.warn_tensors_passed_asis = FALSE)
  expect_silent(grab( x.t[tf$constant(2L):tf$constant(5L),] ))


  null_out_all_extract_opts()
})



# test warnings for extraction that looks like it might be 0-based

test_that('extract warns when indices look 0-based', {

  skip_if_no_tensorflow()
  oopts <- options()

  x <- tf$constant(matrix(0, 2, 2))
  i0 <- 0:1
  i1 <- 1:2

  # explicit 0-indexing shouldn't warn
  options(tensorflow.extract.one_based = FALSE)
  expect_silent(x[i0, i0])

  # explicit 1-indexing shouldn't warn
  options(tensorflow.extract.one_based = TRUE)
  # expect_silent(x[i0, i0]) # expect error

  # default 1-indexing should warn only if there's a zero in there
  options(tensorflow.extract.one_based = NULL)
  expect_silent(x[i1, i1])
  # expect_warning(x[i0, i0], # expect error
  #                "It looks like you might be using 0-based indexing")

  options(oopts)
})

test_that('extract errors when indices have missing elements at variable steps', {

  skip_if_no_tensorflow()

  x <- tf$constant(array(0, dim = c(2, 4, 2)))

  # indexing with sequential values shouldn't error
  expect_silent(x[1, c(1, 2, 3), ])
  expect_error( x[1, c(1, 3, 4),])

})


# reset user's extract method
# options(tensorflow.extract.one_based = old_extract_method)
