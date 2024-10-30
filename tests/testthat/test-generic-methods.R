context("generic methods")


test_that("log with supplied base works", {

  skip_if_no_tensorflow()

  r <- array(as.double(1:20))
  t <- as_tensor(r, dtype = tf$float32)

  expect_near(r, grab(log(as_tensor(exp(r)))))
  expect_near(r, grab(log2(as_tensor(2 ^ r))))
  expect_near(r, grab(log10(as_tensor(10 ^ r))))

  expect_near(r, grab(log(exp(t))))
  expect_near(r, grab(log2(2 ^ t)))
  expect_near(r, grab(log10(10 ^ t)))

  # log() dispatches correctly without trying to change base
  expect_identical(grab(tf$math$log(t)), grab(log(t)))

  expect_near(log(r), grab(log(t)))
  expect_near(log(r, base = 3), grab(log(t, base = 3)))

})



test_generic <- function(fn, ..., namespace = "base") {
  name <- gsub("[\"']", "", deparse(substitute(fn)))
  if(!is.function(fn))
    name <- fn
  test_that(paste("Generic", name, "works"), {
    skip_if_no_tensorflow()
    if(!is.function(fn))
    fn <- get(name, envir = asNamespace(namespace),
              mode = 'function')

    suppress_warning_NaNs_produced({
      out_r <- do.call(fn, list(...))
    })

    if(length(list(...)) == 1) {
      out_tf <- grab(fn(tf$constant(..1)))
      expect_equal(out_tf, out_r)
      return()
    }

    if(length(list(...)) == 2) {
      expect_equal(out_r, grab(fn(tf$constant(..1), ..2)))
      expect_equal(out_r, grab(fn(..1, tf$constant(..2))))
      expect_equal(out_r, grab(fn(tf$constant(..1), tf$constant(..2))))
      return()
    }

    stop("bad test call, only unary and binary S3 generics supported")

  })
}

# --------- binary operators ----------------

binary_arith_generics <- c("+", "-", "*", "/", "^", "%%", "%/%")
binary_compr_generics <- c("==", "!=", "<", "<=", ">", ">=")


for (fn in c(binary_arith_generics, binary_compr_generics)) {
  test_generic(fn, rarr(1,2,3), rarr(1,2,3))

  # test automatic type casting
  fn <- get(fn, envir = asNamespace("base"))
  expect_equal(fn(5L, 3), grab(fn(as_tensor(5L), 3)))
  expect_equal(fn(5L, 3), grab(fn(5L, as_tensor(3, "float64"))))

  expect_equal(fn(5, 3L), grab(fn(5, as_tensor(3L))))
  expect_equal(fn(5, 3L), grab(fn(as_tensor(5, "float64"), 3L)))
}


if(getRversion() >= "4.3.0") {
  test_generic("%*%", rarr(3, 3), rarr(3, 3))
}

expect_equal(as.numeric(as_tensor(3) ^ 2), 3^2)
expect_equal(as.numeric(as_tensor(3, "float64") ^ .5), 3^.5)

binary_logic_generics <- c("&", "|")

x <- lapply(expand.grid(e1 = c(TRUE, FALSE), e2 = c(TRUE, FALSE)),
            as.array)
x$e1.num <- x$e2.num <- 1:4
x$e1.num[!x$e1] <- 0L
x$e2.num[!x$e2] <- 0

for (fn in binary_logic_generics) {
  test_generic(fn, x$e1, x$e2)

  # test automatic type casting
  fn <- get(fn, envir = asNamespace("base"))
  expect_equal(fn(x$e1.num, x$e2), grab(fn(x$e1.num, as_tensor(x$e2))))
  expect_equal(fn(x$e1, x$e2.num), grab(fn(as_tensor(x$e1), x$e2.num)))
}


# ---------- unary operators ---------------
unary_logic_generics <- c("!")

for (fn in unary_logic_generics)
  test_generic(fn, x$e1)


unary_shape_generics <- c("dim", "length")

for (fn in unary_shape_generics) {
  test_generic(fn, arr(1))
  test_generic(fn, arr(1, 2))
  test_generic(fn, arr(1, 2, 3))
  test_generic(fn, arr(3))
}

expect_identical(dim(as_tensor(arr(3, 3))), c(3L, 3L))

f <- tf_function(function(x) {
  expect_identical(dim(x), NA_integer_)
  expect_identical(length(x), NA_integer_)
  x+1
}, input_signature = list(tf$TensorSpec(shape(NA))))
f(as_tensor(array(3), "float32"))

f <- tf_function(function(x) {
  expect_identical(dim(x), c(NA_integer_, 1L, NA_integer_))
  expect_identical(length(x), NA_integer_)
  x+1
}, input_signature = list(tf$TensorSpec(shape(NA, 1, NA))))
f(as_tensor(array(3, dim = c(1,1,1)), "float32"))


f <- tf_function(function(x) {
  expect_identical(dim(x), NULL)
  expect_identical(length(x), NA_integer_)
  x+1
}, input_signature = list(tf$TensorSpec(shape(dims = NULL))))
f(as_tensor(array(3, dim = c(1,1,1)), "float32"))


unary_math_generics <- c(

  "-",
  "+",

  "abs",
  "sign",
  "sqrt",
  "floor",
  "ceiling",
  "round",

  "log",
  "log1p",
  "log2",
  "log10",

  "exp",
  "expm1",

  "cos",
  "sin",
  "tan",

  "sinpi",
  "cospi",
  "tanpi",

  "acos",
  "asin",
  "atan",

  "lgamma",
  "digamma"
)

for (fn in c(unary_math_generics)) {
  test_generic(fn, arr(20))
  test_generic(fn, rarr(20))
}


unary_complex_generics <- c("Re", "Im", "Conj", "Arg", "Mod")

for (fn in unary_complex_generics)
  test_generic(fn, 1 + 2i)



numeric_reduce_generics <-
  list(sum, prod, min, max, mean, range)


x <- arr(3, 4)
xt <- as_tensor(x)

for(fn in numeric_reduce_generics)
  expect_equal(fn(x), as.numeric(fn(as_tensor(x))))

for(fn in list(sum, prod, min, max, range)) # not  mean
  expect_equal(fn(x, x), as.numeric(fn(as_tensor(x), as_tensor(x))))

for(fn in list(sum, prod, min, max, mean)) { # not range
  expect_equal(dim(fn(xt, axis = 1)), 4L)
  expect_equal(dim(fn(xt, axis = 2)), 3L)
  expect_equal(dim(fn(xt, axis = 1, keepdims = TRUE)), c(1L, 4L))
  expect_equal(dim(fn(xt, axis = 2, keepdims = TRUE)), c(3L, 1L))
}


bool_reduce_generics <- list(all, any)
for (fn in bool_reduce_generics) {
  tt <- rep(TRUE, 5)
  ff <- rep(FALSE, 5)
  mx <- rep(c(TRUE, FALSE), 4)
  for (x in list(tt, ff, mx)) {
    expect_equal(fn(x), as.logical(fn(as_tensor(x))))
    expect_equal(fn(x, x), as.logical(fn(as_tensor(x), as_tensor(x))))
    expect_equal(fn(x, x), as.logical(fn(as_tensor(x), x)))
  }
}

expect_equivalent_bind_generic <- function(fn, ...) {
  res1 <- fn(...)
  dimnames(res1) <- NULL
  res2 <- as.array(do.call(fn, lapply(list(...), as_tensor)))
  if(is_windows()) # https://github.com/rstudio/reticulate/issues/1071
    storage.mode(res2) <- "integer"
  expect_identical(res1, res2)

  dots <- list(...)
  dots[[1L]] <- as_tensor(..1)
  res3 <- as.array(do.call(fn, dots))
  if(is_windows()) # https://github.com/rstudio/reticulate/issues/1071
    storage.mode(res3) <- "integer"
  expect_identical(res1, res3)

  dots <- list(...)
  dots[[2L]] <- as_tensor(..2)
  res4 <- as.array(do.call(fn, dots))
  if(is_windows()) # https://github.com/rstudio/reticulate/issues/1071
    storage.mode(res4) <- "integer"
  expect_identical(res1, res4)
}

m <- matrix(1:9, nrow = 3)
v <- 1:3
v1 <- as.array(1:3)
for (fn in list(cbind, rbind)) {
  expect_equivalent_bind_generic(fn, v,v,v)
  expect_equivalent_bind_generic(fn, v1,v,m)
  expect_equivalent_bind_generic(fn, m,v,v)
  expect_equivalent_bind_generic(fn, m, m)
  expect_equivalent_bind_generic(fn, m, v)
  expect_equivalent_bind_generic(fn, 1L, 1L)
  expect_equivalent_bind_generic(fn, 1L, as.matrix(1L))
  expect_equivalent_bind_generic(fn, as.array(1L), 1L)
  expect_equal(fn(as_tensor(1:3), 1:3, dtype = "int64")$dtype$name, "int64")
  expect_equal(fn(as_tensor(1:3), 1:3, dtype = "int16")$dtype$name, "int16")
  expect_equal(fn(as_tensor(1:3), 1:3, dtype = "float32")$dtype$name, "float32")
}


test_generic("t", 1)
test_generic("t", array(1))
test_generic("t", matrix(1))
test_generic("t", 1:3)
test_generic("t", array(1:3))
test_generic("t", matrix(1:3))
test_generic("t", m)

test_generic("aperm", array(1))
test_generic("aperm", matrix(1))
test_generic("aperm", array(1:3))
test_generic("aperm", matrix(1:3))
test_generic("aperm", m)

a <- arr(3, 4, 5)
r1 <- aperm(a, c(2, 1, 3))
r2 <- as.array(aperm(as_tensor(a), c(2, 1, 3)))
expect_identical(r1, r2)


x <- array(c(0, 1, Inf, NaN))
test_generic("is.finite", x)
test_generic("is.infinite", x)
test_generic("is.nan", x)

x <- array(c(2L, 10L, 3L, 1L, 7L, 4L, 6L, 8L, 9L, 5L))
test_generic("sort", x)
decreasing_sort <- function(x) sort(x, decreasing = TRUE)
test_generic(decreasing_sort, x)


xx <- list(array(1:3),
           1)

for (x in xx) {
  test_generic(function(a) as.array(rep(a, 3)), x)
  test_generic(function(a) as.array(rep(as_tensor(a), as_tensor(3L))), x)

  test_generic("as.vector", x)
}


test_that("generics can handle tensors w/ convert=FALSE", {

  skip_if_no_tensorflow()

  # this tests that `*` dispatches correctly even of both x and y provide Ops methods
  if(getRversion() >= "4.3.0") {
    x <- tf$ones(shape(5, 5)) * r_to_py(array(1, dim = c(5, 5)))
    expect_true(as.logical(all(x == 1)))
  }

  # test that as.array / as.raster can work even if convert=FALSE
  img <- tf$cast(tf$random$uniform(shape(256, 256, 4), maxval = 256),
                 "uint8")
  x <- np_array(array(c(2, 1, 1, 1), dim = c(1, 1, 4)), dtype = "uint8") # convert=FALSE

  expect_no_error(as.raster(img))
  expect_no_error(as.raster(r_to_py(img)))

  if (getRversion() >= "4.3.0") {
    expect_no_error(as.raster(img %/% x))
    expect_no_error(as.raster(r_to_py(img %/% x)))
    expect_no_error(as.raster(r_to_py(r_to_py(img) %/% x)))
    expect_no_error(as.raster(r_to_py(img %/% r_to_py(x))))
  }

})


