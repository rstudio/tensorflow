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



test_generic <- function(name, ..., namespace = "base") {
  test_that(paste("Generic", name, "works"), {
    skip_if_no_tensorflow()
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


