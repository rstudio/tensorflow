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
}


binary_logic_generics <- c("&", "|")

x <- lapply(expand.grid(e1 = c(TRUE, FALSE), e2 = c(TRUE, FALSE)),
            as.array)

for (fn in binary_logic_generics)
  test_generic(fn, x$e1, x$e2)


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
