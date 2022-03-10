Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 1)
options(warn = 1L)

if(py_module_available("tensorflow"))
  tf$abs(1) # initialize on load_all()

.SESS <- NULL
grab <- function(x) {
  if(!inherits(x, "tensorflow.tensor"))
    return(x)

  if(tf$executing_eagerly())
    return(as.array(x))

  if (is.null(.SESS)) {
    if (tf_version() >= "1.14")
      .SESS <<- tf$compat$v1$Session()
    else
      .SESS <<- tf$Session()
  }

  .SESS$run(x)
}

skip_if_no_tensorflow <- function() {
  if (!reticulate::py_module_available("tensorflow"))
    skip("TensorFlow not available for testing")
}


arr <- function(..., mode = "double", gen = seq_len)
  array(as.vector(gen(prod(unlist(c(...)))), mode = mode), unlist(c(...)))

set.seed(42)
rarr <- function(...) arr(..., gen=runif)

expect_near <- function(..., tol = 1e-5) expect_equal(..., tolerance = tol)


suppress_warning_NaNs_produced <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if(inherits(w, "warning") && grepl("NaNs produced", w$message))
        invokeRestart("muffleWarning")
    })
}
