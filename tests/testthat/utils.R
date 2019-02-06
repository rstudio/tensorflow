.SESS <- NULL
grab <- function(x) {
  if(tf$executing_eagerly()) {
    as.array(x)
  } else {
    if (is.null(.SESS))
      .SESS <<- tf$Session()
    .SESS$run(x)
  }
}

skip_if_no_tensorflow <- function() {
  if (!reticulate::py_module_available("tensorflow"))
    skip("TensorFlow not available for testing")
}
