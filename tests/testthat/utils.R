Sys.setenv(TF_CPP_MIN_LOG_LEVEL = 1)

.SESS <- NULL
grab <- function(x) {
  if(tf$executing_eagerly()) {
    as.array(x)
  } else {
    if (is.null(.SESS))

      if (tf_version() >= "1.14")
        .SESS <<- tf$compat$v1$Session()
      else
        .SESS <<- tf$Session()

    .SESS$run(x)
  }
}

skip_if_no_tensorflow <- function() {
  if (!reticulate::py_module_available("tensorflow"))
    skip("TensorFlow not available for testing")
}

