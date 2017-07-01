

is_travis <- function() {
  identical(Sys.getenv("TRAVIS"), "true")
}

skip_if_no_tensorflow <- function() {
  if (!is_travis() && !reticulate::py_module_available("tensorflow"))
    skip("TensorFlow not available for testing")
}
