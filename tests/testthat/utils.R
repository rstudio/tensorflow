

skip_if_no_tensorflow <- function() {
  have_tensorflow <- tryCatch({ reticulate::import("tensorflow"); TRUE }, error = function(e) FALSE)
  if (!have_tensorflow)
    skip("TensorFlow not available for testing")
}
