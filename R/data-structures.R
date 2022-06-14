#' @importFrom reticulate py_to_r
#' @export
py_to_r.tensorflow.python.training.tracking.data_structures.List <- function(x) {
  py_builtin()$list(x)
}


py_builtin <- function() {
  builtin <- .globals$py_builtin
  if (is.null(builtin)) {
    builtin <- .globals$py_builtin <- reticulate::import_builtins()
  }
  builtin
}
