
py_list <- function(x) {
  (.globals$py_list %||%
     (.globals$py_list <- reticulate::import_builtins()$list))(x)
}

#' @importFrom reticulate py_to_r
#' @export
py_to_r.tensorflow.python.training.tracking.data_structures.List <- py_list

#' @export
py_to_r.tensorflow.python.trackable.data_structures.ListWrapper <- py_list

#' @export
py_to_r.tensorflow.python.trackable.data_structures.List <- py_list
