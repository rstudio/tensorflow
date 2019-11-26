
#' Tensor shape
#'
#' @param ... Tensor dimensions
#'
#' @export
shape <- function(...) {
  values <- list(...)
  lapply(values, function(value) {
    if (!is.null(value))
      as.integer(value)
    else
      NULL
  })
}

#' @export
py_to_r.tensorflow.python.framework.tensor_shape.TensorShape <- function(x) {
  x$as_list()
}
