
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

tensor_shape_to_list <- function(x) {
  l <- x$as_list()
  if (inherits(l, "python.builtin.object")) {
    l <- reticulate::py_to_r(l)
  }
  l <- as.list(l)
}

#' @export
`[.tensorflow.python.framework.tensor_shape.TensorShape` <- function(x, i) {
  stopifnot(i>0)
  l <- tensor_shape_to_list(x)
  l[i]
}

#' @export
`[[.tensorflow.python.framework.tensor_shape.TensorShape` <- function(x, i) {
  stopifnot(i>0)
  l <- tensor_shape_to_list(x)
  l[[i]]
}
