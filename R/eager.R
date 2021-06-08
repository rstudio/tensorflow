

#' @export
as.array.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else
    x$numpy()
}

#' @export
as.array.tensorflow.python.framework.ops.EagerTensor <- as.array.python.builtin.EagerTensor

#' @export
as.array.tensorflow.python.ops.variables.Variable <- as.array.python.builtin.EagerTensor


#' @export
as.matrix.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else {
    a <- x$numpy()
    if (length(dim(a)) == 2)
      a
    else
      as.matrix(a)
  }
}

#' @export
as.matrix.tensorflow.python.framework.ops.EagerTensor <- as.matrix.python.builtin.EagerTensor

#' @export
as.matrix.tensorflow.python.ops.variables.Variable <- as.matrix.python.builtin.EagerTensor


#' @export
as.integer.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else
    as.integer(as.array(x))
}

#' @export
as.integer.tensorflow.python.framework.ops.EagerTensor <- as.integer.python.builtin.EagerTensor

#' @export
as.integer.tensorflow.python.ops.variables.Variable <- as.integer.python.builtin.EagerTensor


#' @export
as.numeric.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else
    as.numeric(as.array(x))
}

#' @export
as.numeric.tensorflow.python.framework.ops.EagerTensor <- as.numeric.python.builtin.EagerTensor

#' @export
as.numeric.tensorflow.python.ops.variables.Variable <- as.numeric.python.builtin.EagerTensor


#' @export
as.double.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else
    as.double(as.array(x))
}

#' @export
as.double.tensorflow.python.framework.ops.EagerTensor <- as.double.python.builtin.EagerTensor

#' @export
as.double.tensorflow.python.ops.variables.Variable <- as.double.python.builtin.EagerTensor


#' @export
as.logical.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else
    as.logical(as.array(x))
}

#' @export
as.logical.tensorflow.python.framework.ops.EagerTensor <- as.logical.python.builtin.EagerTensor

#' @export
as.logical.tensorflow.python.ops.variables.Variable <- as.logical.python.builtin.EagerTensor


#' Creates a callable TensorFlow graph from an R function.
#'
#' `tf_function` constructs a callable that executes a TensorFlow graph created
#' by tracing the TensorFlow operations in `f`. This allows the TensorFlow
#' runtime to apply optimizations and exploit parallelism in the computation
#' defined by `f`.
#'
#' @param f the function to be compiled
#' @param input_signature A possibly nested sequence of `tf$TensorSpec` objects
#'   specifying the shapes and dtypes of the tensors that will be supplied to
#'   this function. If `NULL`, a separate function is instantiated for each
#'   inferred input signature. If `input_signature` is specified, every input to
#'   `f` must be a tensor.
#' @param autograph Whether autograph should be applied on `f` before tracing a
#'   graph. This allows for dynamic control flow (if's, loops etc.) in the
#'   traced graph. See https://www.tensorflow.org/guide/autograph for more
#'   information. Note: We set the default to `FALSE` until this functionality
#'   is available from R.
#' @param ... additional arguments passed on to `tf.function` (vary based on
#'   Tensorflow version). See
#'   https://www.tensorflow.org/api_docs/python/tf/function for details.
#'
#' @export
tf_function <- function(f,
                        input_signature = NULL,
                        autograph = FALSE, # default will change to TRUE in TF 2.6
                        ...) {

  if(!isFALSE(autograph)) stop("Autograph functionality is not (yet) supported from R.")

  args <- list(py_func(f), input_signature, autograph, ...)

  do.call(tf$`function`, args)
}

