

#' @export
as.array.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    return(NULL)
  if(as_r_value(x$dtype$name) == "string")
    array(as.character(x, ...),
          dim = if(length(dx <- dim(x))) dx else 1L)
  else
    as_r_value(x$numpy())
}

#' @export
as.array.tensorflow.python.framework.ops.EagerTensor <- as.array.python.builtin.EagerTensor

#' @export
as.array.tensorflow.python.ops.variables.Variable <- as.array.python.builtin.EagerTensor



#' @export
as.vector.python.builtin.EagerTensor <- function(x, ...) {
  as.vector(as.array(x, ...))
}

#' @export
as.vector.tensorflow.python.framework.ops.EagerTensor <- as.vector.python.builtin.EagerTensor

#' @export
as.vector.tensorflow.python.ops.variables.Variable <- as.vector.python.builtin.EagerTensor


#' @export
as.matrix.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    return(NULL)

  as.matrix(as.array(x, ...))
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

#' @export
as.character.python.builtin.EagerTensor <- function(x, ...) {
  out <- as_r_value(x$numpy())
  # as.character() on python bytes dispatches to
  # reticulate:::as.character.python.builtin.bytes, which calls
  # x$decode(encoding = "utf-8", errors = "strict")
  if(is.list(out))
    vapply(out, as.character, "", ..., USE.NAMES = FALSE)
  else
    as.character(out, ...)
}

#' @export
as.character.tensorflow.python.framework.ops.EagerTensor <-
  as.character.python.builtin.EagerTensor

#' @export
as.character.tensorflow.python.ops.variables.Variable <-
  as.character.python.builtin.EagerTensor

## @exportS3Method grDevices::as.raster A delayed registration like this requires R>=3.6

#' @importFrom grDevices as.raster
#' @export
as.raster.python.builtin.EagerTensor <-
function(x, max = if(as_r_value(x$dtype$is_integer)) as_r_value(x$dtype$max) else 1, ...)
  as.raster(as.array(x), max = max, ...)

#' @export
as.raster.tensorflow.python.framework.ops.EagerTensor <-
  as.raster.python.builtin.EagerTensor

#' @export
as.raster.tensorflow.python.ops.variables.Variable <-
  as.raster.python.builtin.EagerTensor



#' Creates a callable TensorFlow graph from an R function.
#'
#' `tf_function` constructs a callable that executes a TensorFlow graph created
#' by tracing the TensorFlow operations in `f`. This allows the TensorFlow
#' runtime to apply optimizations and exploit parallelism in the computation
#' defined by `f`.
#'
#' A guide to getting started with
#' [`tf.function`](https://www.tensorflow.org/api_docs/python/tf/function) can
#' be found [here](https://www.tensorflow.org/guide/function).
#'
#' @param f the function to be compiled
#' @param input_signature A possibly nested sequence of `tf$TensorSpec` objects
#'   specifying the shapes and dtypes of the tensors that will be supplied to
#'   this function. If `NULL`, a separate function is instantiated for each
#'   inferred input signature. If `input_signature` is specified, every input to
#'   `f` must be a tensor.
#' @param autograph TRUE or FALSE. If TRUE (the default), you can use tensors in
#'   R control flow expressions `if`, `while`, `for` and `break` and they will
#'   be traced into the tensorflow graph. A guide to getting started and
#'   additional details can be found:
#'   [here](https://t-kalinowski.github.io/tfautograph/)
#' @param ... additional arguments passed on to `tf.function` (vary based on
#'   Tensorflow version). See
#'   [here](https://www.tensorflow.org/api_docs/python/tf/function#args_1) for
#'   details.
#'
#' @export
tf_function <- function(f,
                        input_signature = NULL,
                        autograph = TRUE,
                        ...) {
  if (!is.function(f))
    stop("`f` must be an R function")

  if (!(isTRUE(autograph) || isFALSE(autograph)))
    stop("`autograph` must be TRUE or FALSE")

  if (autograph) {
    # Can't register tfautograph in Imports yet due to circular dependency
    if(!requireNamespace("tfautograph", quietly=TRUE))
      stop('"tfautograph" package required if autograph=TRUE. Please run install.packages("tfautograph")')
    f <- tfautograph::autograph(f)
  }

  args <- list(py_func(f), input_signature, FALSE, ...)
  do.call(tf$`function`, args)
}

# TODO: calling tf_function() with `f` missing should return
# a decorator with args partially pre-specified
