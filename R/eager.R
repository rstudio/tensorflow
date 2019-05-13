

#' Enables, for the rest of the lifetime of this program, eager execution.
#'
#' If not called immediately on startup risks creating breakage and bugs.
#'
#' After eager execution is enabled, operations are executed as they are
#' defined and tensors hold concrete values, and can be accessed as R matrices
#' or arrays with [as.matrix()], [as.array()], [as.double()], etc.
#'
#' @examples \dontrun{
#'
#' # load tensorflow and enable eager execution
#' library(tensorflow)
#' tfe_enable_eager_execution()
#'
#' # create a random 10x10 matrix
#' x <- tf$random_normal(shape(10, 10))
#'
#' # use it in R via as.matrix()
#' heatmap(as.matrix(x))
#' }
#'
#' @param config (Optional) A `tf$ConfigProto()` protocol buffer with
#'   configuration options for the Context. Note that a lot of these options
#'   may be currently unimplemented or irrelevant when eager execution is
#'   enabled.
#' @param device_policy (Optional) What policy to use when trying to run an
#'   operation on a device with inputs which are not on that device. Valid
#'   values: "explicit": raises an error if the placement is not correct.
#'   "warn": copies the tensors which are not on the right device but raises a
#'   warning. "silent": silently copies the tensors. This might hide
#'   performance problems.
#'
#' @export
tfe_enable_eager_execution <- function(
  config = NULL, device_policy = c("explicit", "warn", "silent")) {

  # alias eager mode (error if not available in this version of tf)
  contrib <- tf$contrib
  if (py_has_attr(contrib, "eager")) {
    tfe <- tf$contrib$eager
  } else {
    stop('Eager execution not available in this version of tensorflow.\n',
         'Try: install_tensorflow(version = "nightly")')
  }

  # resovle device policy
  device_policy <- switch(match.arg(device_policy),
    explicit = tfe$DEVICE_PLACEMENT_EXPLICIT,
    warn = tfe$DEVICE_PLACEMENT_WARN,
    silent = tfe$DEVICE_PLACEMENT_SILENT
  )

  # enter eager mode
  if (tf_version() >= '1.7') {
    enable_eager_execution_fn <- tf$python$framework$ops$enable_eager_execution
  } else {
    enable_eager_execution_fn <- tf$contrib$eager$enable_eager_execution
  }

  enable_eager_execution_fn(
    config = config,
    device_policy = device_policy
  )
}


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
as.integer.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else
    as.integer(as.array(x))
}

#' @export
as.integer.tensorflow.python.framework.ops.EagerTensor <- as.integer.python.builtin.EagerTensor



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
as.double.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else
    as.double(as.array(x))
}

#' @export
as.double.tensorflow.python.framework.ops.EagerTensor <- as.double.python.builtin.EagerTensor


#' @export
as.logical.python.builtin.EagerTensor <- function(x, ...) {
  if (py_is_null_xptr(x))
    NULL
  else
    as.logical(as.array(x))
}

#' @export
as.logical.tensorflow.python.framework.ops.EagerTensor <- as.logical.python.builtin.EagerTensor


#' Creates a callable TensorFlow graph from an R function.
#'
#' `tf_function` constructs a callable that executes a TensorFlow graph created by tracing the
#' TensorFlow operations in `f`.
#' This allows the TensorFlow runtime to apply optimizations and exploit parallelism in the
#' computation defined by `f`.
#'
#' @param f the function to be compiled
#' @param input_signature A possibly nested sequence of `tf$TensorSpec` objects specifying the shapes
#' and dtypes of the tensors that will be supplied to this function.
#' If `NULL`, a separate function is instantiated for each inferred input signature.
#' If `input_signature` is specified, every input to `f` must be a tensor.
#' @param autograph Whether autograph should be applied on `f` before tracing a graph.
#' This allows for dynamic control flow (if's, loops etc.) in the traced graph.
#' See https://www.tensorflow.org/guide/autograph for more information.
#' Note: We set the default to `FALSE` until this functionality is available from R.
#' @param experimental_autograph_options Experimental knobs (in the form of a tuple of
#' `tf$autograph$Feature` values) to control behavior when autograph = TRUE.
#'
#' @export
tf_function <- function(f,
                        input_signature = NULL,
                        autograph = FALSE, # default is FALSE until we have an implementation
                        experimental_autograph_options = NULL) {

  if(!isFALSE(autograph)) stop("Autograph functionality is not (yet) supported from R.")

  tf$`function`(
    reticulate::py_func(f),
    input_signature,
    autograph,
    experimental_autograph_options
  )
}

