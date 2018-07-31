

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




