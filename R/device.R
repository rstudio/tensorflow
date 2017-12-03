

#' Execute code with a default TensorFlow device
#'
#' @param device_name_or_function Device name or function to map operations
#'   to device names.
#' @param expr Expression to evaluate
#'
#' @details
#' The `device_name_or_function` argument may either be a device name
#' string, a device function, or NULL:
#'
#'  - If it is a device name string, all operations constructed in this
#'    context will be assigned to the device with that name, unless overridden by a nested `device()` context.
#'  - If it is a function, it will be treated as a function from Operation
#'    objects to device name strings, and invoked each time a new Operation
#'    is created. The Operation will be assigned to the device with the
#'    returned name.
#'  - If it is NULL, all `device()` invocations from the enclosing context
#'    will be ignored. For information about the valid syntax of device
#'    name strings, see the documentation in [`DeviceNameUtils`](https://www.tensorflow.org/code/tensorflow/core/util/device_name_utils.h).
#'
#' @examples \dontrun{
#'
#' with_tf_device("/gpu:0", {
#'   # Operations constructed in this context will be placed on GPU 0.
#'   with_tf_device(NULL, {
#'      # Operations constructed in this context will have no assigned device.
#'   })
#' })
#'
#' # Defines a function from Operation to device string.
#' matmul_on_gpu <- function(n) {
#'   if (n$type == "MatMul")
#'     "/gpu:0"
#'   else
#'     "/cpu:0"
#' }
#'
#' with_tf_device(matmul_on_gpu, {
#'   # All operations of type "MatMul" constructed in this context
#'   # will be placed on GPU 0; all other operations will be placed
#'   # on CPU 0.
#' })
#'
#' }
#'
#' @export
with_tf_device <- function(device_name_or_function, expr) {
  with(tf$device(device_name_or_function), expr)
}


