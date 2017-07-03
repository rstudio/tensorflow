#' Context manager for imperative style TensorFlow
#' 
#' The results of the computation are available right after the execution of a line of code and
#' can be converted to base R objects automatically.
#' 
#' Note that this function is currently only experimental, meaning that the interface is subject
#' to change or remove at later releases.
#' 
#' @param expr A block of code expession
#' @param new_step A boolean indicating whether the expression is evaluated as a new step.
#' A graph is constructed and kept around in the background, both for just executing using
#' the standard TensorFlow runtime, and also for allowing automatic differentiation via `tf$gradients`.
#' If this is set to `TRUE`, the graph as well as the cached tensors that have been kept around for
#' gradient computation will be cleared after the expression is evaluated.
#' @param convert A boolean indicating whether to convert the returned Tensor object to base R object.
#' 
#' @examples 
#' \dontrun{
#' 
#' with_imperative({
#'   a <- tf$constant(list(list(7), list(6)))
#'   b <- tf$constant(list(list(6, 7)))
#'   tf$matmul(a, b)
#' })
#' 
#' }
#' @export
tf_imperative <- function(
  expr,
  new_step = FALSE,
  convert = TRUE
) {
  server <- tf$python$training$training$Server$create_local_server()
  target <- server$target
  with(tf$contrib$imperative$imperative_mode$ImperativeMode(target), as = imperative_mode, {
    if (new_step) {
      with(imperative_mode$new_step(), as = new_step, {
        res <- force(expr)
        convert_tensors(res, convert)
      })
    } else {
      res <- force(expr)
      convert_tensors(res, convert)
    }
  })
}

convert_tensors <- function(res, convert) {
  convert_tensor <- function(res, convert) {
    if (convert && inherits(res, "tensorflow.python.framework.ops.Tensor"))
      invisible(res$eval())
    else
      invisible(res)
  }
  if (is.list(res)) {
    lapply(res, function(item) {
      convert_tensor(item, convert)
    })
  } else {
    convert_tensor(res, convert)
  }
}
