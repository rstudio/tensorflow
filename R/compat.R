
#' Use Compatibility
#'
#' Enables TensorFlow to run under a different API version for compatibility
#' with previous versions. For instance, this is useful to run TensorFlow 1.x
#' code when using TensorFlow 2.x.
#'
#' @inheritParams reticulate py_set_seed
#'
#' @param version The version to activate.
#'
#' @examples
#' \dontrun{
#' library(tensorflow)
#' use_compat("v1")
#' }
#'
#' @export
use_compat <- function(version = c("v1")) {
  if (identical(tf_version(), NULL) || tf_version() < "2.0") return()

  # validate method
  version <- match.arg(version)

  tf2 <- tf

  # disable eager execution to match v1 default
  tf2$compat$v1$disable_eager_execution()

  unlock <- get("unlockBinding")
  lock <- get("lockBinding")

  unlock("tf",  as.environment("package:tensorflow"))
  on.exit(lock("tf",  as.environment("package:tensorflow")))

  assign("tf",  tf$compat[[version]], as.environment("package:tensorflow"))

  invisible(tf2)
}
