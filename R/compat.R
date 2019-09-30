
#' Use Compatibility
#'
#' Enables TensorFlow to run under a different API version for compatibility
#' with previous versions. For instance, this is useful to run TensorFlow 1.x
#' code when using TensorFlow 2.x.
#'
#' @param version The version to activate. Must be `"v1"` or `"v2"`
#'
#' @examples
#' \dontrun{
#' library(tensorflow)
#' use_compat("v1")
#' }
#'
#' @export
use_compat <- function(version = c("v1", "v2")) {
  if (identical(tf_version(), NULL)) return()

  version <- match.arg(version)

  tf2 <- tf

  unlock <- get("unlockBinding")
  lock <- get("lockBinding")

  unlock("tf",  as.environment("package:tensorflow"))
  on.exit(lock("tf",  as.environment("package:tensorflow")))

  assign("tf",  tf$compat[[version]], as.environment("package:tensorflow"))

  invisible(tf2)
}
