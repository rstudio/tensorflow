
#' TensorFlow Probability Module
#'
#' @return Reference to [TensorFlow Probability](https://www.tensorflow.org/probability)
#'   functions and classes
#'
#' @examples \dontrun{
#' library(tensorflow)
#' tfp <- tf_probability()
#' tfp$distributions$Normal(loc=0, scale=1)
#' }
#'
#' @export
tf_probability <- function() {

  # ensure that tensorflow is loaded
  ensure_loaded()

  # validate version
  if (tf_version() < "1.12") {
    stop("TensorFlow Probability requires version 1.12 or higher of TensorFlow ",
         "(you are currently running TensorFlow ", tf_version())
  }

  # return module
  import("tensorflow_probability")
}



