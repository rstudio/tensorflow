
#' TensorFlow Probability Module
#'
#' @return Reference to [TensorFlow Probability](https://www.tensorflow.org/probability)
#'   functions and classes
#'
#' @examples \dontrun{
#' library(tensorflow)
#' ## one time setup:
#' # reticulate::py_install("tensorflow_probability")
#' tfp <- tf_probability()
#' tfp$distributions$Normal(loc = 0, scale = 1)
#' }
#'
#' @export
tf_probability <- function() {

  # ensure that tensorflow is loaded
  ensure_loaded()

  # return module
  import("tensorflow_probability")
}



