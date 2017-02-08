
#' TensorFlow for R
#'
#' \href{https://tensorflow.org}{TensorFlow} is an open source software library
#' for numerical computation using data flow graphs. Nodes in the graph
#' represent mathematical operations, while the graph edges represent the
#' multidimensional data arrays (tensors) communicated between them. The
#' flexible architecture allows you to deploy computation to one or more CPUs or
#' GPUs in a desktop, server, or mobile device with a single API.
#'
#' The \href{https://www.tensorflow.org/api_docs/python/index.html}{TensorFlow
#' API} is composed of a set of Python modules that enable constructing and
#' executing TensorFlow graphs. The tensorflow package provides access to the
#' complete TensorFlow API from within R.
#'
#' For additional documentation on the tensorflow package see
#' \href{https://rstudio.github.io/tensorflow}{https://rstudio.github.io/tensorflow}
#'
#' @import reticulate
#'
#' @docType package
#' @name tensorflow
NULL

.onLoad <- function(libname, pkgname) {

  # if TENSORFLOW_PYTHON is defined then forward it to RETICULATE_PYTHON
  tensorflow_python <- Sys.getenv("TENSORFLOW_PYTHON", unset = NA)
  if (!is.na(tensorflow_python))
    Sys.setenv(RETICULATE_PYTHON = tensorflow_python)

  # delay load tensorflow
  tf <<- import("tensorflow", delay_load = function() {

    # register warning suppression handler
    register_suppress_warnings_handler(list(
      suppress = function() {
        old_verbosity <- tf$logging$get_verbosity()
        tf$logging$set_verbosity(tf$logging$ERROR)
        old_verbosity
      },
      restore = function(context) {
        tf$logging$set_verbosity(context)
      }
    ))

    # if we loaded tensorflow then register tf help topics
    register_tf_help_topics()
  })
}


