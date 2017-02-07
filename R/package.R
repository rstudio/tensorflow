
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

# package level mutable global state
.globals <- new.env(parent = emptyenv())
.globals$load_error_message <- NULL

.onLoad <- function(libname, pkgname) {

  # attempt to load tensorflow
  tf <<- tryCatch(import("tensorflow"), error = function(e) e)
  if (inherits(tf, "error")) {
    .globals$load_error_message <- tf$message
    tf <<- NULL
    return()
  }

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
}


.onAttach <- function(libname, pkgname) {

  if (is.null(tf)) {
    packageStartupMessage("\n", .globals$load_error_message)
    packageStartupMessage("\nIf you have not yet installed TensorFlow, see ",
                          "https://www.tensorflow.org/get_started/\n")
    packageStartupMessage("You should ensure that the version of python where ",
                          "tensorflow is installed is either the default python ",
                          "on the system PATH or is specified explicitly via the ",
                          "TENSORFLOW_PYTHON environment variable.\n")
    if (!is.null(.globals$py_config)) {
      packageStartupMessage("Detected Python configuration:\n")
      packageStartupMessage(str(.globals$py_config))
    }
  }
}
