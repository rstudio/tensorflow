
#' TensorFlow for R
#'
#' \href{https://www.tensorflow.org}{TensorFlow} is an open source software library
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
#' \href{https://tensorflow.rstudio.com}{https://tensorflow.rstudio.com}
#'
#' @import reticulate
#'
#' @docType package
#' @name tensorflow
NULL

# globals
.globals <- new.env(parent = emptyenv())
.globals$tensorboard <- NULL

.onLoad <- function(libname, pkgname) {

  # if TENSORFLOW_PYTHON is defined then forward it to RETICULATE_PYTHON
  tensorflow_python <- Sys.getenv("TENSORFLOW_PYTHON", unset = NA)
  if (!is.na(tensorflow_python))
    Sys.setenv(RETICULATE_PYTHON = tensorflow_python)

  # delay load tensorflow
  tf <<- import("tensorflow", delay_load = list(

    priority = 5,

    environment = "r-tensorflow",

    on_load = function() {

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

      # if we loaded tensorflow then register tf help handler
      register_tf_help_handler()
    }
    ,

    on_error = function(e) {
      stop(tf_config_error_message(), call. = FALSE)
    }

  ))

  # provide a common base S3 class for tensors
  reticulate::register_class_filter(function(classes) {
    if (any(c("tensorflow.python.ops.variables.Variable",
              "tensorflow.python.framework.ops.Tensor")
            %in%
            classes)) {
      c("tensorflow.tensor", classes)
    } else {
      classes
    }
  })

}



#' TensorFlow configuration information
#'
#' @return List with information on the current configuration of TensorFlow.
#'   You can determine whether TensorFlow was found using the `available`
#'   member (other members vary depending on whether `available` is `TRUE`
#'   or `FALSE`)
#'
#' @keywords internal
#' @export
tf_config <- function() {

  # first check if we found tensorflow
  have_tensorflow <- py_module_available("tensorflow")

  # get py config
  config <- py_config()

  # found it!
  if (have_tensorflow) {

    # get version
    tfv <- strsplit(tf$VERSION, ".", fixed = TRUE)[[1]]
    version <- package_version(paste(tfv[[1]], tfv[[2]], sep = "."))

    structure(class = "tensorflow_config", list(
      available = TRUE,
      version = version,
      version_str = tf$VERSION,
      location = config$required_module_path,
      python = config$python,
      python_version = config$version
    ))

  # didn't find it
  } else {
    structure(class = "tensorflow_config", list(
      available = FALSE,
      python_verisons = config$python_versions,
      error_message = tf_config_error_message()
    ))
  }
}


#' @rdname tf_config
#' @keywords internal
#' @export
tf_version <- function() {
  config <- tf_config()
  if (config$available)
    config$version
  else
    NULL
}

#' @export
print.tensorflow_config <- function(x, ...) {
  if (x$available) {
    aliased <- function(path) sub(Sys.getenv("HOME"), "~", path)
    cat("TensorFlow v", x$version_str, " (", aliased(x$location), ")\n", sep = "")
    cat("Python v", x$python_version, " (", aliased(x$python), ")\n", sep = "")
  } else {
    cat(x$error_message, "\n")
  }
}


# Build error message for TensorFlow configuration errors
tf_config_error_message <- function() {
  message <- "Installation of TensorFlow not found."
  config <- py_config()
  if (!is.null(config)) {
    if (length(config$python_versions) > 0) {
      message <- paste0(message,
                        "\n\nPython environments searched for 'tensorflow' package:\n")
      python_versions <- paste0(" ", normalizePath(config$python_versions, mustWork = FALSE),
                                collapse = "\n")
      message <- paste0(message, python_versions, sep = "\n")
    }
  }
  message <- paste0(message,
                    "\nYou can install TensorFlow using the install_tensorflow() function.\n")
  message
}


