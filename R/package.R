
#' TensorFlow for R
#'
#' \href{https://www.tensorflow.org}{TensorFlow} is an open source software library
#' for numerical computation using data flow graphs. Nodes in the graph
#' represent mathematical operations, while the graph edges represent the
#' multidimensional data arrays (tensors) communicated between them. The
#' flexible architecture allows you to deploy computation to one or more CPUs or
#' GPUs in a desktop, server, or mobile device with a single API.
#'
#' The \href{https://www.tensorflow.org/api_docs/python/tf/all_symbols}{TensorFlow
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
#' @aliases tensorflow-package
#' @name tensorflow
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end


tf_v2 <- function() {
  # 1.14 already shows deprecation warnings.
  package_version(tf_version()) >= "2.0"
}

# globals
.globals <- new.env(parent = emptyenv())
.globals$tensorboard <- NULL

#' @export
#' @rdname install_tensorflow
py_require_tensorflow <- function(use_gpu = NA) {
  # register requirements with py_require()
  reqs <- get_py_requirements(use_gpu)
  reticulate::py_require(reqs$packages, reqs$python_version)
}

.onLoad <- function(libname, pkgname) {

  # if (is.na(Sys.getenv("TF_CPP_MIN_LOG_LEVEL", NA))) {
  #   ## Doesn't seem to make a difference
  #   Sys.setenv("TF_CPP_MIN_LOG_LEVEL" = "3")
  # }
  # 0 = all messages are logged (default behavior)
  # 1 = INFO messages are not printed
  # 2 = INFO and WARNING messages are not printed
  # 3 = INFO, WARNING, and ERROR messages are not printed


  # if TENSORFLOW_PYTHON is defined then forward it to RETICULATE_PYTHON
  tensorflow_python <- Sys.getenv("TENSORFLOW_PYTHON", unset = NA)
  if (!is.na(tensorflow_python))
    Sys.setenv(RETICULATE_PYTHON = tensorflow_python)

  # honor option to silence cpp startup logs (INFO, level 1),
  # but insist on printing warnings (level 2) and errors (level 3)
  cpp_log_opt <- getOption("tensorflow.core.cpp_min_log_level")
  if (!is.null(cpp_log_opt))
    Sys.setenv(TF_CPP_MIN_LOG_LEVEL = max(min(cpp_log_opt, 3), 0))

  if (tolower(Sys.getenv("PY_REQUIRE_TENSORFLOW")) %in% c("1", "true", "yes")) {
    py_require_tensorflow()
  }

  # delay load tensorflow
  tryCatch({

    tf <<- import("tensorflow", delay_load = list(

    priority = 5, # keras sets priority = 10

    environment = c(
      "r-tensorflow",
      if (as.package_version(getNamespaceVersion("reticulate")) >= "1.36.0")
         "r-keras"
    ),

    # before_load = function() {
    #
    # },

    on_load = function() {

      # register warning suppression handler
      register_suppress_warnings_handler(list(
        suppress = function() {
          if (tf_v2()) {
            tf_logger <- tf$get_logger()
            logging <- reticulate::import("logging")

            old_verbosity <- tf_logger$level
            tf_logger$setLevel(logging$ERROR)
            old_verbosity
          } else {
            old_verbosity <- tf$logging$get_verbosity()
            tf$logging$set_verbosity(tf$logging$ERROR)
            old_verbosity
          }
        },
        restore = function(context) {
          if (tf_v2()) {
            tf_logger <- tf$get_logger()
            tf_logger$setLevel(context)
          } else {
            tf$logging$set_verbosity(context)
          }
        }
      ))

      # if we loaded tensorflow then register tf help handler
      register_tf_help_handler()

      # workaround to silence crash-causing deprecation warnings
      tryCatch(tf$python$util$deprecation$silence()$`__enter__`(),
               error = function(e) NULL)

      # TODO: move this into .onAttach, where you either emit immediately if
      # already loaded otherwise register emit hook for reticulate
      # emit <- get("packageStartupMessage") # R CMD check
      # emit("Loaded TensorFlow version ", tf$version$VERSION)
    }
    ,

    on_error = function(e) {
      stop(tf_config_error_message(), call. = FALSE)
    }
  ))
  },
  python.builtin.ModuleNotFoundError = function(e) {
    warning(e$message, "\n",
    "Restart the R session and load the tensorflow R package before ",
    "reticulate has initialized Python, or ensure reticulate initialized ",
    "a Python installation where the tensorflow module is installed.", call. = FALSE)
  })


  # provide a common base S3 class for tensors
  reticulate::register_class_filter(function(classes) {
    if (any(c("tensorflow.python.ops.variables.Variable",
              "tensorflow.python.types.core.Tensor",       # 2.14
              "tensorflow.python.framework.tensor.Tensor", # 2.14
              "tensorflow.python.framework.ops.Tensor",
              "tensorflow.python.ops.ragged.ragged_tensor.RaggedTensor",
              "tensorflow.python.framework.sparse_tensor.SparseTensor")
            %in%
            classes)) {
      c("tensorflow.tensor", classes)
    } else {
      classes
    }
  })

}


is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
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
    if (reticulate::py_has_attr(tf, "version"))
      version_raw <- tf$version$VERSION
    else
      version_raw <- tf$VERSION

    tfv <- strsplit(version_raw, ".", fixed = TRUE)[[1]]
    version <- package_version(paste(tfv[[1]], tfv[[2]], sep = "."))

    structure(class = "tensorflow_config", list(
      available = TRUE,
      version = version,
      version_str = version_raw,
      location = config$required_module_path,
      python = config$python,
      python_version = config$version
    ))

    # didn't find it
  } else {
    structure(class = "tensorflow_config", list(
      available = FALSE,
      python_versions = config$python_versions,
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
    cat("Python v", as.character(x$python_version), " (", aliased(x$python), ")\n", sep = "")
  } else {
    cat(x$error_message, "\n")
  }
}

#' TensorFlow GPU configuration information
#'
#' @return A bool, whether GPU is configured or not, or NA if could not be
#' determined.
#'
#' @keywords internal
#' @param verbose boolean. Whether to show extra GPU info.
#' @export
tf_gpu_configured <- function(verbose=TRUE) {
  res <- tryCatch({
    tf$test$is_gpu_available()
  }, error = function(e) {
    warning("Can not determine if GPU is configured.", call. = FALSE);
    NA
  })

  if (!is.na(verbose) && is.logical(verbose) &&verbose) {
    tryCatch({
      cat(paste("TensorFlow built with CUDA: ", tf$test$is_built_with_cuda()),
          "\n");
      cat(paste("GPU device name: ", tf$test$gpu_device_name(),
                collapse = "\n"))
    }, error = function(e) {})
  }
  res
}


# Build error message for TensorFlow configuration errors
tf_config_error_message <- function() {
  message <- "Valid installation of TensorFlow not found."
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

  python_error <- tryCatch({
    import("tensorflow")
    list(message = NULL)
  },
  error = function(e) {
    on.exit(py_clear_last_error())
    py_last_error()
  })

  message <- paste0(message,
                    "\nPython exception encountered:\n ",
                    python_error$message, "\n")

  message <- paste0(message,
                    "\nYou can install TensorFlow using the install_tensorflow() function.\n")
  base::message(
    'Hint: To use tensorflow with `py_require()`, call `py_require("tensorflow")` at the start of the R session'
  )
  message
}
