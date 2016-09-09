
#' @useDynLib tensorflow
#' @importFrom Rcpp evalCpp
#' @importFrom utils str
NULL

.onLoad <- function(libname, pkgname) {

  # initialize python
  config <- py_config()
  py_initialize(config$libpython);

  # attempt to load top level tensorflow modules
  tf <<- tensorflow(silent = TRUE)
  tflearn <<- tensorflow("contrib.learn", silent = TRUE)
  slim <<- tensorflow("contrib.slim", silent = TRUE)
}


.onAttach <- function(libname, pkgname) {
  if (is.null(tf)) {
    packageStartupMessage("TensorFlow not currently installed, please see ",
                          "https://www.tensorflow.org/get_started/")
  }
}

.onUnload <- function(libpath) {
  py_finalize();
}


#' Import TensorFlow module
#'
#' Import a tensorflow module for use in R. Note that the most
#' commonly used modules (\code{tf}, \code{tflearn}, and \code{slim})
#' are pre-imported and available for use when the package is loaded.
#'
#' @inheritParams py_module
#' @param module Name of sub-module to import. Defaults to \code{NULL}, which
#' imports the main tensorflow module.
#'
#' @format A tensorflow module
#'
#' @details
#'
#' Three TensorFlow modules are pre-imported and ready for use when the
#' \pkg{tensorflow} package is loaded:
#'
#' \describe{
#'    \item{\code{tf}}{Main TensorFlow module (\href{https://www.tensorflow.org/api_docs/index.html}{\code{tensorflow}}).}
#'    \item{\code{learn}}{Higher-level API for TensorFlow (\href{https://github.com/tensorflow/tensorflow/tree/master/tensorflow/contrib/learn/python/learn}{\code{tensorflow.contrib.learn}}).}
#'    \item{\code{slim}}{Lightweight library for defining, training and
#'    evaluating complex models in TensorFlow  (\href{https://github.com/tensorflow/tensorflow/tree/master/tensorflow/contrib/slim}{\code{tensorflow.contrib.slim}}).}
#' }
#'
#' @examples
#' \dontrun{
#' metrics <- tensorflow("contrib.metrics")
#' reader <- tensorflow("models.rnn.ptb")
#' }
#'
#' @export
tensorflow <- function(module = NULL, silent = FALSE) {
  if (is.null(module))
    py_module("tensorflow", silent = silent)
  else
    py_module(paste("tensorflow", module, sep="."), silent = silent)
}

#' @rdname tensorflow
#' @export
tf <- NULL

#' @rdname tensorflow
#' @export
tflearn <- NULL

#' @rdname tensorflow
#' @export
slim <- NULL

#' Tensor shape
#'
#' @param ... Tensor dimensions
#'
#' @export
shape <- function(...) {
  dims <- list(...)
  lapply(dims, function(dim) {
    if (!is.null(dim))
      as.integer(dim)
    else
      NULL
  })
}

#' @export
"print.tensorflow.python.framework.ops.Tensor" <- function(x, ...) {
  print.python.object(x, ...)
  if (!is.null(tf$get_default_session())) {
    value <- tryCatch(x$eval(), error = function(e) NULL)
    if (!is.null(value))
      cat(" ", str(value), "\n", sep = "")
  }
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/groupGeneric.html

#' @export
"+.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$add(a, b)
}

#' @export
"+.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$add(a, b)
}

#' @export
"-.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  if (missing(b))
    tf$neg(a)
  else
    tf$sub(a, b)
}

#' @export
"-.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  if (missing(b))
    tf$neg(a)
  else
    tf$sub(a, b)
}

#' @export
"*.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$mul(a, b)
}

#' @export
"*.tensorflow.python.ops.variables.Variable" <- function(a, b) {
  tf$mul(a, b)
}

#' @export
"/.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$truediv(a, b)
}

#' @export
"/.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$truediv(a, b)
}

#' @export
"%/%.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$floordiv(a, b)
}

#' @export
"%/%.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$floordiv(a, b)
}

#' @export
"%%.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$mod(a, b)
}

#' @export
"%%.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$mod(a, b)
}

#' @export
"^.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$pow(a, b)
}

#' @export
"^.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$pow(a, b)
}

#' @export
"&.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$logical_and(a, b)
}

#' @export
"&.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$logical_and(a, b)
}

#' @export
"|.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$logical_or(a, b)
}

#' @export
"!.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$logical_not(x)
}

#' @export
"!.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$logical_not(x)
}

#' @export
"|.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$logical_or(a, b)
}

#' @export
"|.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$logical_or(a, b)
}

#' @export
"|.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$logical_or(a, b)
}

#' @export
"|.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$logical_or(a, b)
}

#' @export
"==.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$equal(a, b)
}

#' @export
"==.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$equal(a, b)
}

#' @export
"!=.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$not_equal(a, b)
}

#' @export
"!=.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$not_equal(a, b)
}

#' @export
"<.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$less(a, b)
}

#' @export
"<.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$less(a, b)
}

#' @export
"<=.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$less_equal(a, b)
}

#' @export
"<=.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$less_equal(a, b)
}

#' @export
">.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$greater(a, b)
}

#' @export
">.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$greater(a, b)
}

#' @export
">=.tensorflow.python.framework.ops.Tensor" <- function(a, b) {
  tf$greater_equal(a, b)
}

#' @export
">=.tensorflow.python.framework.ops.Variable" <- function(a, b) {
  tf$greater_equal(a, b)
}

#' @export
"abs.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$abs(x)
}

#' @export
"abs.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$abs(x)
}

#' @export
"sign.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$sign(x)
}

#' @export
"sign.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$sign(x)
}

#' @export
"sqrt.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$sqrt(x)
}

#' @export
"sqrt.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$sqrt(x)
}

#' @export
"floor.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$floor(x)
}

#' @export
"floor.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$floor(x)
}

#' @export
"ceiling.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$ceil(x)
}

#' @export
"ceiling.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$ceil(x)
}

#' @export
"round.tensorflow.python.framework.ops.Tensor" <- function(x, digits = 0) {
  if (digits != 0)
    stop("TensorFlow round only supports rounding to integers")
  tf$round(x)
}

#' @export
"round.tensorflow.python.framework.ops.Variable" <- function(x, digits = 0) {
  if (digits != 0)
    stop("TensorFlow round only supports rounding to integers")
  tf$round(x)
}

#' @export
"exp.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$exp(x)
}

#' @export
"exp.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$exp(x)
}

#' @export
"log.tensorflow.python.framework.ops.Tensor" <- function(x, base = exp(1)) {
  if (base != exp(1))
    stop("TensorFlow log suppports only natural logarithms")
  tf$log(x)
}

#' @export
"log.tensorflow.python.framework.ops.Variable" <- function(x, base = exp(1)) {
  if (base != exp(1))
    stop("TensorFlow log suppports only natural logarithms")
  tf$log(x)
}

#' @export
"cos.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$cos(x)
}

#' @export
"cos.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$cos(x)
}

#' @export
"sin.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$sin(x)
}

#' @export
"sin.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$sin(x)
}

#' @export
"tan.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$tan(x)
}

#' @export
"tan.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$tan(x)
}

#' @export
"acos.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$acos(x)
}

#' @export
"acos.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$acos(x)
}

#' @export
"asin.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$asin(x)
}

#' @export
"asin.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$asin(x)
}

#' @export
"atan.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$atan(x)
}

#' @export
"atan.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$atan(x)
}

#' @export
"lgamma.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$lgamma(x)
}

#' @export
"lgamma.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$lgamma(x)
}

#' @export
"digamma.tensorflow.python.framework.ops.Tensor" <- function(x) {
  tf$digamma(x)
}

#' @export
"digamma.tensorflow.python.framework.ops.Variable" <- function(x) {
  tf$digamma(x)
}


