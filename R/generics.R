

#' @export
py_str.tensorflow.python.ops.variables.Variable <- function(object, ...) {
  paste0("Variable(shape=", py_str(object$get_shape()), ", ",
         "dtype=", object$dtype$name, ")\n", sep = "")
}

#' @importFrom utils str
#' @export
"print.tensorflow.tensor" <- function(x, ...) {
  if (py_is_null_xptr(x))
    cat("<pointer: 0x0>\n")
  else {
    str(x, ...)
    if (!is.null(tf$get_default_session())) {
      value <- tryCatch(x$eval(), error = function(e) NULL)
      if (!is.null(value))
        cat(" ", str(value), "\n", sep = "")
    }
  }
}

#' @importFrom utils .DollarNames
#' @export
.DollarNames.tensorflow.python.platform.flags._FlagValues <- function(x, pattern = "") {

  # skip if this is a NULL xptr
  if (py_is_null_xptr(x))
    return(character())

  # get the underlying flags and return the names
  flags <- x$`__flags`
  names(flags)
}

#' @export
"dim.tensorflow.tensor" <- function(x) {
  if (py_is_null_xptr(x))
    NULL
  else {
    shape <- x$get_shape()
    if (!is.null(shape$ndims))
      shape$as_list()
    else
      NULL
  }
}

#' @export
"length.tensorflow.tensor" <- function(x) {
  if (py_is_null_xptr(x))
    length(NULL)
  else
    Reduce(`*`, dim(x))
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/InternalMethods.html


#' @export
"[.tensorflow.tensor" <- function(x, ...) {

  call <- match.call()
  check_zero_based(call)

  one_based_extract <- getOption("tensorflow.one_based_extract", TRUE)

  basis <- ifelse(one_based_extract, 1, 0)
  call_list <- as.list(call)[-1]
  do.call(extract_manual,
          c(call_list, basis = basis),
          envir = parent.frame())
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/groupGeneric.html

#' @export
"+.tensorflow.tensor" <- function(a, b) {
  tf$add(a, b)
}

#' @export
"-.tensorflow.tensor" <- function(a, b) {
  if (missing(b)) {
    if (py_has_attr(tf, "negative"))
      tf$negative(a)
    else
      tf$neg(a)
  } else {
    if (py_has_attr(tf, "subtract"))
      tf$subtract(a, b)
    else
      tf$sub(a, b)
  }
}


#' @export
"*.tensorflow.tensor" <- function(a, b) {
  if (py_has_attr(tf, "multiply"))
    tf$multiply(a, b)
  else
    tf$mul(a, b)
}

#' @export
"/.tensorflow.tensor" <- function(a, b) {
  tf$truediv(a, b)
}


#' @export
"%/%.tensorflow.tensor" <- function(a, b) {
  tf$floordiv(a, b)
}


#' @export
"%%.tensorflow.tensor" <- function(a, b) {
  tf$mod(a, b)
}


#' @export
"^.tensorflow.tensor" <- function(a, b) {
  tf$pow(a, b)
}


#' @export
"&.tensorflow.tensor" <- function(a, b) {
  tf$logical_and(a, b)
}


#' @export
"|.tensorflow.tensor" <- function(a, b) {
  tf$logical_or(a, b)
}


#' @export
"!.tensorflow.tensor" <- function(x) {
  tf$logical_not(x)
}


#' @export
"==.tensorflow.tensor" <- function(a, b) {
  tf$equal(a, b)
}


#' @export
"!=.tensorflow.tensor" <- function(a, b) {
  tf$not_equal(a, b)
}


#' @export
"<.tensorflow.tensor" <- function(a, b) {
  tf$less(a, b)
}


#' @export
"<=.tensorflow.tensor" <- function(a, b) {
  tf$less_equal(a, b)
}


#' @export
">.tensorflow.tensor" <- function(a, b) {
  tf$greater(a, b)
}


#' @export
">=.tensorflow.tensor" <- function(a, b) {
  tf$greater_equal(a, b)
}


#' @export
"abs.tensorflow.tensor" <- function(x) {
  tf$abs(x)
}


#' @export
"sign.tensorflow.tensor" <- function(x) {
  tf$sign(x)
}


#' @export
"sqrt.tensorflow.tensor" <- function(x) {
  tf$sqrt(x)
}


#' @export
"floor.tensorflow.tensor" <- function(x) {
  tf$floor(x)
}


#' @export
"ceiling.tensorflow.tensor" <- function(x) {
  tf$ceil(x)
}


#' @export
"round.tensorflow.tensor" <- function(x, digits = 0) {
  if (digits != 0)
    stop("TensorFlow round only supports rounding to integers")
  tf$round(x)
}


#' @export
"exp.tensorflow.tensor" <- function(x) {
  tf$exp(x)
}

#' @export
"log.tensorflow.tensor" <- function(x, base = exp(1)) {
  if (base != exp(1))
    stop("TensorFlow log suppports only natural logarithms")
  tf$log(x)
}


#' @export
"cos.tensorflow.tensor" <- function(x) {
  tf$cos(x)
}


#' @export
"sin.tensorflow.tensor" <- function(x) {
  tf$sin(x)
}


#' @export
"tan.tensorflow.tensor" <- function(x) {
  tf$tan(x)
}


#' @export
"acos.tensorflow.tensor" <- function(x) {
  tf$acos(x)
}


#' @export
"asin.tensorflow.tensor" <- function(x) {
  tf$asin(x)
}


#' @export
"atan.tensorflow.tensor" <- function(x) {
  tf$atan(x)
}

#' @export
"lgamma.tensorflow.tensor" <- function(x) {
  tf$lgamma(x)
}

#' @export
"digamma.tensorflow.tensor" <- function(x) {
  tf$digamma(x)
}
