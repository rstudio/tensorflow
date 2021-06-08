

#' @importFrom utils str
#' @export
"print.tensorflow.tensor" <- function(x, ...) {
  if (py_is_null_xptr(x))
    cat("<pointer: 0x0>\n")
  else {
    str(x, ...)
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

    if (inherits(x, "tensorflow.python.ops.ragged.ragged_tensor.RaggedTensor"))
      shape <- x$shape
    else
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
  else if(identical(dx <- dim(x), list()))
    1L
  else
    Reduce(`*`, dx)
}

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/InternalMethods.html

# extract `[.tensorflow.tensor` in R/extract.R

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/groupGeneric.html

#' @export
"+.tensorflow.tensor" <- function(a, b) {
  if (missing(b)) a else tf$add(a, b)
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
  switch_fun_if_tf(tf$floordiv, tf$math$floordiv)(a, b)
}


#' @export
"%%.tensorflow.tensor" <- function(a, b) {
  switch_fun_if_tf(tf$mod, tf$math$mod)(a, b)
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
  switch_fun_if_tf(tf$ceil, tf$math$ceil)(x)
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
expm1.tensorflow.tensor <- function(x) {
  tf$math$expm1(x)
}


#' @export
"log.tensorflow.tensor" <- function(x, base = exp(1)) {
  if (is_tensor(base) || base != exp(1)) {
    base <- tf$convert_to_tensor(base, x$dtype)
    tf$math$log(x) / tf$math$log(base)
  } else
    tf$math$log(x)
}

#' @export
#' @method log2 tensorflow.tensor
"log2.tensorflow.tensor" <- function(x) {
  log(x, base = 2)
}

#' @export
#' @method log10 tensorflow.tensor
"log10.tensorflow.tensor" <- function(x) {
  log(x, base = 10)
}

#' @export
#' @method log1p tensorflow.tensor
log1p.tensorflow.tensor <- function(x) {
  tf$math$log1p(x)
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
#' @method sinpi tensorflow.tensor
"sinpi.tensorflow.tensor" <- function(x) {
  tf$sin(tf$constant(pi, dtype = x$dtype) * x)
}

#' @export
#' @method cospi tensorflow.tensor
"cospi.tensorflow.tensor" <- function(x) {
  tf$cos(tf$constant(pi, x$dtype) * x)
}

#' @export
#' @method tanpi tensorflow.tensor
"tanpi.tensorflow.tensor" <- function(x) {
  tf$tan(tf$constant(pi, x$dtype) * x)
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
  switch_fun_if_tf(tf$lgamma, tf$math$lgamma)(x)
}

#' @export
"digamma.tensorflow.tensor" <- function(x) {
  switch_fun_if_tf(tf$digamma, tf$math$digamma)(x)
}


#' @export
Re.tensorflow.tensor <- function(z) {
  switch_fun_if_tf(tf$real, tf$math$real)(z)
}

#' @export
Im.tensorflow.tensor <- function(z) {
  switch_fun_if_tf(tf$imag, tf$math$imag)(z)
}

#' @export
Conj.tensorflow.tensor <- function(z) {
  switch_fun_if_tf(tf$conj, tf$math$conj)(z)
}

#' @export
Arg.tensorflow.tensor <- function(z) {
  switch_fun_if_tf(tf$angle, tf$math$angle)(z)
}

#' @export
Mod.tensorflow.tensor <- function(z) {
  switch_fun_if_tf(tf$abs, tf$math$abs)(z)
}

switch_fun_if_tf <- function(x, y, version = "1.14") {
  if (!tensorflow::tf_version() >= version)
    x
  else
    y
}
