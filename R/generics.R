

#' @importFrom utils str
#' @export
print.tensorflow.tensor <- function(x, ...) {
  s <- py_str(x, ...)
  # strip the trailing comma in 1d vector shapes
  # gsub for sparse tensors or similar packed structures
  s <- gsub("shape=\\((None|[[:digit:]]+),\\), dtype=",
            "shape=(\\1), dtype=", s)
  writeLines(s)
  invisible(x)
}



#' @exportS3Method pillar::type_sum
type_sum.tensorflow.tensor <- function(x) {

  if(py_is_null_xptr(x))
    return("<pointer: 0x0>")

  x <- py_repr(x)
  x <- strsplit(x, "\n", fixed = TRUE)[[1L]]
  if (length(x) > 1L)
    x <- paste0(x[[1]], "\u2026>") # 1-char width ellipsis "..."

  # strip the trailing comma in 1d vector shapes
  x <- sub("shape=\\((None|[[:digit:]]+),\\)", "shape=(\\1)", x)

  x
}
# TODO maybe: a nicer print for  print.tensorflow.python.framework.sparse_tensor.SparseTensor




#' @export
str.tensorflow.tensor <- function(object, ...) {
  writeLines(type_sum.tensorflow.tensor(object))
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
dim.tensorflow.tensor <- function(x) {
  x <- x$shape
  if (is.na(length(x)))
    NULL
  else
    as.integer(x)
}



#' @export
length.tensorflow.tensor <- function(x) {
  x$shape$num_elements() %||% NA_integer_
}


#' @export
is.finite.tensorflow.tensor <-
  function(x) tf$math$is_finite(x)

#' @export
is.infinite.tensorflow.tensor <-
  function(x) tf$math$is_inf(x)

#' @export
is.nan.tensorflow.tensor <-
  function(x) tf$math$is_nan(x)


# https://stat.ethz.ch/R-manual/R-devel/library/base/html/InternalMethods.html

# extract `[.tensorflow.tensor` in R/extract.R

# https://stat.ethz.ch/R-manual/R-devel/library/base/html/groupGeneric.html

#' @export
"+.tensorflow.tensor" <- function(a, b) {
  if (missing(b)) a
  else {
    autocast_ab_to_tensors()
    tf$add(a, b)
  }
}

#' @export
"-.tensorflow.tensor" <- function(a, b) {
  if (missing(b)) {
    if (py_has_attr(tf, "negative"))
      tf$negative(a)
    else
      tf$neg(a)
  } else {
    autocast_ab_to_tensors()
    if (py_has_attr(tf, "subtract"))
      tf$subtract(a, b)
    else
      tf$sub(a, b)
  }
}


#' @export
"*.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  if (py_has_attr(tf, "multiply"))
    tf$multiply(a, b)
  else
    tf$mul(a, b)
}

#' @export
"/.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  tf$truediv(a, b)
}


#' @export
"%/%.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  switch_fun_if_tf(tf$floordiv, tf$math$floordiv)(a, b)
}


#' @export
"%%.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  switch_fun_if_tf(tf$mod, tf$math$mod)(a, b)
}


#' @export
"^.tensorflow.tensor" <- function(a, b) {

  if (is.numeric(b)) {
    if (isTRUE(b == 2))
      return(tf$square(a))
    if (isTRUE(b == .5))
      return(tf$sqrt(a))
    else if (is_integerish(b))
      b <- as.integer(b)
  }

  if(is.numeric(a))
    a <- as_tensor(a, dtype = b$dtype)

  tf$pow(a, b)
}


#' @export
"&.tensorflow.tensor" <- function(a, b) {
  a <- tf$cast(a, "bool")
  b <- tf$cast(b, "bool")
  tf$logical_and(a, b)
}


#' @export
"|.tensorflow.tensor" <- function(a, b) {
  a <- tf$cast(a, "bool")
  b <- tf$cast(b, "bool")
  tf$logical_or(a, b)
}


#' @export
"!.tensorflow.tensor" <- function(x) {
  x <- tf$cast(x, "bool")
  tf$logical_not(x)
}


#' @export
"==.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  tf$equal(a, b)
}


#' @export
"!=.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  tf$not_equal(a, b)
}


#' @export
"<.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  tf$less(a, b)
}


#' @export
"<=.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  tf$less_equal(a, b)
}


#' @export
">.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
  tf$greater(a, b)
}


#' @export
">=.tensorflow.tensor" <- function(a, b) {
  autocast_ab_to_tensors()
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
  if (missing(base) || identical(base, exp(1)))
    tf$math$log(x)
  else {
    base <- tf$convert_to_tensor(base, x$dtype)
    tf$truediv(tf$math$log(x),
               tf$math$log(base))
  }
}

#' @export
#' @method log2 tensorflow.tensor
"log2.tensorflow.tensor" <- function(x) {
  log.tensorflow.tensor(x, base = 2)
}

#' @export
#' @method log10 tensorflow.tensor
"log10.tensorflow.tensor" <- function(x) {
  log.tensorflow.tensor(x, base = 10)
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
  tf$sin(tf$multiply(x, tf$constant(pi, x$dtype)))
}

#' @export
#' @method cospi tensorflow.tensor
"cospi.tensorflow.tensor" <- function(x) {
  tf$cos(tf$multiply(x, tf$constant(pi, x$dtype)))
}

#' @export
#' @method tanpi tensorflow.tensor
"tanpi.tensorflow.tensor" <- function(x) {
  tf$tan(tf$multiply(x, tf$constant(pi, x$dtype)))
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

# ---- Reduce (max() and friends) ----

new_tf_reduce_generic <- function(generic, callable) {
  # all the reduce functions have the same signature:
  #  input_tensor, axis=None, keepdims=False, name=None
  #  need na.rm handling here because it's auto added to the call by the dispatch mechanism
  callable <- substitute(callable)
  generic <- substitute(generic)
  sig <- alist(x = , ... = ,
               axis = NULL, keepdims = FALSE, name = NULL,
               na.rm = FALSE)
  sig$name <- paste0("R/", deparse(substitute(generic)))

  body <- substitute({
    if (!isFALSE(na.rm))
      warning("NA not supported by Tensors. `na.rm` argument ignored")

    if (length(list(...))) {
      if (!is.null(axis) || !isFALSE(keepdims))
        stop("`axis` and `keepdims` can only be supplied when reducing only one tensor")
      x <- list(x, ...)
    }

    if (is.list(x))
      x <- tf$stack(lapply(x, generic),
                    name = name)

    callable(x, axis = as_axis(axis), keepdims = keepdims, name = name)
  }, list(callable = callable, generic = generic))

  as.function.default(c(sig, body), envir = parent.env(environment()))
}



as_axis <- function(axis) {
  if(is.null(axis))
    return(NULL)

  if (length(axis) > 1)
    return(tuple(lapply(axis, as_axis)))

  axis <- as.integer(axis)
  if(axis == 0L)
    stop("`axis` argument is 1 based, received 0")

  if(axis > 0L)
    axis - 1L
  else
    axis
}


#' @export
all.tensorflow.tensor <- new_tf_reduce_generic(all, tf$reduce_all)

#' @export
any.tensorflow.tensor <- new_tf_reduce_generic(any, tf$reduce_any)

#' @export
sum.tensorflow.tensor <- new_tf_reduce_generic(sum, tf$reduce_sum)

#' @export
prod.tensorflow.tensor <- new_tf_reduce_generic(prod, tf$reduce_prod)

#' @export
min.tensorflow.tensor <- new_tf_reduce_generic(min, tf$reduce_min)

#' @export
max.tensorflow.tensor <- new_tf_reduce_generic(max, tf$reduce_max)

#' @export
mean.tensorflow.tensor <-
function (x, ..., axis = NULL, keepdims = FALSE, name = "R/mean") {
  if (length(list(...))) {
    dots <- list(...)
    if(!is.null(dots$na.rm) || isFALSE(dots$na.rm)) {
      warning("NA not supported by Tensors. `na.rm` argument ignored")
      dots$na.rm <- NULL
    }
    if(!is.null(dots$trim) || dots$trim == 0) {
      stop("`trim` argument not supported for Tensors.")
      dots$trim <- NULL
    }
    if(length(dots))
      stop("unrecognized arguments passed to mean() method for tensors")
  }

  tf$reduce_mean(x, axis = as_axis(axis), keepdims = keepdims, name = name)
}

#' @export
range.tensorflow.tensor <-
function(x, ..., axis = NULL, keepdims = FALSE, name = "R/range", na.rm = FALSE) {
  if (!isFALSE(na.rm))
    warning("NA not supported by Tensors. `na.rm` argument ignored")

  lower <- min(x, ..., axis = axis, keepdims = keepdims, name = name)
  upper <- max(x, ..., axis = axis, keepdims = keepdims, name = name)
  tf$stack(list(lower, upper), name = name)
}


# ---- cbind / rbind() ----


#' @export
rbind.tensorflow.tensor <-
function(..., deparse.level = 0L, dtype = NULL, name = "R/rbind") {

  if(deparse.level != 0)
    warning("Tensors do not support dimnames; deparse.level argument ignored")

  dots <- list(...)

  if(is.null(dtype))
    dtype <- reduce_dtypes(dots)

  shape <- reduce_dims(dots, binding_on = "rows")

  values <- lapply(dots, as_tensor, dtype = dtype, shape = shape)

  if(length(shape) == 1)
    tf$stack(values, axis = 0L, name = name)
  else # length(shape) == 2
    tf$concat(values, axis = 0L, name = name)
}


#' @export
cbind.tensorflow.tensor <-
function(..., deparse.level = 0L, dtype = NULL, name = "R/cbind") {
  dots <- list(...)

  if(deparse.level != 0)
    warning("Tensors do not support dimnames; deparse.level argument ignored")

  if(is.null(dtype))
    dtype <- reduce_dtypes(dots)

  shape <- reduce_dims(dots, binding_on = "cols")

  values <- lapply(dots, as_tensor, dtype = dtype, shape = shape)

  if(length(shape) == 1)
    tf$stack(values, axis = 1L, name = name)
  else # length(shape) == 2
    tf$concat(values, axis = 1L, name = name)
}



reduce_dims <- function(tensors_andor_atomics, binding_on = c("cols", "rows")) {

  binding_on <- match.arg(binding_on)

  dims <- lapply(tensors_andor_atomics,
                 function(x) dim(x) %||% length(x))

  max_rank <- max(lengths(dims))

  if(max_rank == 0L) {
    shape <- c(1L)
    return(shape)
  }

  if(max_rank == 1L) {
    shape <- do.call(max, dims)
    return(shape)
  }

  if(max_rank != 2L)
    stop("cbind can only accept tensors where length(dim(x)) <= 2")


  if(binding_on == "rows") {

    ncols <- sapply(dims, function(d) {
      if (length(d) == 2L) d[2L]
      else if (length(d) == 1L) 1L
      else 1L
    })

    ncols <- setdiff(ncols, 1L)
    if(length(ncols) == 0) # all scalars
      return(1L)

    if(length(ncols) != 1)
      stop("matrix tensors must have a common number of columns")

    shape <- c(-1L, ncols)
    return(shape)

  } else if (binding_on == "cols") {

    nrows <- sapply(dims, function(d) {
      if (length(d) == 2L) d[1L]
      else if (length(d) == 1L) d
      else 1L
    })

    nrows <- setdiff(nrows, 1L)

    if (length(nrows) == 1)
      shape <- c(nrows, -1L)
    else if(length(nrows) == 0) # all scalars
      shape <- 1L
    else
      stop("matrix tensors must have a common number of rows")
  }

}


reduce_dtypes <- function(tensors_andor_atomics) {
  dtypes <- unique(unlist(lapply(
    tensors_andor_atomics,
    function(x) if(is_tensor(x)) x$dtype$name)))

  if(length(dtypes) == 0)
    return(NULL)
  if(length(dtypes) == 1)
    return(dtypes)

  # if all the tf.tensor dtypes are of the same family, cast to the widest member
  fam <- unique(sub("^([[:alpha:]]+)([[:digit:]]+)", "\\1", dtypes))
  if(length(fam) == 1)
    return(dtypes[which.max(as.integer(sub(fam, "", dtypes)))])

  # if we couldn't reduce to one dtype, do nothing and let python throw the
  # exception
  NULL
}

# ---- t() / aperm() ----

#' @export
aperm.tensorflow.tensor <-
  function(a, perm = NULL, ..., conjugate = FALSE, name = 'R/aperm') {
    chkDots(...)
    if (!is.null(perm))
      perm <- as.integer(perm) - 1L
    tf$transpose(a, perm, conjugate, name = name)
  }


#' @export
t.tensorflow.tensor <- function(x) {
  ndim <- length(dim(x))
  if(ndim == 0L)
    tf$reshape(x, c(1L, 1L), name = "R/t")
  else if(ndim == 1L)
    tf$expand_dims(x, 0L, name = "R/t")
  else if(ndim == 2L)
    tf$transpose(x, name = "R/t")
  else
    stop("x must be a matrix or 1d vector. Use aperm() for nd arrays.")
}



#' @export
sort.tensorflow.tensor <-
function(x, decreasing = FALSE, ..., axis = -1L, name = NULL) {
  if(length(list(...)))
    stop("Unrecognized arguments in ...")
  tf$sort(
    x,
    axis = as_axis(axis),
    direction = if (decreasing) "DESCENDING" else "ASCENDING",
    name = name
  )
}


#' @export
rep.tensorflow.tensor <- function(x, times, ..., name = "R/rep") {
  if(length(list(...)))
    stop("rep() method for tensors only supports 2 arguments, see tf.tile() docs.")

  if(length(times) != 1)
    stop("`times` argument must be a scalar for rep() Tensor")

  times <- as_tensor(times, dtype = "int32", shape = 1L, name = name)

  if (identical(ndim <- as_r_value(x$ndim), 0L))
    return(tf$fill(times, x, name = name))

  if(!identical(ndim, 1L))
    stop("rep() Tensor method only compatible with 1d vectors.",
         " Use tf$tile() directly to expand an nd-array")

  tf$tile(x, times, name = name)
}

# ---- Complex ----

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


# TODO: do something similar for KerasTensor
autocast_ab_to_tensors <- function()
  eval.parent(quote({
    if (!inherits(a, "tensorflow.tensor") && inherits(b, "tensorflow.tensor"))
      a <- as_tensor(a, b$dtype)
    else if (!inherits(b, "tensorflow.tensor") && inherits(a, "tensorflow.tensor"))
      b <- as_tensor(b, a$dtype)
  }))


# ---- as_tensor ----

#' as_tensor
#'
#' Coerce objects to tensorflow tensors (potentially of a specific dtype or shape). The
#' provided default methods will call
#' [`tf$convert_to_tensor`](https://www.tensorflow.org/api_docs/python/tf/convert_to_tensor). Depending on arguments supplied it may also call some combination of
#'  - [`tf$saturate_cast`](https://www.tensorflow.org/api_docs/python/tf/dtypes/saturate_cast) or
#'    [`tf$cast`](https://www.tensorflow.org/api_docs/python/tf/cast)
#'  - [`tf$fill`](https://www.tensorflow.org/api_docs/python/tf/fill) or
#'    [`tf$reshape`](https://www.tensorflow.org/api_docs/python/tf/reshape)
#'
#'
#' @param x object to convert
#' @param dtype `NULL`, a tensorflow dtype (`tf$int32`), or something coercible
#'   to one (e.g. a string `"int32"`)
#' @param ..., ignored
#' @param shape an integer vector, tensor, or `tf.TensorShape`. Can contain up
#'   to 1 unspecified dimension, encoded as a `-1` or `NA`. This will reshape
#'   `x` using row-major (C-style) semantics. It will prefer reshaping using
#'   non-graph operations if possible, but will otherwise invoke `tf$reshape()`.
#'   If `x` is a scalar and the requested `shape` is fully defined or a tensor,
#'   the value of `x` will be recycled to fill a tensor of the requested shape
#'   (it will dispatch to `tf$fill()`).
#' @param name `NULL` or a string. Useful for debugging in graph mode, ignored
#'   while in eager mode.
#'
#' @return a tensorflow tensor
#'
#' @export
#'
#' @examples
#' \dontrun{
#' as_tensor(42, "int32")
#' as_tensor(as_tensor(42))
#' }
as_tensor <- function(x, dtype = NULL, ..., name = NULL) UseMethod("as_tensor")

#' @rdname as_tensor
#' @export
as_tensor.default <- function(x, dtype = NULL, ..., shape = NULL, name = NULL) {

  if (!is.null(shape) && !is_tensor(shape)) {
    shape <- as.integer.tensorflow.python.framework.tensor_shape.TensorShape(
      tensorflow::shape(dims = shape))

    if (partially_defined_shape <- any(undefined <- is.na(shape)))
      shape[undefined] <- -1L

    # if x is scalar and (shape fully defined or a tensor): tf$fill()
    # else if x is R atomic or np.ndarray: np$reshape()
    # else: tf$reshape()

    if(identical(dx <- dim(x), shape))
      shape <- NULL
    else if ((is.atomic(x) || inherits(x, "numpy.ndarray")) &&
             !is_scalar(x)) {
      # prefer reshaping off the graph if possible
      np <- import("numpy", convert = FALSE)
      x <- np$reshape(as.array(x), tuple(as.list(shape)), "C")
      shape <- NULL
    }
  }

  # doubles get cast to float32, which is inconsistent with the dtype
  # of other double vector conversions. For consistency, cast to float64
  if(is.double(x) && !is.array(x))
    x <- tf$convert_to_tensor(x, name=name, dtype = "float64")
  else
  # dtype_hint() arg in convert_to_tensor() calls tf$constant(),
  # can silently overflow e.g., tf$convert_to_tensor(-1L, "uint8") --> 255
    x <- tf$convert_to_tensor(x, name = name)

  if (!is.null(dtype)) {
    dtype <- tf$as_dtype(dtype)

    if((  dtype$is_floating ||   dtype$is_integer) &&
       (x$dtype$is_floating || x$dtype$is_integer))
      x <- tf$dtypes$saturate_cast(x, dtype, name = name)
    else
      x <- tf$cast(x, dtype, name = name)

  }

  if (!is.null(shape)) {
    # shape supplied to tf$fill and tf$reshape must be rank 1;
    # expand dims rank if rank 0
    if(is.numeric(shape))
      shape <- as.list(shape)
    else if (is_tensor(shape) && shape$shape$rank == 0)
      shape <- tf$expand_dims(shape, axis = 0L, name = name)

    if (is_scalar(x) && (is_tensor(shape) || !partially_defined_shape))
      x <- tf$fill(shape, tf$squeeze(x), name = name)
    else
      x <- tf$reshape(x, shape, name = name)
  }

  x
}

#' @rdname as_tensor
#' @export
as_tensor.double <- function(x, dtype = NULL, ..., name = NULL) {
  if (!is.null(dtype)) {
    dtype <- tf$as_dtype(dtype)
    if (dtype$is_integer) {
      # tf.cast() overflows quietly, at least R raises a warning (and produces NA)
      # tf.dtypes.saturate_cast() is better, but still can be surprising.
      # Users must opt in to saturate cast (or overflow cast)
      # by casting in a separate call after converting to tensor.
      if (dtype$max <= .Machine$integer.max)
        storage.mode(x) <- "integer"

      if (anyNA(x))
        stop("converting R numerics with NA values to integer dtypes not supported")
    }
  }

  NextMethod()
}



if(getRversion() < "3.4.2")
  isFALSE <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x) && !x
  }



#' @export
py_to_r.tensorflow.python.trackable.data_structures._DictWrapper <- function(x) {
  import_builtins()$dict(x)
}


#' @export
py_to_r.tensorflow.python.trackable.data_structures.ListWrapper <- function(x) {
  import_builtins()$list(x)
}

# tf_version <= 2.9
#' @export
py_to_r.tensorflow.python.training.tracking.data_structures.ListWrapper <-
  py_to_r.tensorflow.python.trackable.data_structures.ListWrapper

#' @export
py_to_r.tensorflow.python.training.tracking.data_structures._DictWrapper <-
  py_to_r.tensorflow.python.trackable.data_structures._DictWrapper



# For tf_version() >= "2.16"
## Conditionally export these generics, if keras3 hasn't already exported them.
## We do this to keep keras3 and tensorflow decoupled, but to avoid
## "S3 method overwritten" warnings if both packages are loaded.
##
## Note, we still may need to revisit this; either to disable it, or export a custom $<- method
## for base classes like Layer, so that compound assignment expressions aren't a
## problem.

#' @export
py_to_r.keras.src.utils.tracking.TrackedDict <- function(x) import("builtins")$dict(x)

#' @export
py_to_r.keras.src.utils.tracking.TrackedList <- function(x) import("builtins")$list(x)

#' @export
py_to_r.keras.src.utils.tracking.TrackedSet <- function(x) import("builtins")$list(x)

#' @rawNamespace if (getRversion() >= "4.3.0") S3method("%*%",tensorflow.tensor)
`%*%.tensorflow.tensor` <- function(x, y) {
  if (is.atomic(x) && is_tensor(y)) {
    if (length(x) > 1L)
      x <- as.array(x)
    x <- tf$convert_to_tensor(x, dtype = y$dtype)
  } else if (is_tensor(x) && is.atomic(y)) {
    if (length(y) > 1L)
      y <- as.array(y)
    y <- tf$convert_to_tensor(y, dtype = x$dtype)
  }
  NextMethod()
}
