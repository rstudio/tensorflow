
#' @export
"[.tensorflow.tensor" <-
  function(x, ..., drop = getOption("tensorflow.drop_on_extract", TRUE)) {

    dots <- maybe_reparse_as_slice_spec_then_force(..., .env = parent.frame())

    one_based_extract <- getOption("tensorflow.one_based_extract")
    if(is.null(one_based_extract)){
      stop_if_any_zeros(dots)
      one_based_extract <- TRUE
    }
    check_is_TRUE_or_FALSE(one_based_extract)

    if (should_warn_about_negative_indices())
      warn_if_any_negative(dots)

    dots <- lapply(dots,
                   as_valid_py__getitem__arg,
                   one_based = one_based_extract,
                   drop = drop)


    # NULL == tf$newaxis
    if (length(dropNULL(dots)) != py_len(x$shape)) {

      is_ellipsis <- vapply(dots, function(d)
          inherits(d, "python.builtin.ellipsis"), FALSE)

      if (!any(is_ellipsis)) stop(
          "An incorrect number of dimensions was specified. The number of\n",
          "supplied arguments, (not counting any NULL, tf$newaxis or np$newaxis),\n",
          "must match the number of dimensions in the tensor, unless a\n",
          "py_ellipsis() was supplied")
      }

    # mirror the python parser, where single argumets in [] are passed
    # as-supplied, otherwise they wrapped in a tuple
    if(is_scalar(dots))
      dots <- dots[[1L]]
    else
      dots <- tuple(dots)

    x$`__getitem__`(dots)
  }

maybe_reparse_as_slice_spec_then_force <- function(..., .env) {
  dots <- eval(substitute(alist(...)))

  if (!is.null(names(dots))) {
    warning("names are ignored")
    names(dots) <- NULL
  }

  lapply(dots, function(d) {

    if(is_missing(d))
      return(py_slice())

    if (is_has_colon(d)) {
      d <- deparse(d, width.cutoff = 500, backtick = !is.name(d))
      d <- strsplit(d, ":", fixed = TRUE)[[1]]
      d[!nzchar(d)] <- "NULL"
      d <- lapply(d, function(a) eval(parse1(a), envir = .env))
      class(d) <- "py_slice_spec"
      return(d)
    }

    # else, eval normally
    eval(d, envir = .env)
  })
}

is_missing <- function(x) identical(x, quote(expr=))

parse1 <- function(text) parse(text = text, keep.source = FALSE)[[1]]

is_has_colon <- function(x) {

  if (is.call(x) && identical(x[[1]], quote(`:`))) # unbackticked call to :
    TRUE

  else if (is.name(x) && grepl(":", as.character(x), fixed = TRUE)) # backticked
    TRUE

  else
    FALSE
    # should return false if the colon is not the top level call, e.g. -(1:4)
}


should_warn_about_negative_indices <- (function() {
  user_was_warned <- FALSE

  function(x) {
    if (user_was_warned)
      FALSE
    else if (missing(x))
      !identical(
        getOption("tensorflow.warn_negative_extract_is_python_style"), FALSE)
    else
      user_was_warned <<- TRUE
  }
})()




as_valid_py__getitem__arg <- function(x, one_based, drop) {
  if (inherits(x, "py_slice_spec"))
    return(do.call(py_slice, c(x, one_based = one_based)))

  if(is.atomic(x) && (is.character(x) || anyNA(x) || any(is.infinite(x))))
    stop("NA, Inf, or character inputs not supported")

  if(is_scalar_integerish(x)) {
    x <- as.integer(x)
    if(one_based)
      x <- translate_one_based_to_zero_based(x)

    if(!drop) {
      # tf.__getitem__ drops dim on scalar indexes, but not on slices. Here we
      # convert the scalar index to an equivelant py_slice to preserve the dim
      if(x < 0)
        x <- py_slice(x, x-1L, -1L, one_based = FALSE, inclusive_stop = FALSE)
      else
        x <- py_slice(x, x+1L, one_based = FALSE, inclusive_stop = FALSE)
    }
    return(x)
  }

  if (is_integerish(x)) # a numeric sequence of length greater than 1
    return(R_slicing_sequence_to_py_slice(x, one_based)) # error checking within


  if (is_tensor(x))
    maybe_warn_no_translation_performed()

  # else, just pass the input and python will throw an error if it's invalid
  x
}


R_slicing_sequence_to_py_slice <- function(x, one_based) {
  # x is assumed to be integerish

  start <- x[1L]
  end <- x[length(x)]
  step <- x[2L] - start

  if(is.array(x))
    stop("Subsetting tensors with R matrixs or arrays is not currently supported")

  if (start < 0L || end < 0L ||
      any(suppressWarnings(x != seq.int(from = start, to = end, by = step))))
    stop("only positive sequences with a single step size are accepted\n",
         "if indexing with sequences not specified by a call to `:`,\n")

  if(abs(step) == 1L)
    step <- NULL

  py_slice(start, end, step, one_based)
}


translate_one_based_to_zero_based <- function(x) {
  stopifnot(is_scalar_integerish(x))
  if (x > 0L)
    x - 1L
  else if (x < 0L)
    x
  else
    stop("0 is not a valid slice spec if `one_based_extract` is TRUE")

}


# ----- py_* functions ----

# should this live in reticulate?
#' @export
py_ellipsis <- function()
  import_builtins(FALSE)$Ellipsis

# should this live in (and be exported by) reticulate?
py_slice <-
  function(start = NULL, stop = NULL, step = NULL,
           one_based = getOption("tensorflow.one_based_extract", TRUE),
           inclusive_stop = getOption("tensorflow.py_slice_stop_is_inclusive", TRUE)) {
    # python default for inclusive_stop is FALSE

    if(is_tensor(start) || is_tensor(stop) || is_tensor(step)) {
      # this is not an exported function, so to check if we should warn we only
      # need to inspect the global options, not the function args here
      maybe_warn_no_translation_performed()
      return(import_builtins()$slice(start, stop, step))
    }


  check_is_scalar_integerish_or_NULL(start, stop, step)
  check_is_TRUE_or_FALSE(one_based)

  start <- as_nullable_integer(start)
  stop  <- as_nullable_integer(stop)
  step  <- as_nullable_integer(step)

  if (one_based) {
    # need to offset positive integers, but not negative ones
    if(!is.null(start)) start <- translate_one_based_to_zero_based(start)
    if(!is.null(stop))  stop  <- translate_one_based_to_zero_based(stop)
  }

  # maybe invert sign of step if start:stop are selecting items in reverse
  if (isTRUE(start %||% NA > stop %||% NA) &&
      isTRUE(sign(start %||% NA) == sign(stop %||% NA)) &&
      sign(step %||% 1L) == 1)
    step <- (step %||% 1L) * -1L

  # python slice() is exclusive of stop, but R is typically inclusive of all
  # elements in a slicing sequence. Default to R behavior.
  if (isTRUE(inclusive_stop)) {
    if(!is.null(stop))
      stop <- stop + as.integer(sign(stop) * sign(step %||% 1L))
  }

  import_builtins()$slice(start, stop, step)
}



# ---- warn about python <--> R slicing pitfalls

stop_if_any_zeros <- function(dots) {

  has_zero <- rapply(dots, function(d) isTRUE(any(d == 0)),
    classes = c("numeric", "integer"), deflt = FALSE)

  if(any(has_zero))
    stop(paste(
      "It looks like you might be using 0-based indexing to extract using `[`.",
      "The tensorflow package now uses 1-based (R-like) extraction by default.\n",
      "You can switch to the old behavior (1-based extraction) with:",
      "  options(tensorflow.one_based_extract = FALSE)\n", sep = "\n"
    ), call. = FALSE)
}


warn_if_any_negative <- function(dots) {

  has_negative <- rapply(dots, function(d)
    is_scalar_integerish(d) && d < 0,
    classes = c("numeric", "integer"), deflt = FALSE)
  # only check scalars because longer sequences with any negative numbers throw
  # an error later

  if(any(has_negative)) {
    warning("negative numbers are interperted python-style when subsetting tensorflow tensors.\n",
            "(they select items by counting from the back). For more details, see:\n",
            "https://docs.scipy.org/doc/numpy-1.13.0/reference/arrays.indexing.html#basic-slicing-and-indexing\n",
            "To turn of this warning, set 'options(tensorflow.warn_negative_extract_is_python_style = FALSE)'")
    should_warn_about_negative_indices(FALSE) # warn only once
  }
}

maybe_warn_no_translation_performed <- function() {
  if (!isFALSE(getOption("tensorflow.warn_tensors_in_extract_passed_as_is")) &&
      (!isFALSE(getOption("tensorflow.one_based_extract")) ||
       !isFALSE(getOption( "tensorflow.py_slice_stop_is_inclusive")))
  )
  warning(
    "No index offsetting or translation is performed when indexing tensors\n",
    "with tensors. Indexing tensors are passed as-is to python. Specified options\n",
    "for one_based_extract and inclusive_stop are ignored and treated as FALSE\n",
    "to silence this warning, set\n",
    "option(tensorflow.warn_tensors_in_extract_passed_as_is = FALSE)")
}


# ----- check inputs ------------

check_vars <- function(..., .fun, .friendly_description) {
  symbol <- vapply(eval(substitute(alist(...))), deparse, "")
  val <- list(...)

  for(i in seq_along(val)) {
    if(!.fun(val[[i]]))
      stop(paste("In call:", deparse(sys.call(1L)), "\n",
        "Value supplied to", sQuote(symbol[i]), "must be",
                 .friendly_description),
           call. = FALSE)
  }
  invisible(TRUE)
}

check_is_TRUE_or_FALSE <- function(...) {
  check_vars(...,
             .fun = function(x)
               identical(x, TRUE) || identical(x, FALSE),
             .friendly_description = "TRUE or FALSE")
}


check_is_scalar_integerish_or_NULL <- function(...) {
  check_vars( ..., .fun = function(x) {
      is.null(x) || is_scalar_integerish(x)
    },
    .friendly_description = "a whole (integer-like) number or NULL"
  )
}


# ------ mini utils -------

dropNULL <- function(x) x[!vapply(x, is.null, FALSE)]

is_scalar <- function(x) identical(length(x), 1L)


is_scalar_integerish <- function(x) {
  is_scalar(x) && is.numeric(x) && !is.na(x) && is.finite(x) &&
    (is.integer(x) || x == as.integer(x))
}

is_integerish <- function(x) {
  is.numeric(x) &&
    all(x == suppressWarnings(as.integer(x)))
}

as_nullable_integer <- function (x) {
  if (is.null(x))
    x
  else
    as.integer(x)
}

is_tensor <- function(x) inherits(x, "tensorflow.tensor")

isFALSE <- function(x) identical(x, FALSE)
