
#' Subset tensors with `[`
#'
#' @param x Tensorflow tensor
#' @param ... slicing specs. See examples and details.
#' @param drop whether to drop scalar dimensions
#' @param style One of `"python"` or `"R"`.
#' @param options An object returned by `tf_extract_opts()`
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' x <- as_tensor(array(1:15, dim = c(3, 5)))
#' x
#' # by default, numerics supplied to [...] are interpreted R style
#' x[,1]    # first column
#' x[1:2,]  # first two rows
#' x[,1, drop = FALSE] # 1 column matrix
#'
#' # strided steps can be specified in R syntax or python syntax
#' x[, seq(1, 5, by = 2)]
#' x[, 1:5:2]
#' # if you are unfamiliar with python-style strided steps, see:
#' # https://numpy.org/doc/stable/reference/arrays.indexing.html#basic-slicing-and-indexing
#'
#' # missing arguments for python syntax are valid, but they must by backticked
#' # or supplied as NULL
#' x[, `::2`]
#' x[, NULL:NULL:2]
#' x[, `2:`]
#'
#'
#' # all_dims() expands to the shape of the tensor
#' # (equivalent to a python ellipsis `...`)
#' # (not to be confused with R dots `...`)
#' y <- as_tensor(array(1:(3^5), dim = c(3,3,3,3,3)))
#' all.equal(y[all_dims(), 1],
#'           y[, , , , 1])
#'
#' # tf$newaxis are valid (equivalent to a NULL)
#' x[,, tf$newaxis]
#' x[,, NULL]
#'
#'
#' # negative numbers are always interpreted python style
#' # The first time a negative number is supplied to `[`, a warning is issued
#' # about the non-standard behavior.
#' x[-1,]  # last row, with a warning
#' x[-1,]  # the warning is only issued once
#'
#' # specifying `style = 'python'` changes the following:
#' # +  zero-based indexing is used
#' # +  slice sequences in the form of `start:stop` do not include `stop`
#' #    in the returned value
#' # +  out-of-bounds indices in a slice are valid
#'
#' # The style argument can be supplied to individual calls of `[` or set
#' # as a global option
#'
#' # example of zero based  indexing
#' x[0, , style = 'python']  # first row
#' x[1, , style = 'python']  # second row
#'
#' # example of slices with exclusive stop
#' options(tensorflow.extract.style = 'python')
#' x[, 0:1]  # just the first column
#' x[, 0:2]  # first and second column
#'
#' # example of out-of-bounds index
#' x[, 0:10]
#' options(tensorflow.extract.style = NULL)
#'
#' # slicing with tensors is valid too, but note, tensors are never
#' # translated and are always interpreted python-style.
#' # A warning is issued the first time a tensor is passed to `[`
#' x[, tf$constant(0L):tf$constant(2L)]
#' # just as in python, only scalar tensors are valid
#' # https://www.tensorflow.org/api_docs/python/tf/Tensor#__getitem__
#'
#' # To silence the warnings about tensors being passed as-is and negative numbers
#' # being interpreted python-style, set
#' options(tensorflow.extract.style = 'R')
#'
#' # clean up from examples
#' options(tensorflow.extract.style = NULL)
#' }
`[.tensorflow.tensor` <-
  function(x, ..., drop = TRUE,
           style = getOption("tensorflow.extract.style"),
           options = tf_extract_opts(style)) {

    stopifnot(inherits(options, "tf_extract_options"))
    check_is_TRUE_or_FALSE(drop)
    options$drop <- drop

    dots <- maybe_reparse_as_slice_spec_then_force(..., .env = parent.frame())

    if(is_scalar(dots) && length(dim(dots[[1]])) > 1L) # indexing w/ array
       stop("Subsetting tensors with R matrices or arrays is not currently supported")

    if (length(dropNULL(dots)) != py_len(x$shape)) {  # NULL == tf$newaxis
      is_ellipsis <- vapply(dots, is_py_ellipsis, FALSE)
      if(length(dropNULL(dots[!is_ellipsis])) > py_len(x$shape))
        stop( "Incorrect number of dimensions supplied. ", py_len(x$shape),
          " required, but received ", length(dropNULL(dots)))
      else if (!any(is_ellipsis)) warning(
        "Incorrect number of dimensions supplied. The number of supplied arguments, ",
        "(not counting any NULL, tf$newaxis or np$newaxis) must match the",
        "number of dimensions in the tensor, unless an all_dims() was supplied",
        " (this will produce an error in the future)")
    }

    if(options$one_based)
      stop_if_any_zeros(dots)

    if (options$disallow_out_of_bounds)
      stop_if_any_out_of_bounds(x, dots, options)

    if (options$warn_negatives_pythonic)
      warn_if_any_negative(dots)

    if (options$warn_tensors_passed_asis)
      warn_if_any_tensors(dots)

    dots <- lapply(dots, as_valid_py__getitem__arg, options)

    # mirror the python parser, where single argumets in [] are passed
    # as-supplied, otherwise they are wrapped in a tuple
    dots <-  if (is_scalar(dots)) dots[[1L]] else tuple(dots)

    x$`__getitem__`(dots)
  }




#' Tensor extract options
#'
#' @param style one of `NULL` (the default) `"R"` or `"python"`. If supplied,
#'   this overrides all other options. `"python"` is equivalent to all the other
#'   arguments being `FALSE`. `"R"` is equivalent to
#'   `warn_tensors_passed_asis` and `warn_negatives_pythonic`
#'   set to `FALSE`
#' @param ... ignored
#' @param one_based TRUE or FALSE, if one-based indexing should be used
#' @param inclusive_stop TRUE or FALSE, if slices like `start:stop` should be
#'   inclusive of `stop`
#' @param disallow_out_of_bounds TRUE or FALSE, whether checks are performed on
#'   the slicing index to ensure it is within bounds.
#' @param warn_tensors_passed_asis TRUE or FALSE, whether to emit a warning the
#'   first time a tensor is supplied to `[` that tensors are passed as-is, with
#'   no R to python translation
#' @param warn_negatives_pythonic TRUE or FALSE, whether to emit
#'   a warning the first time a negative number is supplied to `[` about the
#'   non-standard (python-style) interpretation
#'
#' @return an object with class "tf_extract_opts", suitable for passing to
#'   `[.tensorflow.tensor()`
#' @export
#'
#' @examples
#' \dontrun{
#' x <- tf$constant(1:10)
#'
#' opts <-  tf_extract_opts("R")
#' x[1, options = opts]
#'
#' # or for more fine-grained control
#' opts <- tf_extract_opts(
#'     one_based = FALSE,
#'     warn_tensors_passed_asis = FALSE,
#'     warn_negatives_pythonic = FALSE
#' )
#' x[0:2, options = opts]
#' }
tf_extract_opts <- function(
  style = getOption("tensorflow.extract.style"),
  ...,
  one_based =
    getOption("tensorflow.extract.one_based", TRUE),
  inclusive_stop =
    getOption("tensorflow.extract.inclusive_stop", TRUE),
  disallow_out_of_bounds =
    getOption("tensorflow.extract.dissallow_out_of_bounds", TRUE),
  warn_tensors_passed_asis =
    getOption("tensorflow.extract.warn_tensors_passed_asis", TRUE),
  warn_negatives_pythonic =
    getOption("tensorflow.extract.warn_negatives_pythonic", TRUE)
) {

  if(length(list(...)))
    warning("arguments passed to `...` are ignored")

  check_is_TRUE_or_FALSE(
    one_based,
    inclusive_stop,
    disallow_out_of_bounds,
    warn_tensors_passed_asis,
    warn_negatives_pythonic
  )

  # backwards compatability, one_based_extract -> renamed to extract.one_based
  if (!is.null(getOption("tensorflow.one_based_extract"))) {
    check_is_TRUE_or_FALSE(getOption("tensorflow.one_based_extract"))
    if (missing(one_based) || is.null(getOption("tensorflow.extract.one_based"))) {
      one_based <- getOption("tensorflow.one_based_extract")
      # warning("option tensorflow.one_based_extract ",
      #         "renamed to tensorflow.extract.one_based")
    } else
      warning("options `tensorflow.one_based_extract` is ignored and",
              "overridden by option `tensorflow.extract.one_based")
  }

  opts <- nlist(
    one_based,
    inclusive_stop,
    disallow_out_of_bounds,
    warn_tensors_passed_asis,
    warn_negatives_pythonic
  )

  if (!is.null(style)) {
    style <- match.arg(style, choices = c("R", "python"))
    if (style == "python")
      opts <- lapply(opts, function(x) FALSE)
    else {
      opts$warn_tensors_passed_asis <- FALSE
      opts$warn_negatives_pythonic <- FALSE
    }

  } else {

    if (warned_about$negative_indices)
      opts$warn_negatives_pythonic <- FALSE

    if (warned_about$tensors_passed_asis)
      opts$warn_tensors_passed_asis <- FALSE

  }

  class(opts) <- "tf_extract_options"
  opts
}


#' All dims
#'
#' This function returns an object that can be used when subsetting tensors with
#' `[`. If you are familiar with python,, this is equivalent to the python Ellipsis
#' `...`, (not to be confused with `...` in `R`).
#'
#' @export
#' @examples
#' \dontrun{
#' # in python, if x is a numpy array or tensorflow tensor
#' x[..., i]
#' # the ellipsis means "expand to match number of dimension of x".
#' # to translate the above python expression to R, write:
#' x[all_dims(), i]
#' }
all_dims <- function()
  .globals$builtin_ellipsis %||%
  (.globals$builtin_ellipsis <- import_builtins(FALSE)$Ellipsis)


builtin_slice <- function(...) {
  slice <- .globals$py_slice
  if(is.null(slice))
    .globals$py_slice <- slice <- import_builtins(FALSE)$slice
  slice(...)
}


py_slice <-
  function(start = NULL, stop = NULL, step = NULL,
           one_based = tf_extract_opts()$one_based,
           inclusive_stop = tf_extract_opts()$inclusive_stop) {

    # early return for most common case
    if(is.null(start) && is.null(stop) && is.null(step))
      return(builtin_slice(NULL))

    NA_to_NULL <- function(x) {
      if (identical(x, NA) ||
          identical(x, NA_integer_) ||
          identical(x, NA_real_))
        NULL
      else
        x
    }

    start <- NA_to_NULL(start)
    stop <- NA_to_NULL(stop)
    step <- NA_to_NULL(step)

    check_vars(start, stop, step,
               .fun = function(x)
                 is.null(x) || is_scalar_integerish(x) || is_tensor(x),
               .friendly_description =
                 "NULL or NA (the default if missing), a scalar whole number, or a tensor"
    )
    check_is_TRUE_or_FALSE(one_based, inclusive_stop)

    if (is.numeric(start)) start <- as.integer(start)
    if (is.numeric(stop)) stop <- as.integer(stop)
    if (is.numeric(step)) step <- as.integer(step)

    if (one_based) {
      # need to offset positive integers, but not negative ones
      if(is.numeric(start)) start <- translate_one_based_to_zero_based(start)
      if(is.numeric(stop))  stop  <- translate_one_based_to_zero_based(stop)
    }

    # maybe invert sign of step if start:stop are selecting items in reverse
    # (python doesn't do this, but we do because it's a nice convenience)
    if (is.numeric(start) && is.numeric(stop) && !is_tensor(step) &&
        start > stop && sign(start) == sign(stop) && sign(step %||% 1) == 1)
      step <- (step %||% 1L) * -1L

    # python slice() is exclusive of stop, but R is typically inclusive of all
    # elements in a slicing sequence. Default to R behavior.
    if (inclusive_stop) {
      if (is.numeric(stop)) {
        stop <- stop + as.integer(sign(step %||% 1L))
        if (stop == 0 && sign(step %||% 1) == 1)
          stop <- NULL
      }
    }

    builtin_slice(start, stop, step)
  }


maybe_reparse_as_slice_spec_then_force <- function(..., .env) {
  dots <- eval(substitute(alist(...)))

  if (!is.null(names(dots))) {
    warning("names are ignored: ", paste(names(dots), collapse = ", "),
            call. = FALSE)
    names(dots) <- NULL
  }

  lapply(dots, function(d) {

    if(is_missing(d))
      return(py_slice())

    if (is_has_colon(d)) {

      if (is_colon_call(d)) {

        d <- as.list(d)[-1]

        if (is_colon_call(d[[1]] -> d1)) # step supplied
          d <- c(as.list(d1)[-1], d[-1])

      } else { # single name with colon , like `::2`

        d <- deparse(d, width.cutoff = 500, backtick = FALSE)
        d <- strsplit(d, ":", fixed = TRUE)[[1]]
        d[!nzchar(d)] <- "NULL"
        d <- lapply(d, parse1)
      }

      if(!length(d) %in% 1:3)
        stop("Only 1, 2, or 3 arguments can be supplied as a python-style slice")

      d <- lapply(d, eval, envir = .env)

      if(length(d) < 3L)
        d <- c(d, rep_len(list(NULL), 3L - length(d)))

      class(d) <- "py_slice_spec"
      return(d)
    }

    # else, eval normally
    eval(d, envir = .env)
  })
}


as_valid_py__getitem__arg <- function(x, options) {
  if (inherits(x, "py_slice_spec")) {
    opts <- options[c("one_based", "inclusive_stop")]
    return(do.call(py_slice, c(x, opts)))
  }

  if(inherits(x, c("python.builtin.slice",
                   "python.builtin.ellipsis")))
    return(x)

  if(is.atomic(x) && (is.character(x) || anyNA(x) || any(is.infinite(x))))
    stop("NA, Inf, or character inputs not supported")

  if(is_scalar_integerish(x)) {
    x <- as.integer(x)
    if(options$one_based)
      x <- translate_one_based_to_zero_based(x)

    if(!options$drop) {
      # tf.__getitem__ drops dim on scalar indexes, but not on slices. Here we
      # convert the scalar index to an equivalent py_slice to preserve the dim
      if(x < 0)
        x <- py_slice(x, x-1L, -1L, one_based = FALSE, inclusive_stop = FALSE)
      else
        x <- py_slice(x, x+1L, one_based = FALSE, inclusive_stop = FALSE)
    }
    return(x)
  }

  if (is_integerish(x)) # a numeric sequence of length greater than 1
    return(R_slicing_sequence_to_py_slice(x, options)) # error checking within

  # else, just pass the input and python will throw an error if it's invalid
  x
}


R_slicing_sequence_to_py_slice <- function(x, options) {
  # x is assumed to be integerish

  start <- x[1L]
  end <- x[length(x)]
  step <- x[2L] - start

  if (start < 0L || end < 0L ||
      any(suppressWarnings(x != seq.int(from = start, to = end, by = step))))
    stop("only positive sequences with a single step size are accepted ",
         "if indexing with sequences not specified by a call to `:`", call. = TRUE)

  if(identical(step, 1L))
    step <- NULL

  py_slice(start, end, step,
           one_based = options$one_based,
           inclusive_stop = options$inclusive_stop)
}


translate_one_based_to_zero_based <- function(x) {
  stopifnot(is_scalar_integerish(x))
  if (x > 0L)
    x - 1L
  else if (x < 0L)
    x
  else
    stop("cannot use 0 in slice syntax if `one_based` is TRUE")
}


stop_if_any_out_of_bounds <- function(x, dots, options) {
  dims <- x$shape$as_list()
  dots <- dropNULL(dots) # possible new dims specified

  is_elip <- vapply(dots, is_py_ellipsis, FALSE)

  if(length(dots) < length(dims)) {
    # make implicit py_ellipsis explicit
    if(!any(is_elip)) {
      dots <- c(dots, all_dims())
      is_elip <- c(is_elip, TRUE)
    }
  }

  if (any(is_elip)) {
    if (sum(is_elip) > 1)
      stop("Only one all_dims() allowed if `dissallow_out_of_bounds` is TRUE")

    elip <- which(is_elip)

    n_missing <- length(dims) - (length(dots) - length(elip))
    # expand py_ellipsis to a list of missing language exprs
    # (actual user-supplied missing args were replaced with py_slice()'s earlier)
    dots <- c(dots[seq_len(elip - 1)],
              rep_len(list(quote(expr =)), n_missing),
              dots[-seq_len(elip)])
  }

  out_of_bounds <- mapply(function(arg, dim) {
    if (is_missing(arg) || is.null(dim)) # unknown dim size
      return(FALSE)

    stopifnot(is_scalar_integerish(dim))

    if (inherits(arg, "py_slice_spec")) {
      arg[[3]] <- NULL # ignore step
      arg <- unlist(arg[vapply(arg, is.numeric, TRUE)]) # drop tensors, NULLs
    }

    if (!is_integerish(arg) || !length(arg))
      return(FALSE) # no check for tensors

    max_idx <- pmax(max(arg) - options$one_based, min(arg) * -1)
    max_idx > dim
  }, arg = dots, dim = dims)

  if(any(out_of_bounds))
    stop(paste(
      "Arg supplied to index dimension", which(out_of_bounds),
      "is out bounds. Please supply a different value, or alternatively, set",
      "options(tensorflow.extract.disallow_out_of_bounds = FALSE)"))
}


stop_if_any_zeros <- function(dots) {
  recursivly_check_dots( dots,
    function(d) isTRUE(any(d == 0)),
    if_any_TRUE = stop( paste(
        "It looks like you might be using 0-based indexing to extract using `[`.",
        "The tensorflow package now uses 1-based extraction by default.\n",
        "You can switch to the old behavior (0-based extraction) with:",
        "  options(tensorflow.extract.one_based = FALSE)\n", sep = "\n" ),
      call. = TRUE
      ))
}


warned_about <- new.env(parent = emptyenv())
warned_about$negative_indices <- FALSE
warned_about$tensors_passed_asis <- FALSE


warn_if_any_negative <- function(dots) {
  recursivly_check_dots(dots,
    check_fun = function(d) is_scalar_integerish(d) && d < 0,
    ignore_py_slice_step = TRUE,
    if_any_TRUE = {
      warning( call. = FALSE,
        "Negative numbers are interpreted python-style when subsetting tensorflow tensors." ,
        "\nSee: ?`[.tensorflow.tensor` for details.",
        "\nTo turn off this warning, set",
        " `options(tensorflow.extract.warn_negatives_pythonic = FALSE)`")
      warned_about$negative_indices <- TRUE
    })
}

warn_if_any_tensors <- function(dots) {
  recursivly_check_dots(dots,
    function(x) TRUE, classes = "tensorflow.tensor",
    if_any_TRUE = {
      warning(call. = TRUE,
        "Indexing tensors are passed as-is to python, no index offsetting or ",
        "R to python translation is performed. Selected options for one_based ",
        "and inclusive_stop are ignored and treated as FALSE. To silence this warning, set ",
        "options(tensorflow.extract.warn_tensors_passed_asis = FALSE)")
      warned_about$tensors_passed_asis <- TRUE
    })
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
           call. = TRUE)
  }
  invisible(TRUE)
}


check_is_TRUE_or_FALSE <-
  function(..., .friendly_description = "TRUE or FALSE") {
    check_vars( ..., .fun = function(x) isTRUE(x) || isFALSE(x),
      .friendly_description = .friendly_description)
  }


# ------ mini-utils -------

dropNULL <- function(x) x[!vapply(x, is.null, FALSE)]

is_scalar <- function(x) identical(length(x), 1L)

is_scalar_integerish <- function(x) {
  is_scalar(x) && is.numeric(x) && !is.na(x) && is.finite(x) &&
    (is.integer(x) || x == as.integer(x))
}

is_integerish <- function(x) {
  is.numeric(x) &&
    isTRUE(all(x == suppressWarnings(as.integer(x))))
}

as_nullable_integer <- function (x) {
  if (is.null(x))
    x
  else
    as.integer(x)
}

is_tensor <- function(x) inherits(x, "tensorflow.tensor")

isFALSE <- function(x) identical(x, FALSE)

nlist <- function(...) {
  nms <- vapply(eval(substitute(alist(...))), deparse, "")
  vals <- list(...)
  names(vals) <- nms
  vals
}

recursivly_check_dots <- function(dots, check_fun, if_any_TRUE,
                                  classes = c("numeric", "integer"),
                                  ignore_py_slice_step = FALSE) {
  if(ignore_py_slice_step)
    dots <- lapply(dots, function(x)
        if (inherits(x, "py_slice_spec")) x[1:2] else x)

  results <- rapply(dots, check_fun, classes, deflt = FALSE)
  if(any(results))
    force(if_any_TRUE)
}

is_py_ellipsis <- function(x) inherits(x, "python.builtin.ellipsis")

is_missing <- function(x) identical(x, quote(expr=))

parse1 <- function(text) parse(text = text, keep.source = FALSE)[[1]]

is_colon_call <- function(x)
  is.call(x) && identical(x[[1L]], quote(`:`))

is_has_colon <- function(x) {
  is_colon_call(x) || (is.name(x) && grepl(":", as.character(x), fixed = TRUE))
}


#' @export
`[<-.tensorflow.tensor` <- function(x, ..., value) {
  # This method exists solely to give a more meaningful error message
  # If the user attempts to assign a tensor
  # without this method: Error in x[1, ] <- 0 : object of type 'environment' is not subsettable
  # with this method: TypeError: 'tensorflow.python.framework.ops.EagerTensor' object does not support item assignment

  dots <- eval(substitute(alist(...)))
  here <- environment()
  key <- lapply(seq_along(dots), function(i) {
    if (is_missing(dots[[i]]))
      builtin_slice(NULL)
    else
      eval(sprintf("..%i", i), here)
  })

  if (is_scalar(length(key)))
    key <- key[[1L]]
  else if (identical(length(key), 0L))
    key <- builtin_slice(NULL)

  cl <- sys.call()
  tryCatch({
    py_set_item(x, key, value) # this will raise an error

  }, error = function(e) {
    e$call <- cl
    stop(e)
  })
  x
}
