

# ~~~~~~
# tensor extract syntax with basis 0 or 1
extract_manual <- function (x, i, j, ..., drop = TRUE, basis = 0) {

  # tensor shape as a vector
  x_size <- x$get_shape()$as_list()

  # if it has dimension(s) of undefined size, this will be a list with NULLs
  if (is.list(x_size)) {
    x_size <- lapply(x_size, function (x) ifelse(is.null(x), NA, x))
    x_size <- unlist(x_size)
  }

  n_indices <- length(x_size)

  # Capture all indices beyond i and j (skip function, `x`, `drop`, `i` & `j`
  # from the arguments). This enables users to skip indices to get their defaults
  cl <- match.call()
  args <- as.list(cl)[-1]
  known_args <- c("x", "i", "j", "drop", "basis")
  extra_indices <- args[!names(args) %in% known_args]

  # if i wasn't specified, make it NA (keep all values)
  if (missing(i))
    i <- list(NA)
  else
    i <- list(validate_index(i))

  # if j wasn't specified, but is required, keep all elements
  # if it isn't required, skip it
  if (missing(j)) {
    if (n_indices > 1) j <- list(NA)
    else j <- list()
  } else {
    j <- list(validate_index(j))
  }

  # evaluate and fill in blanks
  extra_indices <- lapply(extra_indices, evaluate_index)

  # combine the indices & strip out any names
  indices <- c(i, j, extra_indices)
  names(indices) <- NULL

  # error if wrong number of indices
  if (length(indices) !=  n_indices) {
    stop ('incorrect number of dimensions')
  }

  # find index starting element on each dimension
  begin <- vapply(indices,
                  function (x) {
                    if (length(x) == 1 && is.na(x))
                      0
                    else
                      x[1] - basis
                  },
                  FUN.VALUE = 0)

  # find slice end in each dimension
  end <- vapply(indices,
                function (x) {
                  if (length(x) == 1 && is.na(x))
                    Inf
                  else
                    x[length(x)] - basis
                },
                FUN.VALUE = 0)

  # truncate missing indices to be finite & add one to the ends to account for
  # Python's exclusive upper bound
  end <- pmin(end, x_size) + 1

  # convert to shapes
  begin_shape <- do.call('shape', as.list(begin))
  end_shape <- do.call('shape', as.list(end))

  # add stride length (always 1) so that the output is consistent with python API
  stride_shape <- as.list(rep(1L, n_indices))

  # get shrink mask as an integer representing a bitstring
  # if drop=TRUE, drop all *indices* specified as integers,
  # i.e. for a 2x3 Tensor x:
  #   x[1:1, ,drop=TRUE] => shape 1x3
  #   x[1, ,drop=TRUE] => shape 3
  if (drop) {
    # create bit mask as a logical vector, then collapse to an integer
    shrink <- vapply(indices,
                     function (x) {
                       length(x) == 1 && !is.na(x)
                     },
                     FALSE)
    shrink_integer <- integer_mask(shrink)
  } else {
    shrink_integer <- 0
  }

  # if there were dimensions with undefined sizes, mask them from subsetting
  begin_mask <- end_mask <- integer_mask(is.na(x_size))

  # return the slice
  tf$strided_slice(input_ = x,
                   begin = begin_shape,
                   end = end_shape,
                   strides = stride_shape,
                   begin_mask = begin_mask,
                   end_mask = end_mask,
                   shrink_axis_mask = shrink_integer)
}

# check for blank spaces in the call
is.blank <- function (x) is.name(x) && as.character(x) == ''

integer_mask <- function (mask) sum(2 ^ (seq_along(mask) - 1)[mask])

# evaluate any calls (in the environment calling `[`) and replace any
# skipped indices (blank names) with NAs
evaluate_index <- function (x, validate = TRUE, n = 3) {

  if (is.blank(x)) {

    x <- NA

  } else {

    if (is.call(x) | is.name(x))
      x <- eval(x, envir = parent.frame(n = n))

    if (validate)
      x <- validate_index(x)

  }

  x

}

# check the user-specified index is valid
validate_index <- function (x, base = 0) {

  append <- switch (as.character(base),
                    `0` = " with 0-based indexing",
                    `1` = " with unknown dimensions")

  if (!(is.numeric(x) && is.finite(x))) {
    stop ("invalid index - must be numeric and finite",
          append)
  }

  if (!(is.vector(x))) {
    stop ("only vector indexing of Tensors is currently supported",
          append)
  }

  if (any(x < 0)) {
    stop ("negative indexing of Tensors is not currently supported",
          append)
  }

  if (x[length(x)] < x[1]) {
    stop ("decreasing indexing of Tensors is not currently supported",
          append)
  }

  x

}


# check for zero-based extraction indexing and warn, unless r-like indexing is
# explicitly on
check_zero_based <- function (call) {

  # get indices from the call
  args <- as.list(call)[-1]
  other_args <- c("x", "drop", "basis")
  names <- names(args)
  indices <- args[!names %in% other_args]

  # evaluate these in the calling environment and look for 0s in them
  indices <- lapply(indices,
                    evaluate_index,
                    validate = FALSE,
                    n = 4)

  contain_zero <- vapply(indices,
                         function (x) {
                           (is.atomic(x) | is.list(x)) && any(is_a_zero(x))
                         },
                         FUN.VALUE = FALSE)

  default_one_based <- is.null(getOption("tensorflow.one_based_extract"))

  if (any(contain_zero) & default_one_based) {
    warning(paste(
      "It looks like you might be using 0-based indexing to extract using `[`.",
      "The tensorflow package now uses 1-based (R-like) extraction by default.\n",
      "You can switch to the old behavior (1-based extraction) with:",
      "  options(tensorflow.one_based_extract = FALSE)\n",
      "If your indexing is as you intend, you can disable this warning with:",
      "  options(tensorflow.one_based_extract = TRUE)", sep = "\n"
    ), call. = FALSE)
  }

}

# does each element of this vector contain a 0
is_a_zero <- function (x) {
  vapply(x,
         function (x) {
           identical(x, 0) | identical(x, 0L)
         },
         FUN.VALUE = FALSE)
}
