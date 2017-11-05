# both options for tensor extraction, mimicing R's behaviour, or using 0-based
# indexing like python, but without python's R-incompatible syntax (e.g.
# strides)

# tensor extraction mimicing R
extract_r_like <- function(x, call) {

  dims_in <- dim(x)

  # create a dummy array containing the order of elements Python-style
  dummy_in <- dummy(dims_in)

  # modify the call and environment to swap dummy_in for the target, and use
  # primitive subsetting
  call_list <- as.list(call)[-1]
  env <- as.list(parent.frame(n = 2))
  env[[call_list[[1]]]] <- dummy_in
  dummy_out <- with(env, do.call(.Primitive("["), call_list))

  # coerce result to an array
  dummy_out <- as.array(dummy_out)

  # get number of elements in input and dimension of output
  nelem <- prod(dims_in)
  dims_out <- dim(dummy_out)
  dims_out <- do.call(shape, as.list(dims_out))

  # get the index in flat python format, as a tensor
  index <- flatten_rowwise(dummy_out)

  # flatten tensor, gather using index, reshape to output dimension
  tensor_in_flat <- tf$reshape(x, shape(nelem))
  tf_index <- tf$constant(as.integer(index), dtype = tf$int32)
  tensor_out_flat <- tf$gather(tensor_in_flat, tf_index)
  tensor_out <- tf$reshape(tensor_out_flat, dims_out)

  tensor_out

}

# convert an array to a vector row-wise
flatten_rowwise <- function (array) {
  dim <- dim(array)
  array <- aperm(array, rev(seq_along(dim)))
  dim(array) <- NULL
  array
}

# convert an vector to an array row-wise
unflatten_rowwise <- function (array, dim) {
  array <- as.array(array)
  dim(array) <- rev(dim)
  array <- aperm(array, rev(seq_along(dim)))
  dim(array) <- dim
  array
}

# create an array with the same dimensions as tensor and fill it with
# consecutive increasing integers in python order
dummy <- function (dims) {
  vec <- seq_len(prod(dims)) - 1
  unflatten_rowwise(vec, dims)
}

# ~~~~~~

# tensor extract syntax with zero-based indexing
extract_zero_based <- function (x, i, j, ..., drop = TRUE) {

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
  extra_indices <- args[!names(args) %in% c('x', 'i', 'j', 'drop')]

  # if i wasn't specified, make it NA (keep all values)
  if (missing(i)) i <- list(NA)
  else i <- list(validate_index(i))

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
                    if (length(x) == 1 && is.na(x)) 0
                    else x[1]
                  },
                  0)

  # find slice end in each dimension
  end <- vapply(indices,
                function (x) {
                  if (length(x) == 1 && is.na(x)) Inf
                  else x[length(x)]
                },
                0)

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
evaluate_index <- function (x) {
  if (is.blank(x))
    NA
  else if (is.call(x) | is.name(x))
    validate_index(eval(x, envir = parent.frame(n = 3)))
  else
    validate_index(x)
}

# check the user-specified index is valid
validate_index <- function (x) {
  if (!(is.numeric(x) && is.finite(x))) {
    stop ('invalid index - must be numeric and finite')
  }
  if (!(is.vector(x))) {
    stop ('only vector indexing of Tensors is currently supported')
  }
  if (any(x < 0)) {
    stop ('negative indexing of Tensors is not currently supported')
  }
  if (x[length(x)] < x[1]) {
    stop ('decreasing indexing of Tensors is not currently supported')
  }
  x
}
