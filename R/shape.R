#' Create a `tf.TensorShape` object
#'
#' @param ... Tensor dimensions as integers or `NULL` for an unknown
#'   dimensions. `NA` and `-1` are synonyms for `NULL`.
#' @param dims Tensor dimensions as a vector.
#'
#' @seealso <https://www.tensorflow.org/api_docs/python/tf/TensorShape>
#'
#' @export
#' @examples
#' \dontrun{
#'
#' # --- construct ---
#' shape()       # tf.TensorShape()       # scalar
#' shape(NULL)   # tf.TensorShape([None]) # 1-D array of unknown length
#' shape(NA)     # tf.TensorShape([None]) # 1-D array of unknown length, NA is a synonym for NULL
#'
#' shape(dims = NULL) # TensorShape(None)    # Unknown rank, unknown size
#' shape(3, 4)        # TensorShape([3, 4])  # 2-D array (matrix) with 3 rows, 4 columns
#' shape(NA, 4)           # TensorShape([None, 4])  # 2-D array (matrix) with unknown rows, 4 columns
#' shape(dims = c(NA, 4)) # TensorShape([None, 4]) # same as above; bypass ... and pass dims directly
#'
#' # --- inspect ---
#' length(shape(dims = NULL)) # NA_integer_
#' length(shape(1,2,3,NA))    # 4L
#'
#' # ---convert ---
#' x <- shape(dims = list(3L, 5L))
#' as.list(x)     # list(3L, 5L)
#' as.integer(x)  # c(3L, 5L)
#' as.numeric(x)  # c(3, 5)
#' as.double(x)   # c(3, 5) # alias for as.numeric
#' as_tensor(x)   # tf.Tensor([3 5], shape=(2,), dtype=int32)
#'
#' # convert partially undefined shapes
#' x <- shape(NA, 3)
#' as.list(x)     # list(NULL, 3L)
#' as.integer(x)  # c(NA, 3L)
#' as_tensor(x)   # tf.Tensor([-1  3], shape=(2,), dtype=int32) # unspecified dims default is -1
#'
#' # as_tensor() converts undefined dimensions to -1, which is useful for
#' # tf functions that only accept tensors for shapes, e.g,
#' tf$reshape(tf$zeros(shape(8)),
#'            as_tensor(shape(NA, 4)))
#' # tf.Tensor([[0. 0. 0. 0.]
#' #            [0. 0. 0. 0.]], shape=(2, 4), dtype=float32)
#'
#' # converting fully unknown shapes raises an error
#' try(as.list(shape(dims = NULL))) # ValueError: as_list() is not defined on an unknown TensorShape.
#' # test for rank first if this a concern:
#' as.list_or_null <- function(x) if(is.na(length(x))) NULL else as.list(x)
#' as.list_or_null(shape(dims = NULL))
#'
#'
#' # --- compare ---
#' # Fully known shapes return TRUE if and only if each element is equal
#' shape(3, 4) == shape(3, 4) # TRUE
#' shape(3, 4) == shape(4, 4) # FALSE
#'
#' # two unknown dimensions are treated as equal
#' shape(NA, 4) == shape(NA, 4) # TRUE
#' shape(NA, 4) == shape(3, 4)  # FALSE
#'
#' # Two unknown shapes, return TRUE
#' shape(dims = NULL) == shape(dims = NULL) # TRUE
#'
#' # Comparing an unknown shape to a partially or fully defined shape returns FALSE
#' shape(dims = NULL) == shape(NULL) # FALSE
#' shape(dims = NULL) == shape(4)    # FALSE
#'
#'
#' values of length greater than one supplied to `...`  are automatically flattened
#' shape(1, c(2, 3), 4) # shape(1, 2, 3, 4)
#' shape(1, shape(2, 3), 4) # shape(1, 2, 3, 4)
#' shape(1, as_tensor(2, 3), 4) # shape(1, 2, 3, 4)
#'
#' # --- extract or replace ---
#' # regular R-list semantics for `[`, `[[`, `[<-`, `[[<-`
#' x <- shape(1, 2, 3)
#' x[1]       # TensorShape([1])
#' x[[1]]     # 1L
#' x[2:3]     # TensorShape([2, 3])
#' x[-1]      # TensorShape([2, 3])
#'
#' x[1] <- 11        ; x # TensorShape([11, 2, 3])
#' x[1] <- shape(11) ; x # TensorShape([11, 2, 3])
#' x[1] <- list(11)  ; x # TensorShape([11, 2, 3])
#'
#' x[[1]] <- 22            ; x # TensorShape([22, 2, 3])
#' x[1:2] <- c(NA, 99)     ; x # TensorShape([None, 99, 3])
#' x[1:2] <- shape(33, 44) ; x # TensorShape([33, 44, 3])
#'
#' # --- concatenate ---
#' c(shape(1), shape(2, 3), shape(4, NA)) # TensorShape([1, 2, 3, 4, None])
#'
#' # --- merge ---
#' merge(shape(NA, 2),
#'       shape(1 , 2)) # TensorShape([1, 2])
#'
#' try(merge(shape(2, 2),
#'           shape(1, 2))) # ValueError: Shapes (2, 2) and (1, 2) are not compatible
#'
#' rm(x) # cleanup
#' }
shape <- function(..., dims = list(...)) {
  if (is.null(dims))
    return(tf$TensorShape(NULL))

  if(inherits(dims, "tensorflow.tensor") && tf$executing_eagerly())
    dims <- as_r_value(dims$numpy())

  names(dims) <- NULL
  dims <- lapply(dims, function(d) {
    d <- as_r_value(d)
    if (is.null(d) ||
        is.atomic(d) && isTRUE(is.na(d)) ||
        (is.numeric(d) && isTRUE(d == -1L)))
      list(NULL)
    else
      as.integer(d)
  })
  if(length(dims))
    dims <- unlist(dims, recursive = FALSE, use.names = FALSE)

  tf$TensorShape(dims)
}


as_shape <- function(x) {
  if(inherits(x, "tensorflow.python.framework.tensor_shape.TensorShape"))
    x
  else
    shape(dims = x)
}

as_r_value <- function (x) {
  if (inherits(x, "python.builtin.object"))
    py_to_r(x)
  else
    x
}



#' @export
as.list.tensorflow.python.framework.tensor_shape.TensorShape  <- function(x, ...) {
  as.list(as_r_value(x$as_list())) # raises an exception for unknown rank
}

#' @export
#' @method as.integer tensorflow.python.framework.tensor_shape.TensorShape
as.integer.tensorflow.python.framework.tensor_shape.TensorShape <- function(x, ...) {
  vapply(as.list(as_r_value(x$as_list())),
         function(e) e %||% NA_integer_,
         0L)
}

#' @export
#' @method as.numeric tensorflow.python.framework.tensor_shape.TensorShape
as.numeric.tensorflow.python.framework.tensor_shape.TensorShape <- function(x, ...)
  as.numeric(as.integer.tensorflow.python.framework.tensor_shape.TensorShape(x), ...)

#' @export
#' @method as.double tensorflow.python.framework.tensor_shape.TensorShape
as.double.tensorflow.python.framework.tensor_shape.TensorShape <-
as.numeric.tensorflow.python.framework.tensor_shape.TensorShape

#' @export
as_tensor.tensorflow.python.framework.tensor_shape.TensorShape <-
function(x, dtype = NULL, ..., name = NULL) {
  if(x$is_fully_defined())
    return(NextMethod())

  x <- as.integer.tensorflow.python.framework.tensor_shape.TensorShape(x)
  x[is.na(x)] <- -1L
  as_tensor.default(x, dtype, ..., name = name)
}


#' @export
`[.tensorflow.python.framework.tensor_shape.TensorShape` <- function(x, i) {
  x <- as.list.tensorflow.python.framework.tensor_shape.TensorShape(x)
  as_shape(x[i])
}

#' @export
`[[.tensorflow.python.framework.tensor_shape.TensorShape` <- function(x, i) {
  x <- as.list.tensorflow.python.framework.tensor_shape.TensorShape(x)
  x[[i]]
}

#' @export
`[<-.tensorflow.python.framework.tensor_shape.TensorShape` <- function(x, ..., value) {
  x <- as.list.tensorflow.python.framework.tensor_shape.TensorShape(x)
  x[...] <- as.list(value)
  shape(dims = x)
}

#' @export
`[[<-.tensorflow.python.framework.tensor_shape.TensorShape` <- function(x, ..., value) {
  x <- as.list.tensorflow.python.framework.tensor_shape.TensorShape(x)
  x[[...]] <- value
  shape(dims = x)
}


#' @export
`c.tensorflow.python.framework.tensor_shape.TensorShape` <- function(...) {
  x <- ..1
  for(other in list(...)[-1])
    x <- x$concatenate(as_shape(other))
  x
}

# `c.tensorflow.python.framework.tensor_shape.TensorShape` <- function(...) {
#   x <- do.call(c, lapply(unname(list(...)), as.list))
#   shape(dims = x)
# }


#' @export
merge.tensorflow.python.framework.tensor_shape.TensorShape <- function(x, y, ...)
  x$merge_with(as_shape(y))


#' @export
length.tensorflow.python.framework.tensor_shape.TensorShape <- function(x) {
  # x$rank          returns NULL on tensor of unknown rank
  # x$`__len__`())  raises ValueError on tensor of unknown rank
  # dim(tensor) returns NULL on tensor of unknown rank (for reference)
  as_r_value(x$rank) %||% NA_integer_
}

#' @export
format.tensorflow.python.framework.tensor_shape.TensorShape <-
  function(x, ...) {
    if (identical(as_r_value(x$rank), NULL))
      "(<unknown>)"
    else
      sprintf("(%s)", paste0(as.integer(x), collapse = ", "))
  }

#' @export
print.tensorflow.python.framework.tensor_shape.TensorShape <-
  function(x, ...) {
    writeLines(import_builtins()$repr(x))
    invisible(x)
  }


#' @export
py_str.tensorflow.python.framework.tensor_shape.TensorShape <-
  function(object, ...) as_r_value(object$`__repr__`())

## reticulate already dispatches to __eq__, but we need to do
## additional coercion on a and b
#' @export
`==.tensorflow.python.framework.tensor_shape.TensorShape` <- function(a, b) {
  a <- as_shape(a)
  b <- as_shape(b)
  as_r_value(a$`__eq__`(b))
}

# != is not defined as the negation of == in python, tricky!
#' @export
`!=.tensorflow.python.framework.tensor_shape.TensorShape` <- function(a, b) {
  a <- as_shape(a)
  b <- as_shape(b)
  as_r_value(a$`__ne__`(b))
}




# old shape def, retained in namespace in case it's needed for easy back compat
shape_v1 <- function(...) {
  values <- list(...)
  lapply(values, function(value) {
    if (!is.null(value))
      as.integer(value)
    else
      NULL
  })
}
