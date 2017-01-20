ensure_not_na <- function(object) {
  if (any(is.na(object))) {
    stopf(
      "'%s' %s",
      deparse(substitute(object)),
      if (length(object) > 1) "contains NA values" else "is NA"
    )
  }

  object
}

ensure_not_null <- function(object) {
  object %||% stop(sprintf("'%s' is NULL", deparse(substitute(object))))
}

ensure_scalar <- function(object) {

  if (length(object) != 1 || !is.numeric(object)) {
    stopf(
      "'%s' is not a length-one numeric value",
      deparse(substitute(object))
    )
  }

  object
}

#' Enforce Specific Structure for R Objects
#'
#' These routines are useful when preparing to pass objects to
#' TensorFlow routines, as it is often necessary to ensure certain
#' parameters are scalar integers, or scalar doubles, and so on.
#'
#' @param object An \R object.
#' @param allow.na Are \code{NA} values permitted for this object?
#' @param allow.null Are \code{NULL} values permitted for this object?
#' @param default If \code{object} is \code{NULL}, what value should
#'   be used in its place? If \code{default} is specified, \code{allow.null}
#'   is ignored (and assumed to be \code{TRUE}).
#'
#' @name ensure
#' @rdname ensure
NULL

make_ensure_scalar_impl <- function(checker,
                                    message,
                                    converter)
{
  fn <- function(object,
                 allow.na = FALSE,
                 allow.null = FALSE,
                 default = NULL)
  {
    object <- object %||% default

    if (!checker(object))
      stopf("'%s' is not %s", deparse(substitute(object)), message)

    if (is.na(object)) object <- NA_integer_
    if (!allow.na)     ensure_not_na(object)
    if (!allow.null)   ensure_not_null(object)

    converter(object)
  }

  environment(fn) <- parent.frame()

  body(fn) <- do.call(
    substitute,
    list(
      body(fn),
      list(
        checker = substitute(checker),
        message = substitute(message),
        converter = substitute(converter)
      )
    )
  )

  fn
}

#' @rdname ensure
#' @name ensure
#' @export
ensure_scalar_integer <- make_ensure_scalar_impl(
  is.numeric,
  "a length-one integer vector",
  as.integer
)

#' @rdname ensure
#' @name ensure
#' @export
ensure_scalar_double <- make_ensure_scalar_impl(
  is.numeric,
  "a length-one numeric vector",
  as.double
)

#' @rdname ensure
#' @name ensure
#' @export
ensure_scalar_boolean <- make_ensure_scalar_impl(
  is.logical,
  "a length-one logical vector",
  as.logical
)

#' @rdname ensure
#' @name ensure
#' @export
ensure_scalar_character <- make_ensure_scalar_impl(
  is.character,
  "a length-one character vector",
  as.character
)
