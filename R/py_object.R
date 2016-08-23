

#' @import R6
#' @export
PyObject <- R6Class("PyObject",
  lock_objects = FALSE,
  public = list(
    initialize = function(instance) {
      private$instance <<- instance
    }
  ),
  private = list(
    instance = NULL
  )
)

# Call functions on the PyObject via $
#' @export
`$.PyObject` <- function(x, name) {

  # if the name exists then return it
  if (name %in% ls(x, all.names = TRUE, sorted = FALSE)) {
    .subset2(x, name)

  # otherwise dispatch to the object instance
  } else {

    # get the instance
    instance <- x$.__enclos_env__$private$instance

    # check for callable and return a function in that case
    attr <- py_object_get_attr(instance, name)
    if (py_object_is_callable(attr)) {
      function(...) {
        py_object_call(attr)
      }
    } else {
      attr
    }
  }
}

# Alias to [[
#' @export
`[[.PyObject` <- `$.PyObject`




#' @export
`$.py_object` <- function(x, name) {
  attr <- py_object_get_attr(x, name)
  if (py_object_is_callable(attr)) {
    function(...) {
      py_object_call(attr)
    }
  } else {
    attr
  }
}

