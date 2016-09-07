
#' @export
PyObject <- R6Class("PyObject",
  public = list(
    initialize = function(instance) {
      private$instance
    },
    print = function(...) {
      py_print(private$instance)
    },
    .Attrib = function(name) {
      attr <- py_get_attr(private$instance, name)
      if (py_is_callable(attr)) {
        function(...) {
          args <- list()
          keywords <- list()
          dots <- list(...)
          names <- names(dots)
          if (!is.null(names)) {
            for (i in 1:length(dots)) {
              name <- names[[i]]
              if (nzchar(name))
                keywords[[name]] <- dots[[i]]
              else
                args[[length(args) + 1]] <- dots[[i]]
            }
          } else {
            args <- dots
          }
          result = py_call(attr, args, keywords)
          if (is.null(result))
            invisible(result)
          else
            result
        }
      } else {
        py_to_r(attr)
      }
    },
    .DollarNames = function(pattern = "") {
      # get the names and filter out internal attributes (_*)
      names <- py_list_attributes(private$instance)
      names <- names[substr(names, 1, 1) != '_']

      # get the types
      attr(names, "types") <- py_get_attribute_types(private$instance, names)

      # get the doc strings
      inspect <- py_import("inspect")
      attr(names, "docs") <- sapply(names, function(name) {
        inspect$getdoc(py_get_attr(private$instance, name))
      })

      # return
      names
    }
  ),
  private = list(
    instance = NULL
  )
)

#' @export
`$.PyObject` <- function(x, name) {

  # if the name exists then return it
  if (name %in% ls(x, all.names = TRUE, sorted = FALSE))
    .subset2(x, name)

  # otherwise dynamically dispatch to the object instance
  else
    x$.Attrib(name)
}

#' @export
`[[.PyObject` <- `$.PyObject`

#' @export
.DollarNames.PyObject <- function(x, pattern = "") {
  x$.DollarNames(pattern)
}

wrap_py_object <- function(object) {
  PyObject$new(object)
}

# find the name of the python shared library
pythonSharedLibrary <- function() {

  # verify that we have python
  if (!nzchar(Sys.which("python")))
    stop("python not found!")

  # determine version of python
  pythonVersion <- system(intern = TRUE, paste0(
    "python -c 'import sys; print(str(sys.version_info.major) +  \".\" + ",
    " str(sys.version_info.minor))'")
  )

  # add ext and return
  ext <- ifelse(Sys.info()[["sysname"]] == "Darwin", ".dylib", ".so")
  paste0("libpython", pythonVersion, ext)
}
