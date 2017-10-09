#' Export a Saved Model
#'
#' Serialize a model to disk.
#'
#' @param object An \R object.
#' @param export_dir_base A string containing a directory in which to create
#'   versioned subdirectories containing exported SavedModels.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @return The path to the exported directory, as a string.
#'
#' @export
export_savedmodel <- function(object, export_dir_base, ...) {
  UseMethod("export_savedmodel")
}
