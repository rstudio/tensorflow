

#' Train a Model
#'
#' Train a model object.
#'
#' @param object A trainable \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @export
train <- function(object, ...) {
  UseMethod("train")
}

#' Evaluate a Model
#'
#' Evaluate a model object.
#'
#' @param object An evaluatable \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @export
evaluate <- function(object, ...) {
  UseMethod("evaluate")
}

#' Simultaneously Train and Evaluate a Model
#'
#' Train and evaluate a model object.
#'
#' @param object An \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @export
train_and_evaluate <- function(object, ...) {
  UseMethod("train_and_evaluate")
}


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
