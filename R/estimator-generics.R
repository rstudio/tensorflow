

#' Train a Model
#'
#' Train a model object. See implementation in the
#' [tfestimators][tfestimators::train.tf_estimator()] package.
#'
#' @param object A trainable \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @section Implementations:
#'
#'   - [tfestimators][tfestimators::train.tf_estimator()]
#'
#' @export
train <- function(object, ...) {
  UseMethod("train")
}

#' Evaluate a Model
#'
#' Evaluate a model object. See implementations in the
#' [keras][keras::evaluate.keras.engine.training.Model()] and
#' [tfestimators][tfestimators::evaluate.tf_estimator()] packages.
#'
#' @param object An evaluatable \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @section Implementations:
#'
#'   - [keras][keras::evaluate.keras.engine.training.Model()]
#'   - [tfestimators][tfestimators::evaluate.tf_estimator()]
#'
#' @export
evaluate <- function(object, ...) {
  UseMethod("evaluate")
}

#' Simultaneously Train and Evaluate a Model
#'
#' Train and evaluate a model object. See implementation in the
#' [tfestimators][tfestimators::train_and_evaluate.tf_estimator()] package.
#'
#' @param object An \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @section Implementations:
#'
#'   - [tfestimators][tfestimators::train_and_evaluate.tf_estimator()]
#'
#'
#' @export
train_and_evaluate <- function(object, ...) {
  UseMethod("train_and_evaluate")
}


#' Export a Saved Model
#'
#' Serialize a model to disk. See implementations in the
#' [keras][keras::export_savedmodel.keras.engine.training.Model()] and
#' [tfestimators][tfestimators::export_savedmodel.tf_estimator()] packages.
#'
#' @param object An \R object.
#' @param export_dir_base A string containing a directory in which to export the
#'   SavedModel.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @return The path to the exported directory, as a string.
#'
#' @section Implementations:
#'
#'   - [keras][keras::export_savedmodel.keras.engine.training.Model()]
#'   - [tfestimators][tfestimators::export_savedmodel.tf_estimator()]
#'
#'
#' @export
export_savedmodel <- function(
  object,
  export_dir_base,
  ...) {
  UseMethod("export_savedmodel")
}

