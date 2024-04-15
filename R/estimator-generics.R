

#' (Deprecated) Train a Model
#'
#' Train a model object. See implementation in the
#' `tfestimators::train.tf_estimator()` package.
#'
#' @param object A trainable \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' `r lifecycle::badge('deprecated')`
#'
#' @export
#' @keywords internal
train <- function(object, ...) {
  lifecycle::deprecate_warn("2.9", "train()", "fit()")
  UseMethod("train")
}

#' Evaluate a Model
#'
#' Evaluate a model object. See implementations in the
#' [keras3][keras3::evaluate.keras.src.models.model.Model()] package.
#'
#' @param object An evaluatable \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#' @section Implementations:
#'
#'   - [keras3][keras3::evaluate.keras.src.models.model.Model()]
#'
#' @export
evaluate <- function(object, ...) {
  UseMethod("evaluate")
}

#' (Deprecated) Simultaneously Train and Evaluate a Model
#'
#' Train and evaluate a model object. See implementation in the
#' `tfestimators::train_and_evaluate.tf_estimator()` package.
#'
#' @param object An \R object.
#' @param ... Optional arguments passed on to implementing methods.
#'
#'`r lifecycle::badge('deprecated')`
#'
#' @keywords internal
#' @export
train_and_evaluate <- function(object, ...) {
  lifecycle::deprecate_warn("2.9", "train_and_evaluate()")
  UseMethod("train_and_evaluate")
}


#' Export a Saved Model
#'
#' Serialize a model to disk. See implementations in the
#' [keras3][keras3::export_savedmodel.keras.src.models.model.Model()]
#' package.
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
#'   - [keras3][keras3::export_savedmodel.keras.src.models.model.Model]
#'
#' @keywords internal
#' @export
export_savedmodel <- function(
  object,
  export_dir_base,
  ...) {
  UseMethod("export_savedmodel")
}

