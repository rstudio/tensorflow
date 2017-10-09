#' Save TensorFlow Model
#'
#' Saves a TensorFlow model with the goal of deploying and reusing across systems.
#'
#' @param sess The TensorFlow session.
#' @param path The destination path where the model will be saved.
#'
#' @family save
#' @export
export_savedmodel <- function(sess, path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  next_version <- 1
  while (dir.exists(file.path(path, next_version))) next_version <- next_version + 1

  versioned_path <- file.path(path, next_version)

  builder <- tf$saved_model$builder$SavedModelBuilder(versioned_path)
  builder$save()

  variables_path <- file.path(versioned_path, "variables")

  if (!dir.exists(variables_path)) dir.create(variables_path, recursive = TRUE)
  variables_files <- file.path(variables_path, "variables")

  saver <- tf$train$Saver()
  saver$save(sess, variables_files)
}
