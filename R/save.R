#' View a Saved Model
#'
#' View a serialized model from disk.
#'
#' @param model_dir The path to the exported model, as a string.
#'
#' @return URL for browsing TensorBoard (invisibly).
#'
#' @export
view_savedmodel <- function(
  model_dir
) {
  log_dir <- tempfile()
  export_files <- dir(model_dir, full.names = TRUE, recursive = TRUE)
  export_pb <- export_files[grepl("\\.pb$", export_files)]
  if (length(export_pb) != 1) stop("Failed to find 'pb' file under ", model_dir)

  gfile <- tf$python$platform$gfile
  compat <- tf$python$util$compat
  saved_model_pb2 <- tf$core$protobuf$saved_model_pb2

  # open session and file (close both on exit)

  if (tf_version() >= "1.14")
    sess <- tf$compat$v1$Session()
  else
    sess <- tf$Session()

  f <- gfile$FastGFile(export_pb, "rb")
  on.exit({
    f$close()
    sess$close()
  }, add = TRUE)

  # write graph

  graph_def <- tf$GraphDef()

  data <- compat$as_bytes(f$read())
  sm <- saved_model_pb2$SavedModel()
  sm$ParseFromString(data)

  if (sm$meta_graphs$`__len__`() > 1) stop("Saved model contains more than one graph")

  g_in <- tf$import_graph_def(sm$meta_graphs[[0]]$graph_def)


  train_writer <- tf$summary$FileWriter(log_dir)
  train_writer$add_graph(sess$graph)
  train_writer$close()

  tensorboard(log_dir = log_dir)
}

#' @export
export_savedmodel.tensorflow.python.client.session.Session <- function(
  object,
  export_dir_base,
  inputs,
  outputs,
  overwrite = TRUE,
  versioned = !overwrite,
  as_text = FALSE,
  ...) {

  if (versioned) {
    export_dir_base <- file.path(export_dir_base, format(Sys.time(), "%Y%m%d%H%M%OS", tz = "GMT"))
  }

  sess <- object
  if (!overwrite && !versioned && dir.exists(export_dir_base))
    stop("Directory ", export_dir_base, " already exists.")

  tensor_inputs_info <- lapply(inputs, function(i) tf$saved_model$utils$build_tensor_info(i))
  tensor_outputs_info <- lapply(outputs, function(o) tf$saved_model$utils$build_tensor_info(o))

  build_signature_def <- if (tf_version() >= "1.14") {
    tf$compat$v1$saved_model$signature_def_utils$build_signature_def
  } else {
    tf$saved_model$signature_def_utils$build_signature_def
  }

  signature_constants <- if (tf_version() >= "1.14") {
    tf$saved_model
  } else {
    tf$saved_model$signature_constants
  }

  prediction_signature <- build_signature_def(
    inputs = tensor_inputs_info,
    outputs = tensor_outputs_info,
    method_name = signature_constants$PREDICT_METHOD_NAME)

  signature_def_map_class_dig <- signature_constants$DEFAULT_SERVING_SIGNATURE_DEF_KEY
  signature <- list()
  signature[[signature_def_map_class_dig]] <- prediction_signature

  if (overwrite && dir.exists(export_dir_base))
    unlink(export_dir_base, recursive = TRUE)


  if (tf_version() >= "1.14")
    builder <- tf$compat$v1$saved_model$builder$SavedModelBuilder(export_dir_base)
  else
    builder <- tf$saved_model$builder$SavedModelBuilder(export_dir_base)

  builder$add_meta_graph_and_variables(
    sess,
    list(
      tf$python$saved_model$tag_constants$SERVING
    ),
    signature_def_map = signature
  )

  invisible(builder$save(as_text = as_text))
}

