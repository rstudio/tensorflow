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
  sess, export_dir_base, ...) {

  args <- list(...)

  if (is.null(args$inputs) || !is.list(args$inputs))
    stop("Using export_savedmodel() requires 'inputs' paramaeter as a list of named input tensors.")

  if (is.null(args$outputs) || !is.list(args$outputs))
    stop("Using export_savedmodel() requires 'outputs' parameter as a list of named outputs tensors.")

  if (dir.exists(export_dir_base))
    stop("Directory ", export_dir_base, " already exists.")

  tensor_inputs_info <- lapply(args$inputs, function(i) tf$saved_model$utils$build_tensor_info(i))
  tensor_outputs_info <- lapply(args$outputs, function(o) tf$saved_model$utils$build_tensor_info(o))

  prediction_signature <- tf$saved_model$signature_def_utils$build_signature_def(
    inputs = tensor_inputs_info,
    outputs = tensor_outputs_info,
    method_name = tf$saved_model$signature_constants$PREDICT_METHOD_NAME)

  signature_def_map_class_dig <- tf$saved_model$signature_constants$DEFAULT_SERVING_SIGNATURE_DEF_KEY
  signature <- list(
    signature_def_map_class_dig = prediction_signature
  )

  builder <- tf$saved_model$builder$SavedModelBuilder(export_dir_base)

  builder$add_meta_graph_and_variables(
    sess,
    list(
      tf$python$saved_model$tag_constants$SERVING
    ),
    signature_def_map = signature
  )

  builder$save()
}
