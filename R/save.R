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
